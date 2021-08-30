{-# LANGUAGE OverloadedStrings, DeriveGeneric, DuplicateRecordFields, FlexibleContexts, ScopedTypeVariables, DataKinds, TypeFamilies, PolyKinds #-}

{-| The main LSP server loop. -}
module AmuletLsp.Loop (run) where

import AmuletLsp.Features.TypeOverlay
import AmuletLsp.Features.Outline
import AmuletLsp.Features.Folding
import AmuletLsp.Diagnostic
import AmuletLsp.Features
import AmuletLsp.Worker

import Control.Lens hiding (List)
import Control.Concurrent.STM
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class

import qualified Data.Text as T
import Data.Aeson.Types
import Data.Bifunctor
import Data.Foldable
import Data.Default
import Data.Maybe

import Frontend.Errors

import GHC.Generics

import Language.LSP.Types.Lens hiding (didChangeWatchedFiles)
import qualified Language.LSP.VFS as VFS
import qualified Language.LSP.Server as S
import Language.LSP.Types

import Prelude hiding (id)

import Syntax.Var

import qualified CompileTarget as CT

import System.Log.Logger
import System.Exit

data Config = Config
  { libraryPath :: [FilePath]
  , typeOverlay :: Bool
  }
  deriving (Show, Generic)

instance Default Config where
  def = Config [] True

instance FromJSON Config where
  parseJSON = withObject "Config" $ \v -> do
    s <- v .: "amulet"
    flip (withObject "Config.settings") s $ \o -> Config
      <$> o .:? "libraryPath"                 .!= libraryPath def
      <*> o .:? "typeOverlay"                 .!= typeOverlay def


type Lsp = S.LspM Config

-- | Set up and run the server.
--
-- The main server is pretty simple: we forward all messages into a
-- separate 'TQueue', pull them out on a separate thread, and pass them
-- to 'handleRequest'.
--
-- We try to run requests as quickly as possible, thus everything is done
-- asynchronously on the worker.
run :: IO ExitCode
run = do
  qIn <- atomically newTQueue
  e <- S.runServer (server qIn)
  -- (callbacks qIn) (handlers qIn) options Nothing
  pure (if e == 0 then ExitSuccess else ExitFailure e)

  where
    server :: TQueue (Worker -> Lsp ()) -> S.ServerDefinition Config
    server qIn = S.ServerDefinition
        { options = options
        , defaultConfig = def
        , S.doInitialize = \env _ -> do
            config <- S.runLspT env S.getConfig
            wrk <- makeWorker (libraryPath config) CT.lua (publishDiagnostics env)
            _ <- forkIOWith "Loop" (loop env wrk qIn)
            return (Right env)
        , onConfigurationChange = \_ -> first T.pack . parseEither parseJSON
        , staticHandlers = handlers qIn
        , interpretHandler = \env -> S.Iso (S.runLspT env) liftIO
        }

    publishDiagnostics :: S.LanguageContextEnv Config -> NormalizedUri -> ErrorBundle -> IO ()
    publishDiagnostics env uri es =
      S.runLspT env $ S.sendNotification STextDocumentPublishDiagnostics PublishDiagnosticsParams
      { _uri = fromNormalizedUri uri
      , _diagnostics = List $ map diagnosticOf (es ^. parseErrors)
                           ++ map diagnosticOf (es ^. resolveErrors)
                           ++ map diagnosticOf (es ^. typeErrors)
                           ++ map diagnosticOf (es ^. verifyErrors)
      , _version = Nothing -- TODO: Specify a version
      }

    loop :: S.LanguageContextEnv Config -> Worker -> TQueue (Worker -> Lsp ()) -> IO ()
    loop env wrk qIn = forever go where
      go :: IO ()
      go = atomically (readTQueue qIn) >>= S.runLspT env . ($ wrk)

    handlers :: TQueue (Worker -> Lsp ()) -> S.Handlers (S.LspM Config)
    handlers qIn = mconcat
      [ S.notificationHandler SInitialized $ \notif ->
          liftIO $ infoM logN ("Received message from client (" ++ show notif ++ ")")
      , S.notificationHandler SWorkspaceDidChangeConfiguration $ queuedN $ \wrk _ -> do
          config <- S.getConfig
          liftIO $ infoM logN ("Updated config with " ++ show config)
          liftIO $ updateConfig wrk (libraryPath config)
      , S.notificationHandler SCancelRequest $ queuedN doCancelRequest

      -- File changes
      , S.notificationHandler STextDocumentDidOpen   $ queuedN didOpenTextDocument
      , S.notificationHandler STextDocumentDidSave   $ queuedN didSaveTextDocument
      , S.notificationHandler STextDocumentDidChange $ queuedN didChangeTextDocument
      , S.notificationHandler STextDocumentDidClose  $ queuedN didCloseTextDocument
      , S.notificationHandler SWorkspaceDidChangeWatchedFiles $ queuedN didChangeWatchedFiles

      -- Requests query information from the server.
      , S.requestHandler STextDocumentDocumentSymbol $ queuedR handleDocumentSymbols
      , S.requestHandler STextDocumentCodeAction     $ queuedR handleCodeAction
      , S.requestHandler STextDocumentCodeLens       $ queuedR handleCodeLens
      , S.requestHandler SCodeLensResolve            $ queuedR handleCodeLensResolve
      , S.requestHandler STextDocumentFoldingRange   $ queuedR handleFoldingRange
      ]
      where
        queuedN :: forall (m :: Method 'FromClient 'Notification). (Worker -> S.Handler Lsp m)
                -> S.Handler Lsp m
        queuedN f = liftIO . atomically . writeTQueue qIn . flip f
        queuedR :: forall (m :: Method 'FromClient 'Request). (Worker -> S.Handler Lsp m)
                -> S.Handler Lsp m
        queuedR f x cb = liftIO . atomically . writeTQueue qIn $ \wrk -> f wrk x cb

    options :: S.Options
    options =
      def { S.textDocumentSync       = Just TextDocumentSyncOptions
                                       { _openClose         = Just True
                                       , _change            = Just TdSyncIncremental
                                       , _willSave          = Just False
                                       , _willSaveWaitUntil = Just False
                                       , _save              = Just $ InR SaveOptions { _includeText = Just False }
                                       }
          , S.codeActionKinds        = Just [CodeActionQuickFix]
          }

{- * File changes:

  When we receive a file notification, we immediately forward the changes to the
  worker. Afterwards, we potentially inform the worker that they should attempt
  to update all loaded files.

  Note the potentially: not all actions /require/ a refresh. For instance,
  closing a file shouldn't impact the compilation state at all.
-}

didOpenTextDocument :: Worker -> S.Handler Lsp 'TextDocumentDidOpen
didOpenTextDocument wrk msg = do
  let nUri = toNormalizedUri (msg ^. params . textDocument . uri)

  file <- S.getVirtualFile nUri
  case file of
    Nothing -> pure ()
    Just file -> liftIO $ do
      updateFile wrk nUri (Version (VFS._lsp_version file)) (VFS._text file)
      refresh wrk (Just nUri)

didSaveTextDocument :: Worker -> S.Handler Lsp 'TextDocumentDidSave
didSaveTextDocument wrk msg =
  let nUri = toNormalizedUri (msg ^. params . textDocument . uri)
  in liftIO $ refresh wrk (Just nUri)

didChangeTextDocument :: Worker -> S.Handler Lsp 'TextDocumentDidChange
didChangeTextDocument wrk msg = do
  let nUri = toNormalizedUri (msg ^. params . textDocument . uri)
  file <- S.getVirtualFile nUri
  case file of
    Nothing -> pure ()
    Just file -> liftIO $ do
      updateFile wrk nUri (Version (VFS._lsp_version file)) (VFS._text file)
      refresh wrk (Just nUri)

didCloseTextDocument :: Worker -> S.Handler Lsp 'TextDocumentDidClose
didCloseTextDocument wrk msg =
  let nUri = toNormalizedUri (msg ^. params . textDocument . uri)
  in liftIO $ closeFile wrk nUri

didChangeWatchedFiles :: Worker -> S.Handler Lsp 'WorkspaceDidChangeWatchedFiles
didChangeWatchedFiles wrk msg = liftIO $ do
  for_ (msg ^. params . changes) (touchFile wrk . toNormalizedUri . (^.uri))
  refresh wrk Nothing


{- * Requests:

  Requests are conceptually quite simple: we submit a function to the worker,
  which will wait for any dependencies and evaluate when they are fulfilled.
-}

doCancelRequest :: Worker -> S.Handler Lsp 'CancelRequest
doCancelRequest wrk NotificationMessage{_params = CancelParams {_id = req } } =
  liftIO $ cancelRequest wrk (SomeLspId req)

handleDocumentSymbols :: Worker -> S.Handler Lsp 'TextDocumentDocumentSymbol
handleDocumentSymbols wrk msg cb = do
  env <- S.getLspEnv
  liftIO . startRequest wrk (SomeLspId (msg ^. id))
    . RequestLatest (toNormalizedUri rawUri) ReqParsed (sendReplyError env cb)
    $ \_ _ prog -> sendReply env cb . InL . List . maybe [] getOutline $ prog
  where
    rawUri = msg ^. params . textDocument . uri

handleCodeAction :: Worker -> S.Handler Lsp 'TextDocumentCodeAction
handleCodeAction wrk msg cb = do
  env <- S.getLspEnv
  liftIO . startRequest wrk (SomeLspId (msg ^. id))
    . RequestLatest (toNormalizedUri rawUri) ReqErrors (sendReplyError env cb)
    $ \_ (Version ver) es ->
      let versioned = VersionedTextDocumentIdentifier rawUri (Just ver)
      in sendReply env cb . List . map InR $ getCodeActions versioned (msg ^. params . range) es
  where rawUri = msg ^. params . textDocument . uri

handleCodeLens :: Worker -> S.Handler Lsp 'TextDocumentCodeLens
handleCodeLens wrk msg cb = do
  env <- S.getLspEnv
  typeOverlay <- typeOverlay <$> S.getConfig
  if typeOverlay
  then liftIO . startRequest wrk (SomeLspId (msg ^. id))
     . RequestLatest (toNormalizedUri rawUri) ReqTyped (sendReplyError env cb)
     $ \name _ -> sendReply env cb . List . maybe [] (getTypeOverlay name . (\(_, p, _, _) -> p))
  else cb (Right (List []))
  where rawUri = msg ^. params . textDocument . uri

handleCodeLensResolve :: Worker -> S.Handler Lsp 'CodeLensResolve
handleCodeLensResolve wrk msg cb = do
  case msg ^. params of
    -- We're already resolved? This should never happen, but just send it
    -- back anyway.
    c@(CodeLens _ Just{} _) -> cb (Right c)

    c@(CodeLens _ _ (Just extra))
      | Success od@(OverlayData _ _ fileVar) <- fromJSON extra -> do
          -- Find which file this type overlay refers to, and then fire
          -- off a fresh request to fetch the environment and resolve the
          -- code lens.
          uri <- liftIO $ findFile wrk (TgName "" fileVar)
          case uri of
            Nothing -> do
              liftIO $ infoM logN ("Skiping outdated code lens for " ++ show uri)
              cb . Left $ ResponseError ContentModified "File is no longer available" Nothing
            Just path -> do
              lspEnv <- S.getLspEnv
              liftIO . startRequest wrk (SomeLspId (msg ^. id))
               . RequestLatest path ReqTyped (sendReplyError lspEnv cb)
               $ \_ _ r ->
                   case r of
                     Nothing ->
                       sendReplyError lspEnv cb $ ResponseError ContentModified "File cannot be loaded" Nothing
                     Just (_, _, env, _)
                       | Just lens <- resolveTypeOverlay env od c
                       -> sendReply lspEnv cb lens
                       | otherwise -> do
                           warningM logN ("Skiping out-of-date type overlay for " ++ show c)
                           sendReplyError lspEnv cb $ ResponseError ContentModified "Variable is no longer available" Nothing

    c -> do
      liftIO $ errorM logN ("Skiping malformed code lens for " ++ show c)
      cb . Left $ ResponseError InvalidParams "Code lens is missing data" Nothing

handleFoldingRange :: Worker -> S.Handler Lsp 'TextDocumentFoldingRange
handleFoldingRange wrk msg cb = do
  env <- S.getLspEnv
  liftIO . startRequest wrk (SomeLspId (msg ^. id))
    . RequestLatest (toNormalizedUri rawUri) ReqParsed (sendReplyError env cb)
    $ \_ _ prog -> sendReply env cb . List . maybe [] getFolds $ prog
  where rawUri = msg ^. params . textDocument . uri


-- | Send a reply to a request.
sendReply :: S.LanguageContextEnv Config -> (Either ResponseError a -> Lsp ()) -> a -> IO ()
sendReply env cb = S.runLspT env . cb . Right

-- | Send a an error in reply to a request.
sendReplyError :: S.LanguageContextEnv Config -> (Either ResponseError a -> Lsp ()) -> ResponseError -> IO ()
sendReplyError env cb = S.runLspT env . cb . Left

-- | The name of the logger to use.
logN :: String
logN = "AmuletLsp.Loop"
