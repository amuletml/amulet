{-# LANGUAGE OverloadedStrings, DeriveGeneric, DuplicateRecordFields, FlexibleContexts #-}

{-| The main LSP server loop. -}
module AmuletLsp.Loop (run) where

import qualified AmuletLsp.Features.TypeOverlay as TO
import AmuletLsp.Diagnostic
import AmuletLsp.Features
import AmuletLsp.Worker

import Control.Lens hiding (List)
import Control.Concurrent.STM
import Control.Applicative
import Control.Monad

import qualified Data.Text as T
import Data.Aeson.Types
import Data.Bifunctor
import Data.Foldable
import Data.Default
import Data.Maybe

import Frontend.Errors

import Generics.Constructor
import GHC.Generics

import qualified Language.Haskell.LSP.Control as Control
import qualified Language.Haskell.LSP.Types.Lens as L
import Language.Haskell.LSP.Types.Lens hiding (error)
import qualified Language.Haskell.LSP.VFS as VFS
import qualified Language.Haskell.LSP.Core as C
import Language.Haskell.LSP.Messages
import Language.Haskell.LSP.Types

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
  e <- Control.run (callbacks qIn) (handlers qIn) options Nothing
  pure (if e == 0 then ExitSuccess else ExitFailure e)

  where
    callbacks :: TQueue FromClientMessage -> C.InitializeCallbacks Config
    callbacks qIn = C.InitializeCallbacks
        { C.onInitialConfiguration
            = maybe (pure def) (first T.pack . parseEither parseJSON)
            . (^. params . initializationOptions)
        , C.onConfigurationChange
            = first T.pack . parseEither parseJSON . (^. params . settings)
        , C.onStartup = \lf -> do
            config <- C.config lf
            wrk <- makeWorker (maybe [] libraryPath config) CT.lua (publishDiagnostics lf)
            _ <- forkIOWith "Loop" (loop lf wrk qIn)
            return Nothing
        }

    publishDiagnostics :: C.LspFuncs a -> NormalizedUri -> ErrorBundle -> IO ()
    publishDiagnostics lf uri es =
      C.sendFunc lf . NotPublishDiagnostics
      . fmServerPublishDiagnosticsNotification
      $ PublishDiagnosticsParams
      { _uri = fromNormalizedUri uri
      , _diagnostics = List $ map diagnosticOf (es ^. parseErrors)
                           ++ map diagnosticOf (es ^. resolveErrors)
                           ++ map diagnosticOf (es ^. typeErrors)
                           ++ map diagnosticOf (es ^. verifyErrors)
      }


    loop :: C.LspFuncs Config -> Worker -> TQueue FromClientMessage -> IO ()
    loop lf wrk qIn = forever $ atomically (readTQueue qIn) >>= handleRequest lf wrk

    handlers :: TQueue FromClientMessage -> C.Handlers
    handlers qIn = def
      { C.initializedHandler = handle NotInitialized
      , C.responseHandler    = handle RspFromClient

      -- Notifications alert us when something has happened:
      , C.didChangeConfigurationParamsHandler      = handle NotDidChangeConfiguration
      -- File changes
      , C.didOpenTextDocumentNotificationHandler   = handle NotDidOpenTextDocument
      , C.didSaveTextDocumentNotificationHandler   = handle NotDidSaveTextDocument
      , C.didChangeWatchedFilesNotificationHandler = handle NotDidChangeWatchedFiles
      , C.didChangeTextDocumentNotificationHandler = handle NotDidChangeTextDocument
      , C.didCloseTextDocumentNotificationHandler  = handle NotDidCloseTextDocument

      -- Requests query information from the server.
      , C.cancelNotificationHandler = handle NotCancelRequestFromClient
      , C.documentSymbolHandler     = handle ReqDocumentSymbols
      , C.codeActionHandler         = handle ReqCodeAction
      , C.codeLensHandler           = handle ReqCodeLens
      , C.codeLensResolveHandler    = handle ReqCodeLensResolve
      -- , C.foldingRangeHandler   = handle ReqFoldingRange
      }
      where handle c = Just (atomically . writeTQueue qIn . c)

    options :: C.Options
    options =
      def { C.textDocumentSync       = Just TextDocumentSyncOptions
                                       { _openClose         = Just True
                                       , _change            = Just TdSyncIncremental
                                       , _willSave          = Just False
                                       , _willSaveWaitUntil = Just False
                                       , _save              = Just SaveOptions { _includeText = Just False }
                                       }
          , C.codeActionKinds        = Just [CodeActionQuickFix]
          }

handleRequest :: C.LspFuncs Config -> Worker -> FromClientMessage -> IO ()

handleRequest _ _ (RspFromClient msg) =
  infoM logN ("Received message from client (" ++ show msg ++ ")")
handleRequest _ _ NotInitialized{} =
  infoM logN "Initialized server"
handleRequest lf wrk NotDidChangeConfiguration{} = do
  config <- C.config lf
  infoM logN ("Updated config with " ++ show config)
  updateConfig wrk (maybe [] libraryPath config)


{- * File changes:

  When we receive a file notification, we immediately forward the changes to the
  worker. Afterwards, we potentially inform the worker that they should attempt
  to update all loaded files.

  Note the potentially: not all actions /require/ a refresh. For instance,
  closing a file shouldn't impact the compilation state at all.
-}
handleRequest lf wrk (NotDidOpenTextDocument msg) = do
  let nUri = toNormalizedUri (msg ^. params . textDocument . uri)

  file <- C.getVirtualFileFunc lf nUri
  case file of
    Nothing -> pure ()
    Just file -> do
      updateFile wrk nUri (Version (VFS._lsp_version file)) (VFS._text file)
      refresh wrk (Just nUri)

handleRequest _ wrk (NotDidSaveTextDocument msg) =
  let nUri = toNormalizedUri (msg ^. params . textDocument . uri)
  in refresh wrk (Just nUri)

handleRequest lf wrk (NotDidChangeTextDocument msg) = do
  let nUri = toNormalizedUri (msg ^. params . textDocument . uri)
  file <- C.getVirtualFileFunc lf nUri
  case file of
    Nothing -> pure ()
    Just file -> do
      updateFile wrk nUri (Version (VFS._lsp_version file)) (VFS._text file)
      refresh wrk (Just nUri)

handleRequest _ wrk (NotDidCloseTextDocument msg) = do
  let nUri = toNormalizedUri (msg ^. params . textDocument . uri)
  closeFile wrk nUri

handleRequest _ wrk (NotDidChangeWatchedFiles msg) = do
  for_ (msg ^. params . changes) (touchFile wrk . toNormalizedUri . (^.uri))
  refresh wrk Nothing


{- * Requests:

  Requests are conceptually quite simple: we submit a function to the worker,
  which will wait for any dependencies and evaluate when they are fulfilled.
-}

handleRequest _ wrk (NotCancelRequestFromClient msg) =
  cancelRequest wrk (msg ^. params . L.id)

handleRequest lf wrk (ReqDocumentSymbols msg)
  = startRequest wrk (msg ^. id)
  . RequestLatest (toNormalizedUri rawUri) ReqParsed (sendReplyError lf msg)
  $ \_ _ prog ->
    sendReply lf msg RspDocumentSymbols . DSDocumentSymbols . List . maybe [] getOutline $ prog
  where rawUri = msg ^. params . textDocument . uri

handleRequest lf wrk (ReqCodeAction msg)
  = startRequest wrk (msg ^. id)
  . RequestLatest (toNormalizedUri rawUri) ReqErrors (sendReplyError lf msg)
  $ \_ (Version ver) es ->
      let versioned = VersionedTextDocumentIdentifier rawUri (Just ver)
      in sendReply lf msg RspCodeAction . List $ getCodeActions versioned (msg ^. params . range) es
  where rawUri = msg ^. params . textDocument . uri

handleRequest lf wrk (ReqCodeLens msg) = do
  typeOverlay <- typeOverlay . fromMaybe def <$> C.config lf
  if typeOverlay
  then startRequest wrk (msg ^. id)
     . RequestLatest (toNormalizedUri rawUri) ReqTyped (sendReplyError lf msg)
     $ \name _ -> sendReply lf msg RspCodeLens . List . maybe [] (TO.getTypeOverlay name . (\(_, p, _, _) -> p))
  else sendReply lf msg RspCodeLens (List [])
  where rawUri = msg ^. params . textDocument . uri

handleRequest lf wrk (ReqCodeLensResolve msg) =
  case msg ^. params of
    -- We're already resolved? This should never happen, but just send it
    -- back anyway.
    c@(CodeLens _ Just{} _) -> sendReply lf msg RspCodeLensResolve c

    c@(CodeLens _ _ (Just extra))
      | Success od@(TO.OverlayData _ _ fileVar) <- fromJSON extra -> do
          -- Find which file this type overlay refers to, and then fire
          -- off a fresh request to fetch the environment and resolve the
          -- code lens.
          uri <- findFile wrk (TgName "" fileVar)
          case uri of
            Nothing -> do
              infoM logN ("Skiping outdated code lens for " ++ show uri)
              sendReplyError lf msg $ ResponseError ContentModified "File is no longer available" Nothing
            Just path
              -> startRequest wrk (msg ^. id)
               . RequestLatest path ReqTyped (sendReplyError lf msg)
               $ \_ _ r ->
                   case r of
                     Nothing ->
                       sendReplyError lf msg $ ResponseError ContentModified "File cannot be loaded" Nothing
                     Just (_, _, env, _)
                       | Just lens <- TO.resolveTypeOverlay env od c
                       -> sendReply lf msg RspCodeLensResolve lens
                       | otherwise -> do
                           warningM logN ("Skiping out-of-date type overlay for " ++ show c)
                           sendReplyError lf msg $ ResponseError ContentModified "Variable is no longer available" Nothing

    c -> do
      errorM logN ("Skiping malformed code lens for " ++ show c)
      sendReplyError lf msg $ ResponseError InvalidParams "Code lens is missing data" Nothing

-- handleRequest lf (ReqFoldingRange msg) = undefined

handleRequest _ _ msg =
  warningM logN ("Unknown message " ++ conNameOf msg)

-- | Send a reply to a request.
sendReply :: C.LspFuncs c -> RequestMessage ClientMethod req resp -> (ResponseMessage resp -> FromServerMessage) -> resp -> IO ()
sendReply lf req wrapMsg = C.sendFunc lf . wrapMsg . C.makeResponseMessage req

-- | Send a an error in reply to a request.
sendReplyError :: C.LspFuncs c -> RequestMessage ClientMethod req resp -> ResponseError -> IO ()
sendReplyError lf req = C.sendFunc lf . RspError . C.makeResponseError (responseId $ req ^. id)

-- | The name of the logger to use.
logN :: String
logN = "AmuletLsp.Loop"
