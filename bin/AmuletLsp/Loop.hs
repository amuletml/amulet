{-# LANGUAGE OverloadedStrings, DeriveGeneric, DuplicateRecordFields, FlexibleContexts #-}
module AmuletLsp.Loop (run) where

import AmuletLsp.Features
import AmuletLsp.Worker

import Control.Lens hiding (List)
import Control.Concurrent.STM
import Control.Applicative
import Control.Concurrent
import Control.Monad

import qualified Data.Text as T
import Data.Aeson.Types
import Data.Bifunctor
import Data.Foldable
import Data.Default
import Data.Triple
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

import Text.Pretty.Semantic

import Prelude hiding (id)

import System.Log.Logger
import System.Exit

newtype Config = Config
  { libraryPath :: [FilePath]
  }
  deriving (Show, Generic)

instance FromJSON Config where
  parseJSON = withObject "Config" $ \v -> do
    s <- v .: "amulet"
    flip (withObject "Config.settings") s $ \o -> Config
      <$> o .:? "libraryPath"                 .!= []

run :: IO ExitCode
run = do
  qIn <- atomically newTQueue
  e <- Control.run (callbacks qIn) (handlers qIn) options Nothing
  pure (if e == 0 then ExitSuccess else ExitFailure e)

  where
    callbacks :: TQueue FromClientMessage -> C.InitializeCallbacks Config
    callbacks qIn = C.InitializeCallbacks
        { C.onInitialConfiguration
            = maybe (pure (Config [])) (first T.pack . parseEither parseJSON)
            . (^. params . initializationOptions)
        , C.onConfigurationChange
            = first T.pack . parseEither parseJSON . (^. params . settings)
        , C.onStartup = \lf -> do
            config <- C.config lf
            wrk <- makeWorker (maybe [] libraryPath config) (publishDiagnostics lf)
            _ <- forkIO (loop lf wrk qIn)
            return Nothing
        }

    publishDiagnostics :: C.LspFuncs a -> NormalizedUri -> ErrorBundle -> IO ()
    publishDiagnostics lf uri es =
      C.sendFunc lf . NotPublishDiagnostics
      . fmServerPublishDiagnosticsNotification
      $ PublishDiagnosticsParams
      { _uri = fromNormalizedUri uri
      , _diagnostics = List $ map (diagnosticOf (Just "amc.parser")  pretty)        (es ^. parseErrors)
                           ++ map (diagnosticOf (Just "amc.resolve") prettyResolve) (es ^. resolveErrors)
                           ++ map (diagnosticOf (Just "amc.tc")      pretty)        (es ^. typeErrors)
                           ++ map (diagnosticOf (Just "amc.verify")  pretty)        (es ^. verifyErrors)
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
      -- , C.foldingRangeHandler   = handle ReqFoldingRange
      }
      where handle c = Just (atomically . writeTQueue qIn . c)

    -- commandIds :: [T.Text]
    -- commandIds = []

    options :: C.Options
    options =
      def { C.textDocumentSync       = Just TextDocumentSyncOptions
                                       { _openClose         = Just True
                                       , _change            = Just TdSyncIncremental
                                       , _willSave          = Just False
                                       , _willSaveWaitUntil = Just False
                                       , _save              = Just SaveOptions { _includeText = Just False }
                                       }
          -- , C.completionProvider     = Nothing -- CompletionOptions
          --                              { _resolveProvider   = Just True
          --                              , _triggerCharacters = Just ["."]
          --                              }
          , C.codeActionProvider     = Just (CodeActionOptionsStatic True)
          , C.codeLensProvider       = Just (CodeLensOptions (Just False))
          -- , C.foldingRangeProvider   = Just (FoldingRangeOptionsStatic True)
          -- , C.executeCommandProvider = Just ExecuteCommandOptions
          --                              { _commands          = List commandIds
          --                              }
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
      updateFile wrk nUri (Version (VFS._version file)) (VFS._text file)
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
      updateFile wrk nUri (Version (VFS._version file)) (VFS._text file)
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
  $ \_ prog -> do
    debugM logN ("Outline of " ++ show prog)
    sendReply lf msg RspDocumentSymbols . DSDocumentSymbols . List . maybe [] getOutline $ prog
  where rawUri = msg ^. params . textDocument . uri

handleRequest lf wrk (ReqCodeAction msg)
  = startRequest wrk (msg ^. id)
  . RequestLatest (toNormalizedUri rawUri) ReqErrors (sendReplyError lf msg)
  $ \(Version ver) es ->
      let versioned = VersionedTextDocumentIdentifier rawUri (Just ver)
      in sendReply lf msg RspCodeAction . List $ getCodeActions versioned (msg ^. params . range) es
  where rawUri = msg ^. params . textDocument . uri

handleRequest lf wrk (ReqCodeLens msg)
  = startRequest wrk (msg ^. id)
  . RequestLatest (toNormalizedUri rawUri) ReqTyped (sendReplyError lf msg)
  $ \_ -> sendReply lf msg RspCodeLens . List . maybe [] (getCodeLenses . thd3)
  where rawUri = msg ^. params . textDocument . uri

-- handleRequest lf (ReqFoldingRange msg) = undefined

handleRequest _ _ msg =
  warningM logN ("Unknown message " ++ conNameOf msg)

-- | Send a reply to a request.
sendReply :: C.LspFuncs c -> RequestMessage m req resp -> (ResponseMessage resp -> FromServerMessage) -> resp -> IO ()
sendReply lf req wrapMsg msg =
  C.sendFunc lf . wrapMsg $
    ResponseMessage "2.0" (responseId $ req ^. id) (Just msg) Nothing

-- | Send a an error in reply to a request.
sendReplyError :: C.LspFuncs c -> RequestMessage m req resp -> ResponseError -> IO ()
sendReplyError lf req err =
  C.sendFunc lf . RspError $
    ResponseMessage "2.0" (responseId $ req ^. id) Nothing (Just err)

-- | The name of the logger to use.
logN :: String
logN = "AmuletLsp.Loop"
