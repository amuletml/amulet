{-# LANGUAGE OverloadedStrings, DeriveGeneric, DuplicateRecordFields, TypeOperators, TypeSynonymInstances, FlexibleInstances, FlexibleContexts, ScopedTypeVariables #-}
module Amc.Editor (run) where

import           Control.Concurrent
import           Control.Concurrent.STM.TChan
import           Control.Monad
import           Control.Lens hiding (List)
import           Control.Monad.STM
import qualified Data.Aeson as J
import           Data.Aeson.Types
import qualified Data.Attoparsec.ByteString as Atto
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import           Data.Bifunctor
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder.Extra as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import           Data.Default
import           Data.Position
import qualified Data.Rope.UTF16 as Rope
import           Data.Span
import           Data.Spanned
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Encoding as L
import           Frontend.Errors
import           GHC.Generics
import qualified GHC.Generics as G
import           Language.Haskell.LSP.Messages
import           Language.Haskell.LSP.Types
import           Language.Haskell.LSP.Types.Lens hiding (error)
import qualified Data.HashMap.Strict as HM
import           Parser (parseTops)
import           Parser.Wrapper (runParser)
import           Syntax (Toplevel)
import           Syntax.Builtin (builtinResolve, builtinEnv)
import           Syntax.Desugar (desugarProgram)
import           Syntax.Resolve (ResolveResult(..), resolveProgram)
import           Syntax.Resolve.Import
import           Syntax.Resolve.Scope (Signature)
import           Syntax.Types
import           Syntax.Var
import           Syntax.Verify
import           System.Exit
import           System.IO
import qualified System.Log.Logger as L
import           Text.Pretty.Note
import           Text.Pretty.Semantic

newtype Config = Config
  { libraryPath :: Maybe [FilePath]
  }
  deriving (Show, Generic)

instance FromJSON Config where
  parseJSON = genericParseJSON defaultOptions

data LoadedData a = LoadedData
  { currentVersion :: TextDocumentVersion
  , contents       :: a
  }
  deriving Show

data FileState
  -- | A file which has been opened within the editor.
  = OpenedFile
    { openVersion  :: TextDocumentVersion
    , openParsed   :: Maybe (LoadedData [Toplevel Parsed])
    , openResolved :: Maybe (LoadedData (Signature, [Toplevel Resolved]))
    , openTyped    :: Maybe (LoadedData (Signature, Env, [Toplevel Typed]))
    , errors       :: ErrorBundle
    }
  -- | A file which is currently saved on disk.
  | DiskFile
    { diskHash     :: BS.ByteString
    , diskDirty    :: Bool
    , diskResolved :: Maybe Signature
    , diskTyped    :: Maybe Env
    }
  deriving Show

logL :: BSL.ByteString -> IO ()
logL _ = pure ()

logS :: BS.ByteString -> IO ()
logS _ = pure ()

-- | Starts listening and sending requests and responses at the specified handles.
runWith :: Handle -> Handle -> (TChan FromClientMessage -> TChan FromServerMessage -> IO ()) -> IO ()
runWith hIn hOut setup = do
  hSetBuffering hIn NoBuffering
  hSetEncoding  hIn utf8

  hSetBuffering hOut NoBuffering
  hSetEncoding  hOut utf8

  (qIn :: TChan FromClientMessage, qOut :: TChan FromServerMessage)
    <- atomically $ (,) <$> newTChan <*> newTChan
  _ <- forkIO $ sendOutput qOut hOut

  setup qIn qOut
  readInput qIn hIn (Atto.parse parser "")

  where
    sendOptions = J.defaultOptions { J.sumEncoding = J.UntaggedValue }

    crlf, contentLength :: BS.ByteString
    crlf = "\r\n\r\n"
    contentLength = "Content-Length: "

    -- | Reads from the queue, and prints output to the log.
    sendOutput :: TChan FromServerMessage -> Handle -> IO ()
    sendOutput qOut hOut = forever $ do
      msg <- atomically $ readTChan qOut

      -- We need to make sure we only send over the content of the message,
      -- and no other tags/wrapper stuff
      let str = J.encode $ J.genericToJSON sendOptions msg
          out = BSL.concat
                   [ L.encodeUtf8 . L.pack $ "Content-Length: " ++ show (BSL.length str)
                   , BSL.fromStrict crlf
                   , str ]

      BSL.hPut hOut out
      hFlush hOut

      logL $ "<--- " <> str

    readInput :: TChan FromClientMessage -> Handle -> Atto.Result BS.ByteString -> IO ()
    readInput _ _ (Atto.Fail _ ctxs err) = logS ("Cannot parse input " <> BS8.pack err)
    readInput qIn hIn (Atto.Partial c) = do
      bs <- BS.hGetSome hIn BS.defaultChunkSize
      if BS.null bs then logS "Got EOF." else readInput qIn hIn (c bs)
    readInput qIn hIn (Atto.Done remainder msg) = do
      logS $ "---> " <> msg
      case J.eitherDecode jsonStr :: Either String J.Object of
        Left err -> error "TODO: Send error message"
        Right o ->
          case HM.lookup "method" o of
            Just cmd@(J.String s) ->
              case J.fromJSON cmd of
                J.Success m -> handle o m

      readInput qIn hIn (Atto.parse parser remainder)

    parser = do
      _ <- Atto.string contentLength
      len <- Atto.decimal
      _ <- Atto.string crlf
      Atto.take len


run :: IO ()
run = do
  pure ()
--   qIn  <- atomically newTChan :: IO (TChan FromClientMessage)
--   C.setupLogger (Just "/tmp/amc-lsp.log") [] L.DEBUG
--   e <- Control.run (callbacks qIn) (handlers qIn) options (Just "/tmp/amc-lsp-session.log")
--   exitWith (if e == 0 then ExitSuccess else ExitFailure e)

--   where
--     callbacks :: TChan FromClientMessage -> C.InitializeCallbacks Config
--     callbacks qIn = C.InitializeCallbacks
--         { C.onInitialConfiguration
--             = maybe (pure (Config Nothing)) (first T.pack . parseEither parseJSON)
--             . (^. params . initializationOptions)
--         , C.onConfigurationChange
--             = first T.pack . parseEither parseJSON . (^. params . settings)
--         , C.onStartup = \lf -> forkIO (loop lf qIn) >> return Nothing
--         }

--     loop :: C.LspFuncs Config -> TChan FromClientMessage -> IO ()
--     loop lf qIn = do
--       request <- atomically $ readTChan qIn
--       handleRequest lf request
--       loop lf qIn

--     handlers :: TChan FromClientMessage -> C.Handlers
--     handlers qIn = def
--       { C.initializedHandler  = handle NotInitialized
--       , C.responseHandler     = handle RspFromClient

--         -- Notifications alert us when something has changed.
--       , C.didOpenTextDocumentNotificationHandler   = handle NotDidOpenTextDocument
--       , C.didSaveTextDocumentNotificationHandler   = handle NotDidSaveTextDocument
--       , C.didChangeWatchedFilesNotificationHandler = handle NotDidChangeWatchedFiles
--       , C.didChangeTextDocumentNotificationHandler = handle NotDidChangeTextDocument
--       , C.didCloseTextDocumentNotificationHandler  = handle NotDidCloseTextDocument
--       , C.cancelNotificationHandler                = handle NotCancelRequestFromClient

--         -- Requests query information from the server.
--       , C.documentSymbolHandler = handle ReqDocumentSymbols
--       , C.codeActionHandler     = handle ReqCodeAction
--       , C.codeLensHandler       = handle ReqCodeLens
--       , C.foldingRangeHandler   = handle ReqFoldingRange

--       }
--       where handle c = Just (\noti -> atomically $ writeTChan qIn (c noti))

--     commandIds :: [T.Text]
--     commandIds = []

--     options :: C.Options
--     options =
--       def { C.textDocumentSync       = Just TextDocumentSyncOptions
--                                        { _openClose         = Just True
--                                        , _change            = Just TdSyncIncremental
--                                        , _willSave          = Just False
--                                        , _willSaveWaitUntil = Just False
--                                        , _save              = Just SaveOptions { _includeText = Just False }
--                                        }
--           , C.completionProvider     = Just CompletionOptions
--                                        { _resolveProvider   = Just True
--                                        , _triggerCharacters = Just ["."]
--                                        }
--           , C.codeActionProvider     = Just (CodeActionOptionsStatic True)
--           , C.foldingRangeProvider   = Just (FoldingRangeOptionsStatic True)
--           , C.executeCommandProvider = Just ExecuteCommandOptions
--                                        { _commands          = List commandIds
--                                        }
--           }

-- handleRequest :: C.LspFuncs Config -> FromClientMessage -> IO ()

-- handleRequest _ (RspFromClient msg) = logs ("Received message from client (" ++ show msg ++ ")")
-- handleRequest lf NotInitialized{} = do
--   sendMsg lf ReqRegisterCapability fmServerRegisterCapabilityRequest . RegistrationParams . List $
--     [ ]
--   logs "Initialized server."

-- -- -- Notifications alert us when something has changed.
-- -- handleRequest lf (NotDidOpenTextDocument msg) = do
-- -- handleRequest lf (NotDidSaveTextDocument msg) = undefined
-- -- handleRequest lf (NotDidChangeWatchedFiles msg) = undefined
-- handleRequest lf (NotDidChangeTextDocument msg) = do
--   let nUri@(NormalizedUri wholeUri) = toNormalizedUri $ msg ^. params . textDocument . uri
--   file <- C.getVirtualFileFunc lf nUri
--   case file of
--     Nothing -> logs ("Cannot find " ++ show nUri)
--     Just file -> do
--       logs "Opened file"
--       let contents = Rope.toLazyText (VFS._text file)
--           (parsed, es) = runParser (T.unpack wholeUri) contents parseTops

--       C.sendFunc lf . NotPublishDiagnostics
--         . fmServerPublishDiagnosticsNotification
--         $ PublishDiagnosticsParams
--         { _uri = msg ^. params . textDocument .uri
--         , _diagnostics = List (map (diagnosticOf (Just "amc.parser")) es)
--         }

--       pure ()


-- -- handleRequest lf (NotDidChangeTextDocument msg) = undefined
-- -- handleRequest lf (NotDidCloseTextDocument msg) = undefined

-- -- handleRequest lf (NotCancelRequestFromClient msg) = undefined

-- -- -- Requests query information from the server.
-- -- handleRequest lf (ReqDocumentSymbols msg) = undefined
-- -- handleRequest lf (ReqCodeAction msg) = undefined
-- -- handleRequest lf (ReqCodeLens msg) = undefined
-- -- handleRequest lf (ReqFoldingRange msg) = undefined

-- handleRequest _ msg = logs ("Unknown message " ++ gconNameOf (G.from msg))


-- sendMsg :: C.LspFuncs c -> (b -> FromServerMessage) -> (LspId -> a -> b) -> a -> IO ()
-- sendMsg lf wrapMsg mkMsg msg = do
--   rid <- C.getNextReqId lf
--   C.sendFunc lf . wrapMsg . mkMsg rid $ msg

class ConNames f where
  gconNameOf :: f a -> String

instance (ConNames f, ConNames g) => ConNames (f G.:+: g) where
  gconNameOf (L1 x) = gconNameOf x
  gconNameOf (R1 x) = gconNameOf x

instance (ConNames f) => ConNames (G.D1 c f) where
  gconNameOf (M1 x) = gconNameOf x

instance (Constructor c) => ConNames (G.C1 c f) where
  gconNameOf x = G.conName x

diagnosticOf :: (Note a Style, Pretty a) => Maybe DiagnosticSource -> a -> Diagnostic
diagnosticOf source m =
  Diagnostic
  { _range = rangeOf (annotation m)
  , _severity = Just (severityOf (diagnosticKind m))
  , _code = NumberValue . fromIntegral <$> noteId m
  , _message = display . renderPretty 0.4 100 . pretty $ m
  , _relatedInformation = Nothing
  , _source = source
  }

severityOf :: NoteKind -> DiagnosticSeverity
severityOf NoteMessage    = DsInfo
severityOf WarningMessage = DsWarning
severityOf ErrorMessage   = DsError

rangeOf :: Span -> Range
rangeOf s = Range (posOf (spanStart s)) (posOf (spanEnd s))

posOf :: SourcePos -> Position
posOf (SourcePos _ line col) = Position (line - 1) (col - 1)
