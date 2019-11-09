{-# LANGUAGE OverloadedStrings, DeriveGeneric, DuplicateRecordFields, TypeOperators, TypeSynonymInstances, FlexibleInstances, FlexibleContexts, TupleSections #-}
module Amc.Editor (run) where

import Prelude hiding (id)
import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Lens hiding (List)
import           Control.Monad
import           Control.Monad.Infer (firstName, TypeError(..))
import           Control.Monad.Namey
import           Data.Aeson.Types
import Data.These
import           Data.Bifunctor
import qualified Data.ByteString as BS
import           Data.Default
import           Data.Functor
import qualified Data.HashMap.Strict as HM
import           Data.Position
import qualified Data.Rope.UTF16 as Rope
import           Data.Span
import           Data.Spanned
import qualified Data.Text as T
import Data.Foldable
import           Frontend.Errors
import           GHC.Generics
import qualified GHC.Generics as G
import qualified Language.Haskell.LSP.Control as Control
import qualified Language.Haskell.LSP.Core as C
import           Language.Haskell.LSP.Messages
import           Language.Haskell.LSP.Types
import           Language.Haskell.LSP.Types.Lens hiding (error)
import           Language.Haskell.LSP.Utility (logs)
import qualified Language.Haskell.LSP.VFS as VFS
import           Parser (parseTops)
import           Parser.Wrapper (runParser)
import           Syntax (Toplevel(..), ModuleTerm(..), Binding(Binding))
import           Syntax.Builtin (builtinResolve, builtinEnv)
import           Syntax.Desugar (desugarProgram)
import           Syntax.Resolve (ResolveResult(..), resolveProgram)
import           Syntax.Resolve.Import
import           Syntax.Resolve.Scope (Signature)
import           Syntax.Types
import           Syntax.Pretty
import           Syntax.Var
import           Syntax.Verify
import           System.Exit
import qualified System.Log.Logger as L
import           Text.Pretty.Note
import           Text.Pretty.Semantic
import           Types.Infer (inferProgram)

newtype Config = Config
  { libraryPath :: Maybe [FilePath]
  }
  deriving (Show, Generic)

instance FromJSON Config where
  parseJSON = genericParseJSON defaultOptions

type Version = Int

data LoadedData a = LoadedData
  { currentVersion :: Version
  , contents       :: a
  }
  deriving Show

data FileState
  -- | A file which has been opened within the editor.
  = OpenedFile
    { openVersion  :: Version
    , openContents :: Rope.Rope
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

data State = State
  { loadedFiles :: TVar (HM.HashMap NormalizedUri FileState)
  , lastName    :: TVar Name
  , funcs       :: C.LspFuncs Config
  }

run :: IO ()
run = do
  C.setupLogger (Just "/tmp/amc-lsp.log") [] L.DEBUG

  mkState <- atomically $ State <$> newTVar mempty <*> newTVar firstName
  qIn <- atomically newTChan
  e <- Control.run (callbacks mkState qIn) (handlers qIn) options Nothing
  exitWith (if e == 0 then ExitSuccess else ExitFailure e)

  where
    callbacks :: (C.LspFuncs Config -> State) -> TChan FromClientMessage -> C.InitializeCallbacks Config
    callbacks mkSt qIn = C.InitializeCallbacks
        { C.onInitialConfiguration
            = maybe (pure (Config Nothing)) (first T.pack . parseEither parseJSON)
            . (^. params . initializationOptions)
        , C.onConfigurationChange
            = first T.pack . parseEither parseJSON . (^. params . settings)
        , C.onStartup = \lf -> forkIO (loop (mkSt lf) qIn) >> return Nothing
        }

    loop :: State -> TChan FromClientMessage -> IO ()
    loop st qIn = forever $ atomically (readTChan qIn) >>= handleRequest st

    handlers :: TChan FromClientMessage -> C.Handlers
    handlers qIn = def
      { C.initializedHandler  = handle NotInitialized
      , C.responseHandler     = handle RspFromClient

        -- Notifications alert us when something has changed.
      , C.didOpenTextDocumentNotificationHandler   = handle NotDidOpenTextDocument
      , C.didSaveTextDocumentNotificationHandler   = handle NotDidSaveTextDocument
      , C.didChangeWatchedFilesNotificationHandler = handle NotDidChangeWatchedFiles
      , C.didChangeTextDocumentNotificationHandler = handle NotDidChangeTextDocument
      , C.didCloseTextDocumentNotificationHandler  = handle NotDidCloseTextDocument
      , C.cancelNotificationHandler                = handle NotCancelRequestFromClient

        -- Requests query information from the server.
      -- , C.documentSymbolHandler = handle ReqDocumentSymbols
      , C.codeActionHandler     = handle ReqCodeAction
      , C.codeLensHandler       = handle ReqCodeLens
      -- , C.foldingRangeHandler   = handle ReqFoldingRange

      }
      where handle c = Just (\noti -> atomically $ writeTChan qIn (c noti))

    commandIds :: [T.Text]
    commandIds = []

    options :: C.Options
    options =
      def { C.textDocumentSync       = Just TextDocumentSyncOptions
                                       { _openClose         = Just True
                                       , _change            = Just TdSyncIncremental
                                       , _willSave          = Just False
                                       , _willSaveWaitUntil = Just False
                                       , _save              = Just SaveOptions { _includeText = Just False }
                                       }
          , C.completionProvider     = Just CompletionOptions
                                       { _resolveProvider   = Just True
                                       , _triggerCharacters = Just ["."]
                                       }
          , C.codeActionProvider     = Just (CodeActionOptionsStatic True)
          , C.codeLensProvider       = Just (CodeLensOptions (Just False))
          , C.foldingRangeProvider   = Just (FoldingRangeOptionsStatic True)
          , C.executeCommandProvider = Just ExecuteCommandOptions
                                       { _commands          = List commandIds
                                       }
          }

handleRequest :: State -> FromClientMessage -> IO ()

handleRequest _ (RspFromClient msg) = logs ("Received message from client (" ++ show msg ++ ")")
handleRequest _ NotInitialized{} = do
  -- sendMsg (funcs st) ReqRegisterCapability fmServerRegisterCapabilityRequest . RegistrationParams . List $
  --   [ ]
  logs "Initialized server."

-- -- Notifications alert us when something has changed.
handleRequest st (NotDidOpenTextDocument msg) = do
  let nUri = toNormalizedUri (msg ^. params . textDocument . uri)

  file <- C.getVirtualFileFunc (funcs st) nUri
  case file of
    Nothing -> pure ()
    Just file -> do
      atomically . modifyTVar (loadedFiles st) $ HM.insert nUri OpenedFile
        -- TODO: Check when we've got already loaded files
        { openVersion = VFS._version file
        , openContents = VFS._text file
        , openParsed = Nothing, openResolved = Nothing, openTyped = Nothing
        , errors = mempty
        }
      updateDocument st nUri (VFS._version file)

-- handleRequest lf (NotDidSaveTextDocument msg) = undefined
-- handleRequest lf (NotDidChangeWatchedFiles msg) = undefined
handleRequest st (NotDidChangeTextDocument msg) = do
  let nUri = toNormalizedUri (msg ^. params . textDocument . uri)
  file <- C.getVirtualFileFunc (funcs st) nUri
  case file of
    Nothing -> pure ()
    Just file -> do
      atomically . modifyTVar (loadedFiles st) $ HM.update (\f -> Just f
        -- TODO: More sanity checks
        { openVersion = VFS._version file
        , openContents = VFS._text file
        }) nUri
      updateDocument st nUri (VFS._version file)

-- handleRequest lf (NotDidChangeTextDocument msg) = undefined
-- handleRequest lf (NotDidCloseTextDocument msg) = undefined

-- handleRequest lf (NotCancelRequestFromClient msg) = undefined

-- -- Requests query information from the server.
-- handleRequest lf (ReqDocumentSymbols msg) = undefined

handleRequest st (ReqCodeAction msg) = do
  let nUri = toNormalizedUri (msg ^. params . textDocument . uri)
  file <- atomically (HM.lookup nUri <$> readTVar (loadedFiles st))
  case file of
    Just (OpenedFile { openVersion = oVersion , errors = errors }) ->
      let versioned = VersionedTextDocumentIdentifier (msg ^. params . textDocument . uri) (Just oVersion) in
      sendReply st msg RspCodeAction . List $
        foldl' (getAction versioned (msg ^. params . range)) [] (errors ^. typeErrors)
    _ ->
      sendReplyError st msg $ ResponseError RequestCancelled "File is not open." Nothing

  where
    getAction file range ac err
      | errPos <- rangeOf (annotation err)
      , range ^. end >= errPos ^. start
      , range ^. start <= errPos ^. end
      = getActionOf file errPos ac err
      | otherwise = ac

    getActionOf file range ac (ArisingFrom e _) = getActionOf file range ac e
    getActionOf file range ac (FoundHole _ _ exprs@(_:_)) = map (CACodeAction . mkAction file range . pretty) exprs ++ ac
    getActionOf _ _ ac _ = ac

    mkAction file range expr =
      let simple = renderBasic expr
      in CodeAction
      { _title = "Replace hole with '"
              <> (if T.length simple > 20 then T.take 17 simple <> "..." else simple)
              <> "'"
      , _kind = Just CodeActionQuickFix
      , _diagnostics = Nothing
      , _edit = Just (WorkspaceEdit
                      { _changes = Nothing
                      , _documentChanges = Just . List $
                        [ TextDocumentEdit
                          { _textDocument = file
                          , _edits = List [ TextEdit range (renderBasic (hang (range ^. start . character) expr)) ]
                          } ] })
      , _command = Nothing
      }

handleRequest st (ReqCodeLens msg) = do
  let nUri = toNormalizedUri (msg ^. params . textDocument . uri)
  file <- atomically (HM.lookup nUri <$> readTVar (loadedFiles st))
  case file of
    Just (OpenedFile { openVersion = oVersion
                     , openTyped = Just (LoadedData lVersion (_, _, prog))
                     })
      | oVersion == lVersion ->
        sendReply st msg RspCodeLens . List $ getTops [] prog
    _ ->
      sendReplyError st msg $ ResponseError RequestCancelled "File is not up-to-date" Nothing

  where
    -- TODO: Resolve these!!
    getTop ac (LetStmt _ _ b) = foldl getBinding ac b
    getTop ac (Module _ _ (ModStruct ms _)) = getTops ac ms
    getTop ac (Open (ModStruct ms _)) = getTops ac ms
    getTop ac (Include (ModStruct ms _)) = getTops ac ms
    getTop ac _ = ac
    getTops = foldl' getTop

    getBinding :: [CodeLens] -> Binding Typed -> [CodeLens]
    getBinding ac (Binding v _ _ (p, ty)) =
      CodeLens (rangeOf p)
        (Just (Command (renderBasic $ pretty v <+> colon <+> displayType ty) "" Nothing))
        Nothing
      : ac
    getBinding ac _ = ac

-- handleRequest lf (ReqFoldingRange msg) = undefined

handleRequest _ msg = logs ("Unknown message " ++ gconNameOf (G.from msg))

updateDocument :: State -> NormalizedUri -> Version -> IO ()
updateDocument state path@(NormalizedUri pathT) version = do
  file <- atomically $ HM.lookup path <$> readTVar (loadedFiles state)
  case file of
    Nothing -> pure ()
    Just DiskFile{} -> pure ()
    Just file@OpenedFile{}
      | openVersion file /= version -> pure ()
      | otherwise -> do
      let contents = Rope.toLazyText (openContents file)
          (parsed, pEs) = runParser (T.unpack pathT) contents parseTops

      ok <- continueWith (\f -> f { openParsed = LoadedData version <$> parsed <|> openParsed f
                                  , errors     = mempty & parseErrors .~ pEs })
      case parsed of
        Nothing -> publish (mempty & parseErrors .~ pEs)
        Just{} | not ok -> pure ()
        Just parsed -> do
          (resolved, rEs) <- ofEither <$> wrapNamey state (runNullImport (resolveProgram builtinResolve parsed))

          ok <- continueWith (\f -> f { openResolved = (LoadedData version . getResolved) <$> resolved <|> openResolved f
                                      , errors       = errors f & resolveErrors .~ rEs })
          case resolved of
            Nothing -> publish (mempty & (parseErrors .~ pEs) . (resolveErrors .~ rEs))
            Just{} | not ok -> pure ()
            Just (ResolveResult resolved sig _) -> do
              (typed, tEs) <- ofThese <$> wrapNamey state (desugarProgram resolved >>= inferProgram builtinEnv)
              ok <- continueWith (\f -> f { openTyped = (LoadedData version . uncurry (flip (sig,,))) <$> typed <|> openTyped f
                                          , errors    = errors f & typeErrors .~ tEs })
              case typed of
                Nothing -> publish (mempty & (parseErrors .~ pEs) . (resolveErrors .~ rEs) . (typeErrors .~ tEs))
                Just{} | not ok -> pure ()
                Just (prog, env) -> do
                  name <- atomically (readTVar (lastName state))
                  let vEs = toList . snd . runVerify env name $ verifyProgram prog
                  ok <- continueWith (\f -> f { errors = errors f & verifyErrors .~ vEs })
                  if ok
                  then publish (mempty & (parseErrors .~ pEs) . (resolveErrors .~ rEs) . (typeErrors .~ tEs) . (verifyErrors .~ vEs))
                  else pure ()

  where
    ofEither :: Monoid a => Either a b -> (Maybe b, a)
    ofEither (Left es) = (Nothing, es)
    ofEither (Right x) = (Just x, mempty)

    ofThese :: Note n s => These [n] a -> (Maybe a, [n])
    ofThese (That x) = (Just x, mempty)
    ofThese (This es) = (Nothing, es)
    ofThese (These es x) | any isError es = (Nothing, es)
                         | otherwise = (Just x, es)

    isError :: Note a b => a -> Bool
    isError x = diagnosticKind x == ErrorMessage

    getResolved :: ResolveResult -> (Signature, [Toplevel Resolved])
    getResolved (ResolveResult prog sig _) = (sig, prog)

    continueWith :: (FileState -> FileState) -> IO Bool
    continueWith modify = atomically $ do
      files <- readTVar (loadedFiles state)
      case HM.lookup path files of
        Nothing -> pure False
        Just DiskFile{} -> pure False
        Just file@OpenedFile{} -> do
          writeTVar (loadedFiles state) $! HM.insert path (modify file) files
          pure (openVersion file == version)

    publish :: ErrorBundle -> IO ()
    publish es =
      C.sendFunc (funcs state) . NotPublishDiagnostics
        . fmServerPublishDiagnosticsNotification
        $ PublishDiagnosticsParams
        { _uri = fromNormalizedUri path
        , _diagnostics = List (  map (diagnosticOf (Just "amc.parser")) (es ^. parseErrors)
                              ++ map (diagnosticOf (Just "amc.resolve")) (es ^. resolveErrors)
                              ++ map (diagnosticOf (Just "amc.tc")) (es ^. typeErrors)
                              ++ map (diagnosticOf (Just "amc.verify")) (es ^. verifyErrors) )
        }

  --         (result, resErrors, typeErrors, verifyErrors) = execNameyT $ do
  --           case resolved of
  --             Left es -> pure (Nothing, es, mempty, mempty)
  --             Right (ResolveResult resolved sig _) -> do
  --               desugared <- desugarProgram resolved
  --               inferredR <- inferProgram builtinEnv desugared
  --               n <- genName
  --               case inferredR of
  --                 That (tBody, env) ->
  --                   pure (Just (sig, env,
  --                   verifyProg





  --     pure ()


sendMsg :: State -> (b -> FromServerMessage) -> (LspId -> a -> b) -> a -> IO ()
sendMsg st wrapMsg mkMsg msg = do
  rid <- C.getNextReqId (funcs st)
  C.sendFunc (funcs st) . wrapMsg . mkMsg rid $ msg

sendReply :: State -> RequestMessage m req resp -> (ResponseMessage resp -> FromServerMessage) -> resp -> IO ()
sendReply st req wrapMsg msg = do
  C.sendFunc (funcs st) . wrapMsg $
    ResponseMessage "2.0" (responseId $ req ^. id) (Just msg) Nothing

sendReplyError :: State -> RequestMessage m req resp -> ResponseError -> IO ()
sendReplyError st req err = do
  C.sendFunc (funcs st) . RspError $
    ResponseMessage "2.0" (responseId $ req ^. id) Nothing (Just err)


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
  , _message = renderBasic . pretty $ m
  , _relatedInformation = Nothing
  , _source = source
  }

severityOf :: NoteKind -> DiagnosticSeverity
severityOf NoteMessage    = DsInfo
severityOf WarningMessage = DsWarning
severityOf ErrorMessage   = DsError

rangeOf :: Span -> Range
rangeOf s = Range (posOf (spanStart s)) (posOf' (spanEnd s)) where
  posOf' (SourcePos _ line col) = Position (line - 1) col

posOf :: SourcePos -> Position
posOf (SourcePos _ line col) = Position (line - 1) (col - 1)

-- | Run a namey monad within the current state. Note, only one namey
-- action can be run at once, so this should be done from the main
-- executor.
wrapNamey :: State -> NameyT IO a -> IO a
wrapNamey st m = do
  oldName <- atomically (readTVar (lastName st))
  (res, newName) <- runNameyT m oldName
  atomically $ do
    curName <- readTVar (lastName st)
    when (curName /= oldName) (error "Multiple NameyT actions run concurrently.")
    writeTVar (lastName st) newName
  pure res

renderBasic :: Doc -> T.Text
renderBasic = display . uncommentDoc . renderPretty 0.4 100
