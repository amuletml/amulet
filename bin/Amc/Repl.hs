{-# LANGUAGE OverloadedStrings, FlexibleContexts, ScopedTypeVariables, ViewPatterns, TypeFamilies, MultiWayIf, TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Amc.Repl
  ( ReplConfig(..)
  , repl
  , replFrom
  , runRemoteReplCommand
  ) where

import Control.Monad.State.Strict
import Control.Exception

import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.IO as T
import qualified Data.Text as T

import qualified Data.ByteString as Bs
import qualified Data.VarMap as VarMap

import Data.Traversable
import Data.Bifunctor
import Data.Foldable
import Data.Position
import Data.Functor
import Data.Spanned
import Data.Triple
import Data.Maybe
import Data.Char

import qualified Foreign.Lua.Core.Types as L
import qualified Foreign.Lua as L

import System.Console.Haskeline hiding (display, bracket, throwTo)
import System.Directory
import System.IO

import qualified Syntax.Builtin as Bi
import Syntax.Resolve (ResolveResult(..), resolveProgram)
import Syntax.Resolve.Scope (Signature)
import Syntax.Resolve.Import (runNullImport)
import Syntax (displayType)
import qualified Syntax.Var as S
import qualified Syntax as S

import qualified Control.Monad.Infer as T
import Control.Monad.Namey

import Parser.Wrapper (Parser, runParser)
import Parser.Token
import Parser.Error
import Parser

import qualified Core.Lower as L
import qualified Core.Core as C
import Core.Occurrence
import Core.Core (Stmt)
import Core.Simplify
import Core.Var

import qualified Frontend.Driver as D
import Frontend.Errors

import Control.Lens

import qualified Backend.Lua.Postprocess as B
import qualified Backend.Lua.Emit as B
import qualified Backend.Escape as B
import Language.Lua.Syntax

import Text.Pretty.Semantic

import qualified Network.Socket as Net

import Control.Concurrent

import Amc.Repl.Display
import Amc.Debug
import Version

-- Aghh MonadFail!
instance MonadFail L.Lua where
  fail x = error $ "MonadFail L.Lua: fail " ++ x

data ReplConfig = ReplConfig
  { port         :: Int
  , debugMode    :: DebugMode
  , root         :: FilePath
  , driverConfig :: D.DriverConfig
  , prelude      :: Maybe FilePath
  , coreLint     :: Bool
  }
  deriving Show

data ReplState = ReplState
  { resolveScope :: Signature
  , inferScope   :: T.Env
  , emitState    :: B.TopEmitState
  , lastName     :: S.Name
  , lowerState   :: L.LowerState

  , driver       :: D.Driver
  , config       :: ReplConfig

  , luaState     :: L.State

  , currentFile  :: Maybe FilePath
  , outputHandle :: Handle
  }

defaultState :: ReplConfig -> IO ReplState
defaultState config = do
  state <- L.newstate
  -- Init our default libraries
  L.runWith state L.openlibs

  pure ReplState
    { resolveScope = Bi.builtinResolve
    , inferScope   = Bi.builtinEnv
    , emitState    = B.defaultEmitState
    , lowerState   = L.defaultState
    , luaState     = state

    , lastName     = S.TgName (T.pack "a") 1
    , driver       = D.makeDriverWith (driverConfig config)
    , config       = config

    , currentFile  = Nothing
    , outputHandle = stdout
    }

resetState :: ReplState -> IO ReplState
resetState state = do
  lState <- L.newstate
  L.runWith lState L.openlibs
  pure state
    { resolveScope = Bi.builtinResolve
    , inferScope   = Bi.builtinEnv
    , emitState    = B.defaultEmitState
    , lowerState   = L.defaultState
    , luaState     = lState
    }

type Listener = Maybe ThreadId

runRepl :: Listener -> InputT (StateT ReplState IO) ()
runRepl tid = do
  line <- getInputLine "> "
  case line of
    Nothing -> finish tid
    Just "" -> runRepl tid
    Just (':':cmd) -> do
      lift $ uncurry (execCommand tid) . span (/=' ') $ cmd
      runRepl tid
    Just line -> getInput line False >>= (lift . execString "=stdin" . T.pack) >> runRepl tid

  where
    getInput :: String -> Bool -> InputT (StateT ReplState IO) String
    getInput input empty =
      case runParser "=stdin" (L.pack input) parseRepl' of
        (Nothing, xs) ->
          case last xs of
            UnclosedString{} -> continue
            UnclosedComment{} -> continue
            UnexpectedEnd{} -> continue
            UnexpectedToken (Token TcEOF _ _) _ -> continue
            _ -> pure input
        _ -> pure input
        where continue = do
                line <- getInputLine ". "
                case line of
                  Nothing -> pure input
                  Just "" | empty -> pure input
                  Just line -> getInput (input ++ '\n':line) (line == "")

parseRepl' :: Parser (Either [S.Toplevel S.Parsed] (S.Expr S.Parsed))
parseRepl' = first pure <$> parseRepl

execCommand :: (MonadState ReplState m, MonadIO m) => Listener -> String -> String -> m ()
execCommand tid "quit" _ = finish tid
execCommand tid "q"    _ = finish tid

execCommand _ "l"    arg = loadCommand arg
execCommand _ "load" arg = loadCommand arg

execCommand _ "r"      _ = reloadCommand
execCommand _ "reload" _ = reloadCommand

execCommand _ "t" arg = typeCommand arg
execCommand _ "type" arg = typeCommand arg

execCommand _ "i" arg = infoCommand arg
execCommand _ "info" arg = infoCommand arg

execCommand _ "c" arg = compileCommand arg
execCommand _ "compile" arg = compileCommand arg

execCommand _ "add-library-path" arg =
  case dropWhile isSpace arg of
    [] -> liftIO $ putStrLn ":add-library-path needs an argument"
    dir -> do
      path <- liftIO $ canonicalizePath dir
      exists <- liftIO $ doesDirectoryExist path
      if exists
      then wrapDriver $ D.adjustConfig (\c -> c { D.libraryPath = path : D.libraryPath c })
      else liftIO . putStrLn $ arg ++ ": No such directory"

execCommand _ "version" _ = liftIO (putStrLn ("The Amulet compiler, version " ++ $amcVersion))

execCommand _ cmd _ = outputDoc ("Unknown command" <+> verbatim cmd)

-- | Split a string into arguments
parseArgs :: String -> [String]
parseArgs xs =
  let (ys, y) = parseArgs' xs
      parseArgs' "" = ([], Nothing)
      parseArgs' (' ':xs) = (parseArgs xs, Nothing)
      parseArgs' ('\\':x:xs) = Just . (x:) . fromMaybe [] <$> parseArgs' xs
      parseArgs' (x:xs) = Just . (x:) . fromMaybe [] <$> parseArgs' xs
   in maybe ys (:ys) y


loadCommand :: (MonadState ReplState m, MonadIO m) => String -> m ()
loadCommand arg = case parseArgs arg of
                    [file] -> loadFile (Just file)
                    _ -> outputDoc "Usage `:load [file]`"

reloadCommand :: (MonadState ReplState m, MonadIO m) => m ()
reloadCommand = loadFile =<< gets currentFile

infoCommand :: (MonadState ReplState m, MonadIO m) => String -> m ()
infoCommand (T.pack . dropWhile isSpace -> input) = do
  state <- get
  let files :: [(String, T.Text)]
      files = [("<input>", input)]
      (parsed, parseMsg) = runParser "<input>" (L.fromStrict input) parseInfoVar
      handle = outputHandle state
  liftIO $ traverse_ (hReport (outputHandle state) files) parseMsg
  case parsed of
    Nothing -> pure ()
    Just var -> do
      let prog :: [S.Toplevel S.Parsed]
          prog = [ S.LetStmt S.NonRecursive S.Public
                   [ S.Binding (S.Name "_")
                        (S.VarRef (getL var) (annotation var))
                        True (annotation var) ] ]

      resolved <-
          flip evalNameyT (lastName state)
        . runNullImport
        $ resolveProgram (resolveScope state) prog

      case resolved of
        Right (ResolveResult [ S.LetStmt _ _ [S.Binding _ (S.VarRef name _) _ _] ] _ _) ->
          liftIO . hPutDoc handle . displayType $
            (inferScope state ^. T.names . at name . non undefined)
        _ -> liftIO . hPutDoc handle $ "Name not in scope:" <+> pretty (getL var)


typeCommand :: (MonadState ReplState m, MonadIO m) => String -> m ()
typeCommand (dropWhile isSpace -> input) = do
  state <- get
  let files :: [(String, T.Text)]
      files = [("<input>", T.pack input)]
      (parsed, parseMsg) = runParser "<input>" (L.pack input) parseReplExpr
      handle = outputHandle state
  liftIO $ traverse_ (hReport (outputHandle state) files) parseMsg
  case parsed of
    Nothing -> pure ()
    Just parsed -> do
      let ann = annotation parsed
          prog :: [S.Toplevel S.Parsed]
          prog = [ S.LetStmt S.NonRecursive S.Public [ S.Matching (S.Wildcard ann) parsed ann ] ]

      (infer, es) <- wrapDriver $ do
        D.tick
        D.inferWith (root (config state)) prog (resolveScope state) (inferScope state)
      hReportAll (outputHandle state) files es
      case infer of
        Nothing -> pure ()
        Just (prog, _, _) ->
          let ~[S.LetStmt _ _ [ S.TypedMatching _ expr _ _ ]] = prog
              t = S.getType expr
          in liftIO $ hPutDoc handle (string input <+> colon <+> displayType t)

compileCommand :: (MonadState ReplState m, MonadIO m) => FilePath -> m ()
compileCommand [] = liftIO $ putStrLn ":compile command needs an argument"
compileCommand (dropWhile isSpace -> path) = do
  current <- gets currentFile
  output <- gets outputHandle
  files <- D.fileMap =<< gets driver
  case current of
    Just file -> do
      in_p <- liftIO $ canonicalizePath file
      (core, errors) <- wrapDriver $ D.tick >> D.compiles in_p
      handle <- liftIO $ openFile path WriteMode

      case core of
        Just core -> do
          lint <- gets (coreLint . config)
          optm <- wrapNamey $ optimise lint core
          (_, lua) <- emitCore optm
          liftIO $ Bs.hPutStr handle lua
        Nothing ->
          hReportAll output files errors
      liftIO $ hClose handle
    Nothing -> liftIO $ putStrLn "No file loaded"

execString :: (MonadState ReplState m, MonadIO m)
           => SourceName -> T.Text
           -> m Bool
execString name line = do
  oldInfer <- gets inferScope
  core <- parseCore parseRepl' name line
  case core of
    Nothing -> pure False
    Just (vs, prog, core) -> do
      (luaExpr, luaSyntax) <- emitCore core
      state <- get
      (ok, res) <- liftIO $ do
        dumpTypes (debugMode (config state)) prog oldInfer (inferScope state)
        dumpCore (debugMode (config state)) core core luaExpr

        L.runWith (luaState state) $ do
          L.OK <- L.dostring "-- time out hook\nlocal function f() error('Timed out!', 3) end; debug.sethook(f, '', 1e6)"
          L.OK <- L.loadbuffer luaSyntax ('=':name)
          res <- L.try $ L.call 0 L.multret

          case res of
            Right () -> do
              vs' <- for vs $ \(v, _) -> do
                let Just (_, vs) = VarMap.lookup v (emitState state ^. B.topVars)
                repr <- traverse (valueRepr . evalExpr . B.unsimple) vs
                let CoVar id _ _ = v
                    var = S.TgName (covarDisplayName v) id
                case inferScope state ^. T.names . at var of
                  Just _ -> pure (Just (pretty v <+> equals <+> hsep (map pretty repr)))
                  Nothing -> pure Nothing

              pure (True, catMaybes vs')
            Left (L.Exception msg) -> pure (False, [string msg])

      unless (null res) (outputDoc (vsep res))
      pure ok

evalExpr :: LuaExpr -> L.Lua ()
evalExpr (LuaRef (LuaName n)) = L.getglobal (T.unpack n)
evalExpr LuaNil = L.pushnil
evalExpr LuaTrue = L.pushboolean True
evalExpr LuaFalse = L.pushboolean True
evalExpr (LuaInteger n) = L.pushinteger (fromIntegral n)
evalExpr (LuaNumber n) = L.pushnumber (L.Number n)
evalExpr (LuaString s) = L.pushstring (T.encodeUtf8 s)
evalExpr s = error ("Not a simple expression: " ++ show s)

-- | We convert any top-level local declarations into global ones. This
-- means they are accessible outside normal REPL invocations.
patchupLua :: B.TopEmitState -> LuaStmt -> LuaStmt
patchupLua s (LuaLocal vs [])
  | all (isTopVar s) vs
  = LuaAssign vs [LuaNil]
patchupLua s (LuaLocal vs es)
  | all (isTopVar s) vs
  = LuaAssign vs es
patchupLua s (LuaLocalFun v as ss)
  | isTopVar s v
  = LuaAssign [v] [LuaFunction as ss]
patchupLua _ x = x

isTopVar :: B.TopEmitState -> LuaVar -> Bool
isTopVar s (LuaName v)
  | Just _ <- B.getEscaped v (s ^. B.topEscape) :: Maybe CoVar
  = True
isTopVar _ _ = False

-- | Patchup the usage of a series of statements to ensure every one is
-- considered "used".
--
-- This guarantees we'll generate bindings for them all, and so they can
-- be evaluated in the REPL.
patchupUsage :: IsVar a => [C.AnnStmt b (OccursVar a)] -> [C.AnnStmt b(OccursVar a)]
patchupUsage [] = []
patchupUsage (s@C.Foreign{}:xs) = s:patchupUsage xs
patchupUsage (s@C.Type{}:xs) = s:patchupUsage xs
patchupUsage (C.StmtLet (C.One v):xs) = C.StmtLet (C.One (first3 patchupVarUsage v)):patchupUsage xs
patchupUsage (C.StmtLet (C.Many v):xs) = C.StmtLet (C.Many (map (first3 patchupVarUsage) v)):patchupUsage xs

-- | Patchup the usage of a single variable. See 'patchupUsage' for more
-- information.
patchupVarUsage :: OccursVar a -> OccursVar a
patchupVarUsage (OccursVar v u) = OccursVar v (u <> Once)

parseCore :: (MonadState ReplState m, MonadIO m)
          => Parser (Either [S.Toplevel S.Parsed] (S.Expr S.Parsed))
          -> SourceName -> T.Text
          -> m (Maybe ([(CoVar, C.Type CoVar)]
                      , [S.Toplevel S.Typed]
                      , [Stmt CoVar]))
parseCore parser name input = do
  state <- get
  let files :: [(String, T.Text)]
      files = [(name, input)]
      (parsed, parseMsg) = runParser name (L.fromStrict input) parser
  liftIO $ traverse_ (hReport (outputHandle state) files) parseMsg

  case parsed of
    Nothing -> pure Nothing
    Just parsed -> do
      let parsed' = case parsed of
            Left s -> s
            Right e -> [S.LetStmt S.NonRecursive S.Public [S.Binding (S.Name "_") e True (annotation e)]]

      (lower, es) <- wrapDriver $ do
        D.tick
        D.lowerWith (root (config state)) parsed' (resolveScope state) (inferScope state) (lowerState state)
      driver_files <- D.fileMap =<< gets driver
      hReportAll (outputHandle state) (files ++ driver_files) es
      case lower of
        Nothing -> pure Nothing
        Just (lower, lState, typed, env, ResolveResult _ _ sig) -> do
          let lastTerms = case lower of
                [] -> []
                _:_ -> case last lower of
                  (C.StmtLet (C.One (v, t, _))) -> [(v, t)]
                  (C.StmtLet (C.Many vs)) -> map (\(v, t, _) -> (v, t)) vs
                  _ -> []

          modify (\s -> s { resolveScope = sig, inferScope = env, lowerState = lState })
          pure $ Just (lastTerms, typed, lower)


emitCore :: MonadState ReplState m => [Stmt CoVar] -> m (LuaStmt, Bs.ByteString)
emitCore core = do
  emit <- gets emitState

  let core' = patchupUsage . snd . tagOccurStmt (const occursSet) OccursVar $ core
      (luaStmt, emit') = uncurry B.addBuiltins $ runState (B.emitStmt core') emit
      luaExpr = LuaDo . map (patchupLua emit') . toList $ luaStmt
      luaSyntax = T.encodeUtf8 . display . uncommentDoc . renderPretty 0.8 100 . pretty $ luaExpr

  modify (\s -> s { emitState = emit' })
  pure (luaExpr, luaSyntax)

-- | Reset the environment and load a series of files from environment
loadFile :: (MonadState ReplState m, MonadIO m) => Maybe FilePath -> m ()
loadFile file = do
  -- Reset the state
  put =<< (liftIO . resetState) =<< get
  modify (\s ->  s { currentFile = file })
  wrapDriver D.tock

  -- Determine whether to load the prelude and an additional file.
  prelude <- gets (toList . prelude . config)
  load <- case file of
    Nothing -> pure []
    Just file -> do
      path <- liftIO $ canonicalizePath file
      exists <- liftIO $ doesFileExist path
      if exists
      then outputDoc ("Loading:" <+> verbatim file) $> [path]
      else outputDoc ("Cannot open" <+> verbatim file) $> []

  loadFiles (prelude ++ load) $> ()

outputDoc :: (MonadState ReplState m, MonadIO m) => Doc -> m ()
outputDoc x = do
  h <- gets outputHandle
  liftIO $ do
    hPutDoc h x
    hFlush h

runRemoteReplCommand :: Int -> String -> IO ()
runRemoteReplCommand port command = Net.withSocketsDo $ do
  sock <- Net.socket Net.AF_INET Net.Stream Net.defaultProtocol
  r <- try $ Net.connect sock (Net.SockAddrInet (fromIntegral port) $ Net.tupleToHostAddress (127, 0, 0, 1))
  case r of
    Right () -> do
      handle <- Net.socketToHandle sock ReadWriteMode
      hSetEncoding handle utf8
      T.hPutStrLn handle (T.pack command)
      T.hPutStrLn handle mempty
      T.putStr =<< T.hGetContents handle
    Left (_ :: SomeException) ->
      putStrLn $ "Failed to connect to server on port " ++ show port

loadFiles :: (MonadState ReplState m, MonadIO m) => [FilePath] -> m Bool
loadFiles [] = pure True
loadFiles paths = do
  (core, es) <- wrapDriver $ D.tick >> D.compile paths

  files <- D.fileMap =<< gets driver
  handle <- gets outputHandle
  hReportAll handle files es
  case core of
    Nothing -> pure False
    Just core -> do
      for_ paths $ \path -> do
        (sig, env, lEnv) <- wrapDriver $ do
          ~(Just sig) <- D.getSignature path
          ~(Just env) <- D.getOpenedTypeEnv path
          ~(Just lEnv) <- D.getLowerState path
          pure (sig, env, lEnv)

        modify (\s -> s { resolveScope = resolveScope s <> sig
                        , inferScope = inferScope s <> env
                        , lowerState = lowerState s <> lEnv })

      (luaExpr, luaSyntax) <- emitCore core

      luaState <- gets luaState
      debug <- gets (debugMode . config)
      liftIO $ do
        dumpCore debug core core luaExpr
        res <- L.runWith luaState $ do
          code <- L.dostring luaSyntax
          case code of
            L.OK -> pure (Right ())
            _ -> do
              val <- valueRepr (pure ())
              L.pop 1
              case val of
                String str -> pure (Left (text str <+> parens (shown code)))
                _ -> pure (Left (keyword "Error:" <+> pretty val))

        case res of
          Left err -> hPutDoc handle err >> pure False
          Right () -> pure True

repl :: ReplConfig -> IO ()
repl config = replFrom config Nothing

replFrom :: ReplConfig -> Maybe FilePath -> IO ()
replFrom config file = do
  state <- execStateT (loadFile file) =<< defaultState config
  hSetBuffering stdout LineBuffering

  ready <- newEmptyMVar
  tid <-
    if port config /= 0
       then forkIO $ startServer ready (port config) state
       else do
         putMVar ready ()
         myThreadId

  takeMVar ready
  bracket (pure ()) (const (killThread tid)) $ \() ->
    evalStateT (runInputT defaultSettings (runRepl (Just tid))) state

finish :: MonadIO m => Listener -> m ()
finish Nothing = pure ()
finish (Just i) = liftIO $ throwTo i ThreadKilled

startServer :: MVar () -> Int -> ReplState -> IO ()
startServer ready port state = Net.withSocketsDo $ bracket getSock Net.close (work state) where
  getSock = do
    (addr:_) <-
      Net.getAddrInfo (Just (Net.defaultHints { Net.addrFlags = [ Net.AI_NUMERICHOST
                                                                , Net.AI_PASSIVE
                                                                ]
                                              , Net.addrSocketType = Net.Stream }))
                      (Just "127.0.0.1")
                      (Just (show port))
    sock <- Net.socket Net.AF_INET Net.Stream Net.defaultProtocol
    x <- try $ do
      Net.setSocketOption sock Net.ReuseAddr 1
      Net.bind sock (Net.addrAddress addr)
      Net.listen sock 4
    case x of
      Left (_ :: SomeException) -> do
        putStrLn $ "Failed to start REPL server on port " ++ show port
        killThread =<< myThreadId
      Right () -> pure ()
    putStrLn $ "Listening on port " ++ show port
    putMVar ready ()
    pure sock

  handleReplLine line =
    if | T.null line -> pure ()
       | ':' == T.head line -> uncurry (execCommand Nothing) . span (/=' ') $ T.unpack (T.tail line)
       | otherwise -> () <$ execString "=<remote command>" line

  handleLines handle = do
    eof <- liftIO $ hIsEOF handle
    if eof then
      pure ()
    else do
      line <- liftIO $ T.hGetLine handle
      if T.null line then
        pure ()
      else do
        handleReplLine line
        handleLines handle

  work state sock = do
    (conn, _) <- Net.accept sock
    handle <- Net.socketToHandle conn ReadWriteMode
    hSetEncoding handle utf8

    state <- execStateT (handleLines handle) (state { outputHandle = handle })

    hClose handle

    work state sock

wrapDriver :: MonadState ReplState m
           => StateT D.Driver (NameyT m) a
           -> m a
wrapDriver m = do
  state <- get
  ((res, drive), name)
    <- flip runNameyT (lastName state)
     . flip runStateT (driver state)
     $ m
  put state { lastName = name, driver = drive }
  pure res

wrapNamey :: MonadState ReplState m
          => NameyT m a
          -> m a
wrapNamey m = do
  (res, name) <- runNameyT m =<< gets lastName
  modify (\s -> s { lastName = name })
  pure res
