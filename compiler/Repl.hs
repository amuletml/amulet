{-# LANGUAGE OverloadedStrings, FlexibleContexts, ScopedTypeVariables, ViewPatterns, TypeFamilies, MultiWayIf #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Repl
  ( repl
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
import Data.Spanned
import Data.Functor
import Data.Triple
import Data.These
import Data.Maybe
import Data.Char

import qualified Foreign.Lua.Core.Types as L
import qualified Foreign.Lua as L

import System.Console.Haskeline hiding (display, bracket, throwTo)
import System.IO

import qualified Syntax.Resolve.Toplevel as R
import qualified Syntax.Resolve.Scope as R
import qualified Syntax.Builtin as Bi
import Syntax.Resolve (resolveProgram)
import Syntax.Desugar (desugarProgram)
import Syntax.Verify
import Syntax (displayType)
import qualified Syntax.Var as S
import qualified Syntax as S

import qualified Control.Monad.Infer as T
import Control.Monad.Namey

import Types.Infer (inferProgram, inferExpr)

import Parser.Wrapper (Parser, runParser)
import Parser.Token
import Parser.Error
import Parser

import qualified Core.Core as C
import qualified Core.Lower as L
import Core.Optimise.Newtype
import Core.Occurrence
import Core.Core (Stmt)
import Core.Var

import Control.Lens

import qualified Backend.Lua.Postprocess as B
import qualified Backend.Lua.Emit as B
import qualified Backend.Escape as B
import Language.Lua.Syntax

import Text.Pretty.Semantic
import Text.Pretty.Note

import qualified Network.Socket as Net

import Control.Concurrent

import Repl.Display
import Errors
import Debug

-- Aghh MonadFail!
instance MonadFail L.Lua where
  fail x = error $ "MonadFail L.Lua: fail " ++ x

data ReplState = ReplState
  { resolveScope :: R.Scope
  , moduleScope  :: R.ModuleScope
  , inferScope   :: T.Env
  , emitState    :: B.TopEmitState
  , lastName     :: S.Name
  , lowerState   :: L.LowerState

  , luaState     :: L.State

  , debugMode    :: DebugMode

  , currentFiles :: [FilePath]
  , outputHandle :: Handle
  }

defaultState :: DebugMode -> IO ReplState
defaultState mode = do
  state <- L.newstate
  -- Init our default libraries
  L.runWith state L.openlibs

  pure ReplState
    { resolveScope = Bi.builtinResolve
    , moduleScope  = Bi.builtinModules
    , inferScope   = Bi.builtinEnv
    , emitState    = B.defaultEmitState
    , lastName     = S.TgName (T.pack "a") 1
    , lowerState   = L.defaultState

    , luaState     = state

    , debugMode    = mode

    , currentFiles = []
    , outputHandle = stdout
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
                    [] -> outputDoc "Usage `:load [file]`"
                    files -> loadFiles files

reloadCommand :: (MonadState ReplState m, MonadIO m) => m ()
reloadCommand = do
  files <- gets currentFiles
  case files of
    [] -> outputDoc "No files to reload"
    files -> loadFiles files

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
      let prog = [ S.LetStmt S.Public
                    [ S.Binding (S.Name "_")
                        (S.VarRef (getL var) (annotation var))
                        True (annotation var)
                    ]
                 ]
          prog :: [S.Toplevel S.Parsed]

      resolved <-
        flip evalNameyT (lastName state) $
          resolveProgram (resolveScope state) (moduleScope state) prog

      case resolved of
        Right ([ S.LetStmt _ [S.Binding _ (S.VarRef name _) _ _] ], _) ->
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
    Just parsed -> flip evalNameyT (lastName state) $ do
      let ann = annotation parsed
          prog :: [S.Toplevel S.Parsed]
          prog = [ S.LetStmt S.Public [ S.Matching (S.Wildcard ann) parsed ann ] ]

      resolved <- resolveProgram (resolveScope state) (moduleScope state) prog
      case resolved of
        Left es -> liftIO $ traverse_ (hReport (outputHandle state) files) es
        Right (resolved, _) -> do
          ~[S.LetStmt _ [ S.Matching _ desugared _ ]] <- desugarProgram resolved
          inferred <- inferExpr (inferScope state) desugared
          let (ty, es) = case inferred of
                This es -> (Nothing, es)
                That ty -> (Just ty, [])
                These es ty -> if any isError es then (Nothing, es) else (Just ty, es)
          liftIO $ traverse_ (hReport (outputHandle state) files) es
          case ty of
            Nothing -> pure ()
            Just t ->
              liftIO $ hPutDoc handle (string input <+> colon <+> displayType t)

execString :: (MonadState ReplState m, MonadIO m)
           => SourceName -> T.Text
           -> m Bool
execString name line = do
  state <- get
  core <- flip evalNameyT (lastName state) $ parseCore state parseRepl' name line
  case core of
    Nothing -> pure False
    Just (vs, prog, core, state') -> do
      let (emit', luaExpr, luaSyntax) = emitCore state' core
      (ok, res) <- liftIO $ do
        dump (debugMode state') prog core core luaExpr (inferScope state) (inferScope state')

        L.runWith (luaState state') $ do
          L.OK <- L.dostring "-- time out hook\nlocal function f() error('Timed out!', 3) end; debug.sethook(f, '', 1e6)"
          L.OK <- L.loadbuffer luaSyntax ('=':name)
          res <- L.try $ L.call 0 L.multret

          case res of
            Right () -> do
              vs' <- for vs $ \(v, _) -> do
                let Just (_, vs) = VarMap.lookup v (emit' ^. B.topVars)
                repr <- traverse (valueRepr . evalExpr . B.unsimple) vs
                let CoVar id nam _ = v
                    var = S.TgName nam id
                case inferScope state' ^. T.names . at var of
                  Just ty -> pure (Just (pretty v <+> colon <+> nest 2 (displayType ty <+> equals </> hsep (map pretty repr))))
                  Nothing -> pure Nothing

              pure (True, vsep (catMaybes vs'))
            Left (L.Exception msg) -> pure (False, string msg)

      put state' { emitState = emit' }
      outputDoc res
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

isError :: Note a b => a -> Bool
isError x = diagnosticKind x == ErrorMessage

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

parseCore :: (MonadNamey m, MonadIO m)
          => ReplState
          -> Parser (Either [S.Toplevel S.Parsed] (S.Expr S.Parsed)) -> SourceName -> T.Text
          -> m (Maybe ([(CoVar, C.Type CoVar)]
                      , [S.Toplevel S.Typed]
                      , [Stmt CoVar]
                      , ReplState))
parseCore state parser name input = do
  let files :: [(String, T.Text)]
      files = [(name, input)]
      (parsed, parseMsg) = runParser name (L.fromStrict input) parser
  liftIO $ traverse_ (hReport (outputHandle state) files) parseMsg

  case parsed of
    Nothing -> pure Nothing
    Just parsed -> do
      let parsed' = case parsed of
                      Left s -> s
                      Right e -> [S.LetStmt S.Public [S.Binding (S.Name "_") e True (annotation e)]]

      let rScope = resolveScope state
          lEnv = lowerState state
      let cont modScope' resolved prog env' = do
            verifyV <- genName
            let es = case runVerify env' verifyV (verifyProgram prog) of
                       Left es -> toList es
                       Right () -> []
            liftIO $ traverse_ (hReport (outputHandle state) files) es
            if any isError es
               then pure Nothing
               else do
                 let (var, tys) = R.extractToplevels parsed'
                     (var', tys') = R.extractToplevels resolved

                 (lEnv', lower) <- L.runLowerWithEnv lEnv (L.lowerProgEnv prog)
                 lower <- killNewtypePass lower
                 lastG <- genName
                 case lower of
                   [] -> error "lower returned no statements for the repl"
                   _ -> pure ()
                 pure $ Just ( case last lower of
                                 (C.StmtLet (C.One (v, t, _))) -> [(v, t)]
                                 (C.StmtLet (C.Many vs)) -> map (\(v, t, _) -> (v, t)) vs
                                 _ -> []
                             , prog
                             , lower
                             , state { resolveScope = rScope { R.varScope = R.insertN' (R.varScope rScope) (zip var var')
                                                             , R.tyScope  = R.insertN' (R.tyScope rScope)  (zip tys tys')
                                                             }
                                     , moduleScope = modScope'
                                     , inferScope = env'
                                     , lowerState = lEnv'
                                     , lastName = lastG })
      resolved <- resolveProgram rScope (moduleScope state) parsed'
      case resolved of
        Left es -> liftIO $ traverse_ (hReport (outputHandle state) files) es $> Nothing
        Right (resolved, modScope') -> do
          desugared <- desugarProgram resolved
          inferred <- inferProgram (inferScope state) desugared
          case inferred of
            This es -> liftIO $ traverse_ (hReport (outputHandle state) files) es $> Nothing
            That (prog, env') -> cont modScope' resolved prog env'
            These es (prog, env') -> do
              liftIO $ traverse_ (hReport (outputHandle state) files) es
              if any isError es
                 then pure Nothing
                 else cont modScope' resolved prog env'

emitCore :: ReplState -> [Stmt CoVar] -> (B.TopEmitState, LuaStmt, Bs.ByteString)
emitCore state core =
  let core' = patchupUsage . snd . tagOccurStmt (const occursSet) OccursVar $ core
      (luaStmt, emit') = uncurry B.addBuiltins $ runState (B.emitStmt core') (emitState state)
      luaExpr = LuaDo . map (patchupLua emit') . toList $ luaStmt
      luaSyntax = T.encodeUtf8 . display . uncommentDoc . renderPretty 0.8 100 . pretty $ luaExpr
  in (emit', luaExpr, luaSyntax)


-- | Reset the environment and load a series of files from environment
loadFiles :: (MonadState ReplState m, MonadIO m) => [FilePath] -> m ()
loadFiles files = do
  -- Reset the state
  dmode <- gets debugMode
  handle <- gets outputHandle
  state' <- liftIO (defaultState dmode)
  put state' { currentFiles = files, outputHandle = handle }

  -- Load each file in turn
  _ <- foldlM (go (length files)) True (zip [(1::Int)..] files)
  pure ()
  where
    go _ False _ = pure False
    go n True (i, file) = do
      outputDoc $ "Loading [" <> shown i <> "/" <> shown n <> "]:" <+> verbatim file

      contents <- liftIO . try . T.readFile $ file
      case contents of
        Right contents -> execFile file contents
        Left (_ :: IOException) -> outputDoc ("Cannot open" <+> verbatim file) $> False

execFile :: (MonadState ReplState m, MonadIO m)
         => SourceName -> T.Text
         -> m Bool
execFile name line = do
  state <- get
  core <- flip evalNameyT (lastName state) $ parseCore state (Left <$> parseTops) name line
  case core of
    Nothing -> pure False
    Just (_, prog, core, state') -> do
      let (emit', luaExpr, luaSyntax) = emitCore state' core
      ok <- liftIO $ do
        res <- L.runWith (luaState state') $ do
          code <- L.dostring luaSyntax

          liftIO $ dump (debugMode state') prog core core luaExpr (inferScope state) (inferScope state')
          case code of
            L.OK -> pure (Right ())
            _ -> do
              val <- valueRepr (pure ())
              L.pop 1
              case val of
                String str -> pure (Left (text str <+> parens (shown code)))
                _ -> pure (Left (keyword "Error:" <+> pretty val))

        case res of
          Left err -> hPutDoc (outputHandle state) err $> False
          Right () -> pure True

      put state' { emitState = emit' }
      pure ok

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

repl :: Int -> DebugMode -> IO ()
repl port mode = replFrom port mode []

replFrom :: Int -> DebugMode -> [FilePath] -> IO ()
replFrom port mode files = do
  state <- execStateT (loadFiles files) =<< defaultState mode
  hSetBuffering stdout LineBuffering

  ready <- newEmptyMVar
  tid <-
    if port /= 0
       then forkIO $ startServer ready port state
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
