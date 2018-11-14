{-# LANGUAGE OverloadedStrings, FlexibleContexts, ScopedTypeVariables, ViewPatterns, TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Repl
  ( repl
  , replFrom
  ) where

import Control.Monad.State.Strict
import Control.Monad.Fail
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

import System.Console.Haskeline hiding (display)
import System.IO

import qualified Syntax.Resolve.Scope as R
import qualified Syntax.Resolve.Toplevel as R
import Syntax.Resolve (resolveProgram)
import Syntax.Desugar (desugarProgram)
import Syntax.Verify
import Syntax.Pretty (displayType)
import qualified Syntax.Var as S
import qualified Syntax as S

import qualified Control.Monad.Infer as T
import Control.Monad.Namey

import qualified Types.Infer as I
import Types.Infer (inferProgram)

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
  }

defaultState :: DebugMode -> IO ReplState
defaultState mode = do
  state <- L.newstate

  let preamble = display . uncommentDoc . renderPretty 0.8 100 . pretty
                . LuaDo . foldMap (map (patchupLua B.defaultEmitState) . snd . B.genBuiltin . fst)
                $ VarMap.toList B.builtinVars

  -- Init our default libraries and operator functions
  L.runWith state $ do
    L.openlibs
    L.OK <- L.dostring (T.encodeUtf8 preamble)
    pure ()

  pure ReplState
    { resolveScope = R.builtinScope
    , moduleScope  = R.emptyModules
    , inferScope   = I.builtinsEnv
    , emitState    = B.defaultEmitState
    , lastName     = S.TgName (T.pack "a") 1
    , lowerState   = L.defaultState

    , luaState     = state

    , debugMode    = mode

    , currentFiles = []
    }

runRepl :: InputT (StateT ReplState IO) ()
runRepl = do
  line <- getInputLine "> "
  case line of
    Nothing -> pure ()
    Just "" -> runRepl
    Just (':':cmd) -> uncurry execCommand . span (/=' ') $ cmd
    Just line -> getInput line False >>= (lift . execString "=stdin" . T.pack) >> runRepl

  where
    parseRepl' :: Parser (Either [S.Toplevel S.Parsed] (S.Expr S.Parsed))
    parseRepl' = first pure <$> parseRepl

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

    execCommand :: String -> String -> InputT (StateT ReplState IO) ()
    execCommand "quit" _ = pure ()
    execCommand "q"    _ = pure ()

    execCommand "l"    arg = lift (loadCommand arg) >> runRepl
    execCommand "load" arg = lift (loadCommand arg) >> runRepl

    execCommand "r"      _ = lift reloadCommand >> runRepl
    execCommand "reload" _ = lift reloadCommand >> runRepl

    execCommand "t" arg = lift (typeCommand arg) >> runRepl
    execCommand "type" arg = lift (typeCommand arg) >> runRepl

    execCommand cmd _ = liftIO (putDoc ("Unknown command" <+> verbatim cmd)) >> runRepl

    -- | Split a string into arguments
    parseArgs :: String -> [String]
    parseArgs xs = let (ys, y) = parseArgs' xs
                   in maybe ys (:ys) y

    parseArgs' "" = ([], Nothing)
    parseArgs' (' ':xs) = (parseArgs xs, Nothing)
    parseArgs' ('\\':x:xs) = Just . (x:) . fromMaybe [] <$> parseArgs' xs
    parseArgs' (x:xs) = Just . (x:) . fromMaybe [] <$> parseArgs' xs

    loadCommand arg = case parseArgs arg of
                        [] -> liftIO (putDoc "Usage `:load [file]`")
                        files -> loadFiles files

    reloadCommand = do
      files <- gets currentFiles
      case files of
        [] -> liftIO (putDoc "No files to reload")
        files -> loadFiles files

    typeCommand (dropWhile isSpace -> line) = do
      state <- get
      core <- flip evalNameyT (lastName state) $ parseCore state parseReplExpr "<interactive>" (T.pack line)
      case core of
        Just ((v, _ ):_, _, _, state') -> do
          let CoVar id nam _ = v
              var = S.TgName nam id
          case inferScope state' ^. T.names . at var of
            Just ty -> liftIO $ putDoc (string line <+> colon <+> displayType ty)
            _ -> error "what"
        _ -> pure ()

    execString :: SourceName -> T.Text
               -> StateT ReplState IO Bool
    execString name line = do
      state <- get
      core <- flip evalNameyT (lastName state) $ parseCore state parseRepl' name line
      case core of
        Nothing -> pure False
        Just (vs, prog, core, state') -> do
          let (emit', luaExpr, luaSyntax) = emitCore state' core
          ok <- liftIO $ do
            dump (debugMode state') prog core core luaExpr (inferScope state) (inferScope state')

            (ok, res) <- L.runWith (luaState state') $ do
              _ <- L.dostring "-- time out hook\nlocal function f() error('Timed out!', 3) end; debug.sethook(f, '', 1e6)"
              _ <- L.loadbuffer luaSyntax "<interactive>"
              code <- L.pcall 0 L.multret Nothing

              case code of
                L.OK -> do
                  vs' <- for vs $ \(v, _) -> do
                    let Just vs = VarMap.lookup v (emit' ^. B.topVars)
                    repr <- traverse (valueRepr . L.getglobal . T.unpack . \(LuaName n) -> n) vs
                    let CoVar id nam _ = v
                        var = S.TgName nam id
                    case inferScope state' ^. T.names . at var of
                      Just ty -> pure (Just (pretty v <+> colon <+> nest 2 (displayType ty <+> equals </> hsep (map pretty repr))))
                      Nothing -> pure Nothing

                  pure (True, vsep (catMaybes vs'))
                _ -> do
                  val <- valueRepr (pure ())
                  L.pop 1
                  case val of
                    String str -> pure (False, text str <+> parens (shown code))
                    _ -> pure (False, keyword "Error:" <+> pretty val)

            hFlush stdout
            hFlush stderr

            putDoc res
            pure ok

          put state' { emitState = emit' }
          pure ok

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
  liftIO $ traverse_ (`reportS`files) parseMsg

  case parsed of
    Nothing -> pure Nothing
    Just parsed -> do
      let parsed' = case parsed of
                      Left s -> s
                      Right e -> [S.LetStmt [S.Binding (S.Name "_") e (annotation e)]]

      let rScope = resolveScope state
          lEnv = lowerState state
      let cont modScope' resolved prog env' = do
            verifyV <- genName
            let es = case runVerify env' verifyV (verifyProgram prog) of
                       Left es -> toList es
                       Right () -> []
            liftIO $ traverse_ (`reportS`files) es
            if any isError es
               then pure Nothing
               else do
                 let (var, tys) = R.extractToplevels parsed'
                     (var', tys') = R.extractToplevels resolved

                 (lEnv', lower) <- L.runLowerWithEnv lEnv (L.lowerProgEnv prog)
                 lower' <- killNewtypePass lower
                 lastG <- genName
                 case lower' of
                   [] -> error "lower returned no statements for the repl"
                   _ -> pure ()
                 pure $ Just ( case last lower' of
                                 (C.StmtLet (C.One (v, t, _))) -> [(v, t)]
                                 (C.StmtLet (C.Many vs)) -> map (\(v, t, _) -> (v, t)) vs
                                 _ -> []
                             , prog
                             , lower'
                             , state { resolveScope = rScope { R.varScope = R.insertN' (R.varScope rScope) (zip var var')
                                                             , R.tyScope  = R.insertN' (R.tyScope rScope)  (zip tys tys')
                                                             }
                                     , moduleScope = modScope'
                                     , inferScope = env'
                                     , lowerState = lEnv'
                                     , lastName = lastG })
      resolved <- resolveProgram rScope (moduleScope state) parsed'
      case resolved of
        Left es -> liftIO $ traverse_ (`reportS`files) es $> Nothing
        Right (resolved, modScope') -> do
          desugared <- desugarProgram resolved
          inferred <- inferProgram (inferScope state) desugared
          case inferred of
            This es -> liftIO $ traverse_ (`reportS`files) es $> Nothing
            That (prog, env') -> cont modScope' resolved prog env'
            These es (prog, env') -> do
              liftIO $ traverse_ (`reportS`files) es
              if any isError es
                 then pure Nothing
                 else cont modScope' resolved prog env'

emitCore :: ReplState -> [Stmt CoVar] -> (B.TopEmitState, LuaStmt, Bs.ByteString)
emitCore state core =
  let core' = patchupUsage . snd . tagOccurStmt (const occursSet) OccursVar $ core
      (luaStmt, emit') = runState (B.emitStmt core') (emitState state)
      luaExpr = LuaDo . map (patchupLua emit') . toList $ luaStmt
      luaSyntax = T.encodeUtf8 . display . uncommentDoc . renderPretty 0.8 100 . pretty $ luaExpr
  in (emit', luaExpr, luaSyntax)


-- | Reset the environment and load a series of files from environment
loadFiles :: [FilePath] -> StateT ReplState IO ()
loadFiles files = do
  -- Reset the state
  dmode <- gets debugMode
  state' <- liftIO (defaultState dmode)
  put state' { currentFiles = files }

  -- Load each file in turn
  _ <- foldlM (go (length files)) True (zip [(1::Int)..] files)
  pure ()
  where
    go _ False _ = pure False
    go n True (i, file) = do
      liftIO . putDoc $ "Loading [" <> shown i <> "/" <> shown n <> "]:" <+> verbatim file

      contents <- liftIO . try . T.readFile $ file
      case contents of
        Right contents -> execFile file contents
        Left (_ :: IOException) -> liftIO (putDoc ("Cannot open" <+> verbatim file)) $> False

execFile :: SourceName -> T.Text
         -> StateT ReplState IO Bool
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

        hFlush stdout
        hFlush stderr

        case res of
          Left err -> putDoc err $> False
          Right () -> pure True

      put state' { emitState = emit' }
      pure ok


repl :: DebugMode -> IO ()
repl mode = defaultState mode >>= evalStateT (runInputT defaultSettings runRepl)

replFrom :: DebugMode -> [FilePath] -> IO ()
replFrom mode files = defaultState mode >>= evalStateT (loadFiles files >> runInputT defaultSettings runRepl)
