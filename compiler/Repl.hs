{-# LANGUAGE OverloadedStrings, FlexibleContexts, ScopedTypeVariables #-}

module Repl (repl) where

import Control.Monad.State.Strict
import Control.Exception

import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.IO as T
import qualified Data.Text as T

import qualified Data.Map.Strict as Map
import qualified Data.VarMap as VarMap
import qualified Data.Set as Set
import Data.Traversable
import Data.Bifunctor
import Data.Foldable
import Data.Position
import Data.Spanned
import Data.Functor
import Data.Triple
import Data.These
import Data.Maybe
import Data.List

import qualified Foreign.Lua.Api.Types as L
import qualified Foreign.Lua as L

import System.Console.Haskeline hiding (display)
import System.IO

import qualified Syntax.Resolve.Scope as R
import qualified Syntax.Resolve.Toplevel as R
import Syntax.Resolve (resolveProgram)
import Syntax.Desugar (desugarProgram)
import Syntax.Verify
import Syntax.Pretty (displayType)
import Syntax.Types (constructors, toMap)
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
import Core.Lower (runLowerWithCtors, lowerProg, lowerType)
import Core.Builtin (vLAZY, vForce, vOpApp)
import Core.Core (Stmt)
import Core.Occurrence
import Core.Free
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

data ReplState = ReplState
  { resolveScope :: R.Scope
  , moduleScope  :: R.ModuleScope
  , inferScope   :: T.Env
  , emitState    :: B.TopEmitState
  , lastName     :: S.Name

  , luaState     :: L.LuaState

  , debugMode    :: DebugMode
  }

defaultState :: DebugMode -> IO ReplState
defaultState mode = do
  state <- L.newstate

  let preamble = T.unpack . display . uncommentDoc . renderPretty 0.8 100 . pretty
                . LuaDo . map (patchupLua B.defaultEmitState . B.genOperator . fst)
                . ([ (vLAZY, undefined)
                   , (vForce, undefined)
                   , (vOpApp, undefined)
                   ]++)
                $ VarMap.toList B.ops

  -- Init our default libraries and operator functions
  L.runLuaWith state $ do
    L.openlibs
    L.OK <- L.dostring preamble
    L.OK <- L.dostring "__builtin_unit = { __tag = '__builtin_unit' }"
    pure ()

  pure ReplState
    { resolveScope = R.builtinScope
    , moduleScope  = R.emptyModules
    , inferScope   = I.builtinsEnv
    , emitState    = B.defaultEmitState
    , lastName     = S.TgName (T.pack "a") 1

    , luaState     = state

    , debugMode    = mode
    }

runRepl :: InputT (StateT ReplState IO) ()
runRepl = do
  line <- getInputLine "> "
  case line of
    Nothing -> pure ()
    Just "" -> runRepl
    Just (':':cmd) -> uncurry execCommand . second (dropWhile (== ' ') . dropWhileEnd (== ' ')) . span (/=' ') $ cmd
    Just line -> getInput line False >>= (lift . execString parseRepl' "=stdin" . T.pack) >> runRepl

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
    execCommand "quit" _    = pure ()
    execCommand "q"    _    = pure ()
    execCommand "l"    file = lift (loadFile file) >> runRepl
    execCommand "load" file = lift (loadFile file) >> runRepl
    execCommand cmd    _    = liftIO (putDoc ("Unknown command" <+> verbatim cmd)) >> runRepl

    loadFile "" = liftIO . putDoc $ "Usage `:load [file]`"
    loadFile file = do
      contents <- liftIO . try . T.readFile $ file
      case contents of
        Right contents -> execString (Left <$> parseTops) file contents
        Left (_ :: IOException) -> liftIO . putDoc $ "Cannot open" <+> verbatim file

    execString :: Parser (Either [S.Toplevel S.Parsed] (S.Expr S.Parsed)) -> SourceName -> T.Text
               -> StateT ReplState IO ()
    execString parser name line = do
      state <- get
      core <- flip evalNameyT (lastName state) $ parseCore state parser name line
      case core of
        Nothing -> pure ()
        Just (vs, prog, core, state') -> do
          let core' = patchupUsage . tagFreeSet . tagOccursVar $ core
              (luaStmt, emit') = runState (B.emitStmt core') (emitState state')
              luaExpr = LuaDo . map (patchupLua emit') . toList $ luaStmt
              luaSyntax = T.unpack . display . uncommentDoc . renderPretty 0.8 100 . pretty $ luaExpr


          liftIO $ do
            dump (debugMode state') prog core core luaExpr (inferScope state) (inferScope state')

            res <- L.runLuaWith (luaState state') $ do
              code <- L.dostring luaSyntax

              case code of
                L.OK -> do
                  vs' <- for vs $ \(v, _) -> do
                    let Just vs = VarMap.lookup v . B.topVars $ emit'
                    repr <- traverse (valueRepr . L.getglobal . T.unpack . \(LuaName n) -> n) vs
                    let CoVar id nam _ = v
                        var = S.TgName nam id
                    case inferScope state' ^. T.names . at var of
                      Just ty -> pure (Just (pretty v <+> colon <+> nest 2 (displayType ty <+> equals </> hsep (map pretty repr))))
                      Nothing -> pure Nothing

                  pure (vsep (catMaybes vs'))
                _ -> do
                  msg <- T.decodeLatin1 <$> L.tostring L.stackTop
                  L.pop 1

                  pure (text msg <+> parens (shown code))

            hFlush stdout
            hFlush stderr

            putDoc res

          put state' { emitState = emit' }


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
          let cont modScope' resolved prog env' =
                let es = case runVerify (verifyProgram prog) of
                           Left es -> toList es
                           Right () -> []
                 in do
                  liftIO $ traverse_ (`reportS`files) es
                  if any isError es
                     then pure Nothing
                     else do
                       let (var, tys) = R.extractToplevels parsed'
                           (var', tys') = R.extractToplevels resolved
                           ctors = fmap lowerType (Map.restrictKeys (env' ^. T.names . to toMap)
                                                    (Set.mapMonotonic S.unTvName (env' ^. constructors)))

                       lower <- runLowerWithCtors ctors (lowerProg prog)
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
  | Just _ <- B.getEscaped v (B.topEscape s) :: Maybe CoVar
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

repl :: DebugMode -> IO ()
repl mode = defaultState mode >>= evalStateT (runInputT defaultSettings runRepl)
