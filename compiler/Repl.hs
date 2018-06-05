{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Repl (repl) where

import Control.Monad.State.Strict
import Control.Monad.Gen

import qualified Data.Text.Encoding as T
import qualified Data.Map.Strict as Map
import qualified Data.Text.Lazy as L
import qualified Data.Text as T

import qualified Data.VarMap as VarMap
import Data.Traversable
import Data.Foldable
import Data.Spanned
import Data.Functor

import qualified Foreign.Lua.Api.Types as L
import qualified Foreign.Lua as L

import System.Console.Haskeline hiding (display)
import System.IO

import qualified Syntax.Resolve.Scope as R
import qualified Syntax.Resolve.Toplevel as R
import Syntax.Resolve (resolveProgram)
import Syntax.Desugar (desugarProgram)
import Syntax.Pretty (displayType)
import Syntax.Types (constructors, toMap)
import qualified Syntax as S

import qualified Control.Monad.Infer as T
import qualified Types.Infer as I
import Types.Infer (inferProgram)

import Parser.Wrapper (runParser)
import Parser (parseRepl)
import Parser.Token
import Parser.Error

import qualified Core.Core as C
import Core.Lower (runLowerWithCtors, lowerProg, lowerType)
import Core.Optimise.Reduce
import Core.Occurrence
import Core.Builtin (vLAZY, vForce)
import Core.Core (Stmt)
import Core.Var

import Control.Lens

import qualified Backend.Lua.Postprocess as B
import qualified Backend.Lua.Emit as B
import qualified Backend.Escape as B
import Backend.Lua.Syntax

import Text.Pretty.Semantic

import Repl.Display
import Errors
import Debug

data ReplState = ReplState
  { resolveScope :: R.Scope
  , moduleScope  :: R.ModuleScope
  , inferScope   :: T.Env
  , escapeScope  :: B.EscapeScope
  , lastGen      :: Int

  , luaState     :: L.LuaState

  , debugMode    :: DebugMode
  }

defaultState :: DebugMode -> IO ReplState
defaultState mode = do
  state <- L.newstate

  let preamble = T.unpack . display . uncommentDoc . renderPretty 0.8 100 . pretty
                . LuaDo . map (patchupLua . B.genOperator . fst)
                . ((vLAZY, undefined):) . ((vForce, undefined):)
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
    , escapeScope  = B.escapeScope
    , lastGen      = 0

    , luaState     = state

    , debugMode    = mode
    }

runRepl :: InputT (StateT ReplState IO) ()
runRepl = do
  line <- getInputLine "> "
  case line of
    Nothing -> pure ()
    Just "" -> runRepl
    Just (':':cmd) -> execCommand cmd
    Just line -> getInput line False >>= (lift . execString) >> runRepl

  where
    getInput input empty =
      case runParser "=stdin" (L.pack input) parseRepl of
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

    execCommand "quit" = pure ()
    execCommand "q" = pure ()
    execCommand _ = runRepl

    execString line = do
      state <- get
      core <- runGenTFrom (lastGen state) $ parseCore state line
      case core of
        Nothing -> pure ()
        Just (vs, prog, core, state') -> do
          let (luaStmt, escape') = B.emitProgramWith (inferScope state') (escapeScope state') (tagOccursVar core)
              luaExpr = LuaDo . map patchupLua $ luaStmt
              luaSyntax = T.unpack . display . uncommentDoc . renderPretty 0.8 100 . pretty $ luaExpr


          liftIO $ do
            dump (debugMode state') prog core core luaExpr (inferScope state) (inferScope state')

            res <- L.runLuaWith (luaState state') $ do
              code <- L.dostring luaSyntax

              case code of
                L.OK -> do
                  vs' <- for vs $ \(v, _) -> do
                    repr <- valueRepr (L.getglobal (T.unpack (B.getVar v escape')))
                    let CoVar id nam _ = v
                        var = S.TgName nam id
                    case inferScope state' ^. T.names . at var of
                      Just ty -> pure (pretty v <+> colon <+> nest 2 (displayType ty <+> equals </> pretty repr))
                      Nothing -> error "variable not bound in infer scope?"

                  pure (vsep vs')
                _ -> do
                  msg <- T.decodeLatin1 <$> L.tostring L.stackTop
                  L.pop 1

                  pure (text msg <+> parens (shown code))

            hFlush stdout
            hFlush stderr

            putDoc res

          put state' { escapeScope = escape' }


    parseCore :: (MonadGen Int m, MonadIO m)
              => ReplState -> String
              -> m (Maybe ([(CoVar, C.Type CoVar)]
                          , [S.Toplevel S.Typed]
                          , [Stmt CoVar]
                          , ReplState))
    parseCore state input = do
      let files = [("=stdin", T.pack input)]
          (parsed, parseMsg) = runParser "=stdin" (L.pack input) parseRepl
      liftIO $ traverse_ (`reportS`files) parseMsg

      case parsed of
        Nothing -> pure Nothing
        Just parsed -> do
          let parsed' = case parsed of
                          Left s -> [s]
                          Right e -> [S.LetStmt [(S.Name "_", e, annotation e)]]

          let rScope = resolveScope state
          resolved <- resolveProgram rScope (moduleScope state) parsed'
          case resolved of
            Left es -> liftIO $ traverse_ (`reportS`files) es $> Nothing
            Right (resolved, modScope') -> do
              desugared <- desugarProgram resolved
              inferred <- inferProgram (inferScope state) desugared
              case inferred of
                Left es -> liftIO $ traverse_ (`reportS`files) es $> Nothing
                Right (prog, env') -> do
                  let (var, tys) = R.extractToplevels parsed'
                      (var', tys') = R.extractToplevels resolved
                      ctors = fmap lowerType (Map.restrictKeys (env' ^. T.names . to toMap) (env' ^. constructors))

                  -- We don't perform any complex optimisations, but run one reduction pass in order
                  -- to get some basic commuting conversion, allowing the codegen to be more
                  -- effective.
                  lower <- reducePass <$> runLowerWithCtors ctors (lowerProg prog)
                  lastG <- gen
                  pure $ Just ( case last lower of
                                  (C.StmtLet vs) -> map (\(v, t, _) -> (v, t)) vs
                                  _ -> []
                              , prog
                              , lower
                              , state { resolveScope = rScope { R.varScope = R.insertN (R.varScope rScope) (zip var var')
                                                              , R.tyScope  = R.insertN (R.tyScope rScope)  (zip tys tys')
                                                              }
                                      , moduleScope = modScope'
                                      , inferScope = env'
                                      , lastGen = lastG })


-- We convert any top-level local declarations into global ones. This
-- means they are accessible outside normal REPL invocations.
patchupLua :: LuaStmt -> LuaStmt
patchupLua (LuaLocal vs es)
  | length es < length vs = LuaAssign vs (es ++ replicate (length vs - length es) LuaNil)
  | otherwise = LuaAssign vs es
patchupLua x = x

repl :: DebugMode -> IO ()
repl mode = defaultState mode >>= evalStateT (runInputT defaultSettings runRepl)
