{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Repl (repl) where

import Control.Monad.State.Strict
import Control.Arrow (first)
import Control.Monad.Gen

import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as L
import qualified Data.Text as T

import qualified Data.VarMap as VarMap
import Data.Traversable
import Data.Foldable
import Data.Spanned
import Data.Functor
import Data.List

import qualified Foreign.Lua.Types.Error as L
import qualified Foreign.Lua.Api.Types as L
import qualified Foreign.Lua as L

import System.Console.Haskeline hiding (display)
import System.IO

import qualified Syntax.Resolve.Scope as R
import qualified Syntax.Resolve.Toplevel as R
import Syntax.Resolve (resolveProgram)
import Syntax.Desugar (desugarProgram)
import qualified Syntax as S

import qualified Control.Monad.Infer as T
import qualified Types.Infer as I
import Types.Infer (inferProgram)

import Parser.Wrapper (runParser)
import Parser (parseRepl)
import Parser.Token
import Parser.Error

import qualified Core.Core as C
import Core.Lower (runLowerT, lowerProg)
import Core.Optimise.Reduce
import Core.Occurrence
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
               $ VarMap.toList B.ops

  -- Init our default libraries and operator functions
  L.runLuaWith state $ do
    L.openlibs
    L.OK <- L.dostring preamble
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
                  vs' <- for vs $ \(v, _) -> displayLuaAsAmulet (L.getglobal (T.unpack (B.getVar v escape'))) $ \t -> do
                    let CoVar id nam _ = v
                        var = S.TgName nam id
                    case inferScope state' ^. T.values . at var of
                      Just ty -> pure (pretty v <+> colon <+> nest 2 (displayType ty <+> equals </> t))
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

                  -- We don't perform any complex optimisations, but run one reduction pass in order
                  -- to get some basic commuting conversion, allowing the codegen to be more
                  -- effective.
                  lower <- reducePass <$> runLowerT (lowerProg prog)
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
patchupLua (LuaLocal vs es) = LuaAssign vs es
patchupLua x = x

displayLuaAsAmulet :: L.Lua () -> (Doc -> L.Lua a) -> L.Lua a
displayLuaAsAmulet getVal cont = do
  getVal
  t <- L.ltype L.stackTop
  cont =<< case t of
    L.TypeString ->
      sstring . dquotes . text . T.decodeLatin1
        <$> L.tostring L.stackTop
    L.TypeNumber ->
      sliteral . text . T.decodeLatin1 <$> L.tostring L.stackTop
    L.TypeNone -> error "Invalid stack index"
    L.TypeNil -> pure (keyword "()")
    L.TypeBoolean -> do
      b <- L.toboolean L.stackTop
      pure . sliteral . text $ if b then "true" else "false"
    L.TypeFunction -> pure (keyword "<function>")
    L.TypeTable -> do
      table <- L.absindex L.stackTop
      L.pushnil
      let loop :: L.Lua Bool -> L.Lua [(T.Text, Doc)]
          loop cont = do
            weDo <- cont `L.catchLuaError` (const (pure False))
            if weDo
               then do
                 k <- T.decodeLatin1 <$> L.tostring (-2)
                 v <- displayLuaAsAmulet (pure ()) pure
                 (:) (k, equals <+> v) <$> loop cont
               else pure []
      enclose (lbrace <> space) (space <> rbrace)
        . hsep . punctuate comma . map (uncurry (<+>) . first text) . sortOn fst <$> loop (L.next table)
    _ -> pure (keyword "<foreign value>")

    <* L.pop 1

repl :: DebugMode -> IO ()
repl mode = defaultState mode >>= evalStateT (runInputT defaultSettings runRepl)
