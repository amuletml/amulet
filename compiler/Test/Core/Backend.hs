module Test.Core.Backend (tests) where

import Test.Tasty
import Test.Util

import Control.Monad.State
import Control.Monad.Infer

import qualified Data.Text.Lazy as L
import qualified Data.Text as T
import Data.Foldable
import Data.Triple

import Parser.Wrapper (runParser)
import Parser

import Types.Infer (inferProgram)

import Syntax.Resolve (resolveProgram)
import Syntax.Desugar (desugarProgram)
import Syntax.Builtin

import Core.Occurrence
import Core.Simplify
import Core.Lower
import Core.Var
import Core.Core

import Language.Lua.Syntax
import Backend.Lua.Postprocess
import Backend.Lua.Emit
import Backend.Lua

import Text.Pretty.Semantic

result :: Bool -> String -> T.Text -> T.Text
result o f c = fst . flip runNamey firstName $ do
  let parsed = requireJust f c $ runParser f (L.fromStrict c) parseTops
  (resolved, _) <- requireRight f c <$> resolveProgram builtinResolve builtinModules parsed
  desugared <- desugarProgram resolved
  (inferred, _) <- requireThat f c <$> inferProgram builtinEnv desugared
  lower <- runLowerT (lowerProg inferred)
  compiled <-
    if o
    then compileProgram <$> optimise lower
    else pure . LuaDo . toList . fst
       . uncurry addBuiltins
       . flip runState defaultEmitState . emitStmt
       . patchupUsage . snd . tagOccurStmt (const occursSet) OccursVar
       $ lower
  pure . display . uncommentDoc . renderPretty 0.8 120 . (<##>empty)
       $ pretty compiled

tests :: IO TestTree
tests = do
  withOpt <- goldenDirOn (result True) (++".lua") "tests/lua/" ".ml"
  noOpt <- goldenDirOn (result False) (++".lua") "tests/lua_no_opt/" ".ml"
  pure $ testGroup "Tests.Core.Backend"
    [ testGroup "No optimiser" noOpt
    , testGroup "With optimiser" withOpt
    ]

patchupUsage :: IsVar a => [AnnStmt b (OccursVar a)] -> [AnnStmt b(OccursVar a)]
patchupUsage [] = []
patchupUsage (s@Foreign{}:xs) = s:patchupUsage xs
patchupUsage (s@Type{}:xs) = s:patchupUsage xs
patchupUsage (StmtLet (One v):xs) = StmtLet (One (first3 patchupVarUsage v)):patchupUsage xs
patchupUsage (StmtLet (Many v):xs) = StmtLet (Many (map (first3 patchupVarUsage) v)):patchupUsage xs

patchupVarUsage :: OccursVar a -> OccursVar a
patchupVarUsage (OccursVar v u) = OccursVar v (u <> Once)
