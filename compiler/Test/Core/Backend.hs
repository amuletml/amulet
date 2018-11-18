module Test.Core.Backend (tests) where

import Test.Tasty
import Test.Util

import Control.Monad.Infer

import qualified Data.Text.Lazy as L
import qualified Data.Text as T

import Parser.Wrapper (runParser)
import Parser

import Types.Infer (inferProgram)

import Syntax.Resolve (resolveProgram)
import Syntax.Desugar (desugarProgram)
import Syntax.Builtin
import Syntax.Pretty()

import Core.Simplify
import Core.Lower

import Backend.Lua

import Text.Pretty.Semantic

result :: String -> T.Text -> T.Text
result f c = fst . flip runNamey firstName $ do
  let parsed = requireJust f c $ runParser f (L.fromStrict c) parseTops
  (resolved, _) <- requireRight f c <$> resolveProgram builtinResolve builtinModules parsed
  desugared <- desugarProgram resolved
  (inferred, _) <- requireThat f c <$> inferProgram builtinEnv desugared
  lower <- runLowerT (lowerProg inferred)
  optm <- optimise lower
  pure . display . uncommentDoc . renderPretty 0.8 120 . (<##>empty)
       . pretty . compileProgram $ optm

tests :: IO TestTree
tests = testGroup "Tests.Core.Backend" <$> goldenDirOn result (++".lua") "tests/lua/" ".ml"
