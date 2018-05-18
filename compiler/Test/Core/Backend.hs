module Test.Core.Backend (tests) where

import Test.Tasty
import Test.Util

import Control.Monad.Gen

import qualified Data.Text.Lazy as L
import qualified Data.Text as T

import Parser.Wrapper (runParser)
import Parser

import Syntax.Resolve (resolveProgram)
import Types.Infer (inferProgram, builtinsEnv)
import qualified Syntax.Resolve.Scope as RS
import Syntax.Desugar (desugarProgram)

import Core.Simplify
import Core.Lower
import Backend.Lua

import Syntax.Pretty()
import Text.Pretty.Semantic

result :: String -> T.Text -> T.Text
result file contents = runGen $ do
  let (Just parsed, _) = runParser file (L.fromStrict contents) parseInput
  Right (resolved, _) <- resolveProgram RS.builtinScope RS.emptyModules parsed
  desugared <- desugarProgram resolved
  Right (inferred, env) <- inferProgram builtinsEnv desugared
  lower <- runLowerT (lowerProg inferred)
  optm <- optimise lower
  pure . display . uncommentDoc . renderPretty 0.8 120 . (<##>empty)
       . pretty . compileProgram env $ optm

tests :: IO TestTree
tests = testGroup "Tests.Core.Backend" <$> goldenDirOn result (++".lua") "tests/lua/" ".ml"
