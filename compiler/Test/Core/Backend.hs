module Test.Core.Backend (tests) where

import Test.Tasty
import Test.Util

import Control.Monad.Gen

import qualified Data.Text.Lazy as L
import qualified Data.Text as T

import Parser.Wrapper (ParseResult(..), runParser)
import Parser

import Syntax.Resolve (resolveProgram)
import Types.Infer (inferProgram, builtinsEnv)
import qualified Syntax.Resolve.Scope as RS
import Syntax.Desugar (desugarProgram)

import Core.Simplify
import Core.Occurrence (tagOccursVar)
import Core.Lower
import Backend.Compile

import Syntax.Pretty()
import Pretty

result :: String -> T.Text -> String
result file contents = runGen $ do
  let POK _ parsed = runParser file (L.fromStrict contents) parseInput
  Right (resolved, _) <- resolveProgram RS.builtinScope RS.emptyModules parsed
  desugared <- desugarProgram resolved
  Right (inferred, env) <- inferProgram builtinsEnv desugared
  lower <- runLowerT (lowerProg inferred)
  optm <- optimise lower
  pure . display . renderPretty 0.8 120 . (<##>empty)
        . pretty . compileProgram env $ tagOccursVar optm

tests :: IO TestTree
tests = testGroup "Tests.Core.Backend" <$> goldenDirOn result (++".lua") "tests/lua/" ".ml"
