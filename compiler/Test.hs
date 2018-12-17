module Main where

import Test.Tasty.Ingredients.ConsoleReporter
import Test.Tasty.Ingredients.Basic
import Test.Tasty.Runners.AntXML
import Test.Tasty.Ingredients
import Test.Tasty

import Test.Util
import qualified Test.Types.Unify as Solver
import qualified Test.Core.Lint as Lint

import qualified Test.Parser.Lexer as Lexer
import qualified Test.Parser.Parser as Parser
import qualified Test.Syntax.Resolve as Resolve
import qualified Test.Syntax.Verify as Verify
import qualified Test.Types.Check as TypesC
import qualified Test.Core.Backend as Backend
import qualified Test.Lua.Parser as LParser

tests :: IO TestTree
tests = testGroup "Tests" <$> sequence
  [ pure (hedgehog Solver.tests)

  , Lint.tests
  , Lexer.tests
  , Parser.tests
  , Resolve.tests
  , TypesC.tests
  , Verify.tests
  , Backend.tests
  , LParser.tests
  ]

main :: IO ()
main = tests >>= defaultMainWithIngredients [ listingTests, consoleTestReporter `composeReporters` antXMLRunner ]
