module Main where

import Test.Tasty.Ingredients.Basic
import Test.Tasty.Runners.AntXML
import Test.Tasty.Ingredients
import Test.Tasty.Reporter
import Test.Tasty.Rerun
import Test.Tasty

-- import qualified Test.Types.Unify as Solver
import qualified Test.Core.Lint as Lint
import qualified Test.Parser.Lexer as Lexer
import qualified Test.Parser.Parser as Parser
import qualified Test.Syntax.Resolve as Resolve
import qualified Test.Syntax.Verify as Verify
import qualified Test.Types.Check as TypesC
import qualified Test.Core.Backend as Backend
import qualified Test.Lua.Parser as LParser
import qualified Test.Frontend.Amc as Amc

tests :: IO TestTree
tests = testGroup "Tests" <$> sequence
  -- [ pure Solver.tests Has a tendency to timeout. TODO: Get better CI
  -- machines

  [ Amc.tests
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
main = tests >>= defaultMainWithIngredients ingredients where
  ingredients =
    [ rerunning
      [ listingTests
      , boringReporter `composeReporters` antXMLRunner ]
    ]
