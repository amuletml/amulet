module Main where

import Test.Tasty

import Test.Util
import qualified Test.Types.Infer as Types
import qualified Test.Core.Lint as CLint
import qualified Test.Parser.Lexer as Lexer
import qualified Test.Parser.Parser as Parser
import qualified Test.Syntax.Resolve as Resolve
import qualified Test.Types.Check as TypesC

tests :: TestTree
tests = testGroup "Tests" [ hedgehog Types.tests
                          , CLint.tests
                          , Lexer.tests
                          , Parser.tests
                          , Resolve.tests
                          , TypesC.tests
                          ]

main :: IO ()
main = defaultMain tests
