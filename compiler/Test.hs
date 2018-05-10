module Main where

import Test.Tasty

import Test.Util
import qualified Test.Types.Infer as Types
import qualified Test.Core.Lint as CLint
import qualified Test.Parser.Lexer as Lexer
import qualified Test.Parser.Parser as Parser
import qualified Test.Syntax.Resolve as Resolve

tests :: TestTree
tests = testGroup "Tests" [ hedgehog Types.tests
                          , CLint.tests
                          , Lexer.tests
                          , Parser.tests
                          , Resolve.tests
                          ]

main :: IO ()
main = defaultMain tests
