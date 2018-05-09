module Main where

import Test.Tasty

import Test.Util
import qualified Test.Types.Infer as Types
import qualified Test.Core.Lint as CLint
import qualified Test.Parser.Lexer as Lexer

tests :: TestTree
tests = testGroup "Tests" [ hedgehog Types.tests
                          , CLint.tests
                          , Lexer.tests
                          ]

main :: IO ()
main = defaultMain tests
