module Main where

import Test.Tasty

import Test.Util
import qualified Test.Types.Infer as Types
import qualified Test.Core.Lint as CLint

tests :: TestTree
tests = testGroup "Tests" [ hedgehog Types.tests
                          , CLint.tests ]

main :: IO ()
main = defaultMain tests
