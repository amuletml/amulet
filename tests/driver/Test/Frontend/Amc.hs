module Test.Frontend.Amc (tests) where

import Test.Tasty.Cram
import Test.Tasty

tests :: IO TestTree
tests = testGroup "amc" <$> cramDir "tests/amc"
