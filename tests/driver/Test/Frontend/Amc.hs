module Test.Frontend.Amc (tests) where

import Test.Util

tests :: IO TestTree
tests = testGroup "amc" <$> cramDir "tests/amc"
