module Test.Frontend.Amc (tests) where

import Test.Tasty
import Test.Cram

tests :: IO TestTree
tests = testGroup "amc" <$> cramDir "tests/amc"
