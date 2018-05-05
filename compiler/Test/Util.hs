module Test.Util where

import Hedgehog
import Hedgehog.Internal.Property (unGroupName, unPropertyName)
import Test.Tasty.Hedgehog
import Test.Tasty

hedgehog :: Group -> TestTree
hedgehog Group { groupName = n, groupProperties = ps }
  = testGroup (unGroupName n) (map (\(n, p) -> testProperty (unPropertyName n) p) ps)
