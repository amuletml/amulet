{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}
module Test.Util
  ( displayPlain
  , displayPlainVerbose
  , hedgehog
  , module Test.Golden
  ) where

import Hedgehog
import Hedgehog.Internal.Property (unGroupName, unPropertyName)

import Test.Tasty.Hedgehog
import Test.Golden
import Test.Tasty

import qualified Data.Text as T

import qualified Text.Pretty.Note as N
import Text.Pretty.Semantic

displayPlain, displayPlainVerbose :: N.NoteDoc Style -> T.Text
displayPlain
  = display
  . fmap (either N.toAnsi toAnsi)
  . filterSimpleDoc (either (const True) uncommentFilter)
  . renderPretty 0.4 100
  . (<>line)
displayPlainVerbose
  = display
  . fmap (either N.toAnsi toAnsi)
  . renderPretty 0.4 100
  . (<>line)

hedgehog :: Group -> TestTree
hedgehog Group { groupName = n, groupProperties = ps }
  = testGroup (unGroupName n) (map (\(n, p) -> testProperty (unPropertyName n) p) ps)
