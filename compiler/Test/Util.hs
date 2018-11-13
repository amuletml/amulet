{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}
module Test.Util
  ( displayPlain
  , displayPlainVerbose
  , hedgehog
  , module Test.Golden

  , requireJust, requireRight, requireThat, requireThese

  , toEither
  ) where

import Hedgehog
import Hedgehog.Internal.Property (unGroupName, unPropertyName)

import Test.Tasty.Hedgehog
import Test.Golden
import Test.Tasty

import qualified Data.Text as T
import Data.These

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

requireJust :: N.Note a Style => String -> T.Text -> (Maybe b, [a]) -> b
requireJust _ _ (Just x, _) = x
requireJust f c (Nothing, es) = err f c es

requireRight :: N.Note a Style  => String -> T.Text -> Either [a] b -> b
requireRight _ _ (Right x) = x
requireRight f c (Left es) = err f c es

requireThat :: N.Note a Style => String -> T.Text -> These [a] b -> b
requireThat _ _ (That x) = x
requireThat _ _ (These [] x) = x
requireThat f c (These es _) = err f c es
requireThat f c (This es) = err f c es

requireThese :: N.Note a Style => String -> T.Text -> These [a] b -> (b, [a])
requireThese _ _ (That x) = (x, [])
requireThese _ _ (These y x) = (x, y)
requireThese f c (This es) = err f c es

toEither :: These [a] b -> Either [a] b
toEither (This e) = Left e
toEither (These [] x) = Right x
toEither (These e _) = Left e
toEither (That x) = Right x

err :: N.Note a Style => String -> T.Text -> [a] -> b
err f c = error . T.unpack . displayPlain . vsep . map (N.format (N.fileSpans [(f, c)] N.defaultHighlight))
