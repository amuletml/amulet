{-# LANGUAGE OverloadedStrings #-}

{-| Reexports 'Test.Tasty.HUnit', but using prettier error printing. -}
module Test.Tasty.HUnitPretty
  ( module M
  , assertEqual, (@?=), (@=?)
  , assertEqualP, (%?=), (%=?)
  ) where

import Control.Monad

import qualified Data.Text as T

import Test.Tasty.HUnit as M hiding (assertEqual, (@?=), (@=?))

import Text.Pretty.Semantic
import Text.Show.Pretty

infix 1 @=?, @?=, %=?, %?=

-- | Asserts that the specified actual value is equal to the expected value.
-- The output message will contain the prefix, the expected value, and the
-- actual value.
--
-- If the prefix is the empty string (i.e., @\"\"@), then the prefix is omitted
-- and only the expected and actual values are output.
assertEqual
  :: (Eq a, Show a, HasCallStack)
  => String -- ^ The message prefix
  -> a      -- ^ The expected value
  -> a      -- ^ The actual value
  -> Assertion
assertEqual preface expected actual =
  unless (actual == expected) (assertFailure msg)
 where msg = (if null preface then "" else preface ++ "\n") ++
             "expected: " ++ ppShow expected ++ "\n" ++
             "but got:  " ++ ppShow actual

-- | Asserts that the specified actual value is equal to the expected value.
-- The output message will contain the prefix, the expected value, and the
-- actual value.
--
-- If the prefix is the empty string (i.e., @\"\"@), then the prefix is omitted
-- and only the expected and actual values are output.
assertEqualP
  :: (Eq a, Pretty a, HasCallStack)
  => Doc    -- ^ The message prefix
  -> a      -- ^ The expected value
  -> a      -- ^ The actual value
  -> Assertion
assertEqualP preface expected actual =
  unless (actual == expected) (assertFailure (renderBasic msg))
 where msg = preface </>
             "expected: " <> align (pretty expected) <#>
             "but got:  " <> align (pretty expected)

       renderBasic = T.unpack . display . renderPretty 0.4 100

-- | Asserts that the specified actual value is equal to the expected value
--   (with the expected value on the left-hand side).
(@=?)
  :: (Eq a, Show a, HasCallStack)
  => a -- ^ The expected value
  -> a -- ^ The actual value
  -> Assertion
expected @=? actual = assertEqual "" expected actual

-- | Asserts that the specified actual value is equal to the expected value
--   (with the actual value on the left-hand side).
(@?=)
  :: (Eq a, Show a, HasCallStack)
  => a -- ^ The actual value
  -> a -- ^ The expected value
  -> Assertion
actual @?= expected = assertEqual "" expected actual

-- | Asserts that the specified actual value is equal to the expected value
--   (with the expected value on the left-hand side).
(%=?)
  :: (Eq a, Pretty a, HasCallStack)
  => a -- ^ The expected value
  -> a -- ^ The actual value
  -> Assertion
expected %=? actual = assertEqualP "" expected actual

-- | Asserts that the specified actual value is equal to the expected value
--   (with the actual value on the left-hand side).
(%?=)
  :: (Eq a, Pretty a, HasCallStack)
  => a -- ^ The actual value
  -> a -- ^ The expected value
  -> Assertion
actual %?= expected = assertEqualP "" expected actual
