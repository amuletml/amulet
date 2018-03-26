{-# LANGUAGE TemplateHaskell, OverloadedStrings, FlexibleContexts #-}
module Test.Types.Infer where

import Control.Monad

import Hedgehog

import Test.Types.Util
import Test.Syntax.Gen

import Pretty (pretty)

prop_wellTyped :: Property
prop_wellTyped = property $ do
  ty <- forAll genType
  ex <- forAll (genCorrectExpr ty)
  case inferExpr ex of
    Left e -> footnote (show (pretty e)) *> failure
    Right ty' -> guard (ty `equivalent` ty')

prop_generatedChecks :: Property
prop_generatedChecks = property $ do
  ty <- forAll genType
  ex <- forAll (genCorrectExpr ty)
  case checkExpr ex ty of
    Left e -> footnote (show (pretty e)) *> failure
    Right () -> pure ()

prop_findsErrors :: Property
prop_findsErrors = property $ do
  ex <- forAll genBadExpr
  case inferExpr ex of
    Left _ -> success
    Right t -> footnote ("Found type " ++ show (pretty t) ++ " for " ++ show (pretty ex)) *> failure

tests :: IO Bool
tests = checkParallel $$(discover)
