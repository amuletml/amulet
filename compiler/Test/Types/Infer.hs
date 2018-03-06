{-# LANGUAGE PackageImports, TemplateHaskell, OverloadedStrings #-}
module Test.Types.Infer where

import "amuletml" Control.Monad.Infer (values, Constraint(..), SomeReason(..))

import "amuletml" Types.Infer (inferProgram, closeOver)
import "amuletml" Types.Unify (solve)

import "amuletml" Syntax.Raise
import "amuletml" Syntax (Var(..), Toplevel(..), Type(..), Typed, Expr(Ascription), unTvName)

import "amuletml" Data.Spanned
import "amuletml" Data.Span

import qualified Control.Monad.Gen as MonadGen
import Control.Monad
import Control.Applicative
import Control.Lens

import qualified Hedgehog.Range as Range
import qualified Hedgehog.Gen as Gen
import Hedgehog

import Test.Syntax.Gen

import Debug.Trace

import Pretty (pretty)

prop_welltyped :: Property
prop_welltyped = property $ do
  ty <- forAll genType
  ex <- forAll (genCorrectExpr ty)
  let var = TgInternal "main"
      tys = MonadGen.runGen (inferProgram [LetStmt [(var, ex, internal)]])
  case tys of
    Left e -> footnote (show (pretty e)) *> failure
    Right (_, x) -> eqTypes ty (x ^. values . at var . non undefined)

prop_generatedChecks :: Property
prop_generatedChecks = property $ do
  ty <- forAll genType
  ex <- forAll (genCorrectExpr ty)
  let var = TgInternal "main"
      tys = MonadGen.runGen (inferProgram [LetStmt [(var, (Ascription ex (raiseT unTvName ty) internal), internal)]])
  case tys of
    Left e -> footnote (show (pretty e)) *> failure
    Right (_, x) -> eqTypes ty (x ^. values . at var . non undefined)

prop_findsErrors :: Property
prop_findsErrors = property $ do
  ex <- forAll genBadExpr
  let var = TgInternal "main"
      tys = MonadGen.runGen (inferProgram [LetStmt [(var, ex, internal)]])
  case tys of
    Left e -> success
    Right (_, x) -> footnote ("Found type " ++ show (x ^. values . at var . non undefined)) *> failure

eqTypes :: (Monad m, MonadPlus m) => Type Typed -> Type Typed -> PropertyT m ()
eqTypes a b =
  let noPoly (TyForall _ t) = noPoly t
      noPoly t = t

      a' = noPoly a
      b' = noPoly b
   in do
     case solve 0 mempty [ConUnify (BecauseOf internal) a' b'] of
       Left e -> footnote (show (pretty e)) *> failure
       Right c -> pure ()

tests :: IO Bool
tests = checkParallel $$(discover)
