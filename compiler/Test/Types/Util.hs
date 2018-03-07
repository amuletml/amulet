{-# LANGUAGE FlexibleContexts #-}
module Test.Types.Util where

import "amuletml" Types.Infer
import "amuletml" Types.Unify

import "amuletml" Syntax.Subst
import "amuletml" Syntax

import qualified "amuletml" Control.Monad.Infer as MonadInfer
import "amuletml" Control.Monad.Infer (Constraint(..), SomeReason(..), TypeError)

import "amuletml" Data.Span

import qualified Control.Monad.Gen as MonadGen

inferExpr :: Expr Resolved -> Either TypeError (Type Typed)
inferExpr e = go . MonadGen.runGen $ MonadInfer.runInfer builtinsEnv (infer e) where
  go (Left e) = Left e
  go (Right ((_, t), cs)) = case solve 1 mempty cs of
    Left e -> Left e
    Right x -> pure (apply x t)

checkExpr :: Expr Resolved -> Type Typed -> Either TypeError ()
checkExpr e t = go . MonadGen.runGen $ MonadInfer.runInfer builtinsEnv (check e t) where
  go (Left e) = Left e
  go (Right (_, cs)) = case solve 1 mempty cs of
    Left e -> Left e
    Right _ -> pure ()

equivalent, disjoint :: Type Typed -> Type Typed -> Bool
equivalent a b =
  let noPoly (TyForall _ t) = noPoly t
      noPoly t = t

      a' = noPoly a
      b' = noPoly b
   in case solve 0 mempty [ConUnify (BecauseOf internal) a' b'] of
       Left{} -> False
       Right{} -> True
disjoint a b = not (a `equivalent` b)
