{-# LANGUAGE FlexibleContexts, OverloadedStrings, FlexibleInstances, MultiParamTypeClasses #-}
module Test.Types.Util where

import "amuletml" Types.Infer
import "amuletml" Types.Unify

import "amuletml" Syntax.Subst
import "amuletml" Syntax

import qualified "amuletml" Control.Monad.Infer as MonadInfer
import "amuletml" Control.Monad.Infer (Constraint(..), SomeReason(..), TypeError)

import "amuletml" Text.Pretty.Semantic
import "amuletml" Data.Spanned
import "amuletml" Data.Reason
import "amuletml" Data.Span

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq

import qualified Control.Monad.Gen as MonadGen

inferExpr :: Expr Resolved -> Either [TypeError] (Type Typed)
inferExpr e = go . MonadGen.runGen $ MonadInfer.runInfer builtinsEnv (infer e) where
  go (Left e) = Left e
  go (Right ((_, t), cs)) = case solve 0 cs of
    Left e -> Left [e]
    Right (x, _) -> pure (apply x t)

checkExpr :: Expr Resolved -> Type Typed -> Either [TypeError] ()
checkExpr e t = go . MonadGen.runGen $ MonadInfer.runInfer builtinsEnv (check e t) where
  go (Left e) = Left e
  go (Right (_, cs)) = case solve 0 cs of
    Left e -> Left [e]
    Right _ -> pure ()

equivalent, disjoint :: Type Typed -> Type Typed -> Bool
equivalent a b =
  case solve 0 (Seq.singleton (ConUnify (BecauseOf (Blame internal)) (TvName (TgInternal "co")) a b)) of
    Left{} -> False
    Right{} -> True
disjoint a b = not (a `equivalent` b)

unify :: Type Typed -> Type Typed -> Either TypeError (Coercion Typed)
unify a b =
  case solve 0 (Seq.singleton (ConUnify (BecauseOf (Blame internal)) (TvName (TgInternal "co")) a b)) of
    Left e -> Left e
    Right (_, m) -> case m Map.! TvName (TgInternal "co") of
      Cast co -> Right co
      _ -> error "not a coercion"


newtype Blame p = Blame Span

instance Reasonable Blame p where
  blame _ = string "a test"

instance Spanned (Blame p) where
  annotation (Blame x) =x

instance Pretty (Blame p) where
  pretty _ = empty
