{-# LANGUAGE FlexibleContexts, OverloadedStrings, FlexibleInstances, MultiParamTypeClasses #-}
module Test.Types.Util where

import Types.Infer
import Types.Unify

import Syntax.Subst
import Syntax.Var
import Syntax

import qualified Control.Monad.Infer as MonadInfer
import Control.Monad.Infer (Constraint(..), SomeReason(..), TypeError)
import qualified Control.Monad.Namey as Namey

import Text.Pretty.Semantic
import Data.Spanned
import Data.Reason
import Data.Span

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq


inferExpr :: Expr Resolved -> Either [TypeError] (Type Typed)
inferExpr e = go . flip Namey.runNamey MonadInfer.firstName $ MonadInfer.runInfer builtinsEnv (infer e) where
  go (Left e, _) = Left e
  go (Right ((_, t), cs), xs) = case solve xs cs of
    Left e -> Left [e]
    Right (x, _) -> pure (apply x t)

checkExpr :: Expr Resolved -> Type Typed -> Either [TypeError] ()
checkExpr e t = go . flip Namey.runNamey MonadInfer.firstName $ MonadInfer.runInfer builtinsEnv (check e t) where
  go (Left e, _) = Left e
  go (Right (_, cs), xs) = case solve xs cs of
    Left e -> Left [e]
    Right _ -> pure ()

equivalent, disjoint :: Type Typed -> Type Typed -> Bool
equivalent a b =
  case solve MonadInfer.firstName (Seq.singleton (ConUnify (BecauseOf (Blame internal)) (TvName (TgInternal "co")) a b)) of
    Left{} -> False
    Right{} -> True
disjoint a b = not (a `equivalent` b)

unify :: Type Typed -> Type Typed -> Either TypeError (Coercion Typed)
unify a b =
  case solve MonadInfer.firstName (Seq.singleton (ConUnify (BecauseOf (Blame internal)) (TvName (TgInternal "co")) a b)) of
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
