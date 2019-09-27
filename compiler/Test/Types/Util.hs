{-# LANGUAGE FlexibleContexts, OverloadedStrings, FlexibleInstances, MultiParamTypeClasses #-}
module Test.Types.Util where

import Types.Infer
import Types.Unify

import Syntax.Builtin
import Syntax.Subst
import Syntax.Var
import Syntax

import qualified Control.Monad.Infer as MonadInfer
import Control.Monad.Infer (Constraint(..), SomeReason(..), TypeError)
import qualified Control.Monad.Namey as Namey

import Text.Pretty.Semantic
import Data.Sequence (Seq)
import Data.Foldable
import Data.Spanned
import Data.Reason
import Data.These
import Data.Span

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq


inferExpr :: Expr Desugared -> Either [TypeError] (Type Typed)
inferExpr e = flip Namey.evalNamey MonadInfer.firstName $ MonadInfer.runInfer builtinEnv (infer e) >>= go where
  go (This e) = pure . Left $ e
  go (These e _) = pure . Left $ e
  go (That ((_, t), cs)) = do
    solved <- MonadInfer.runChronicleT $ solve cs mempty
    pure $ case toEither solved of
      Left e -> Left (toList e)
      Right (x, _, _) -> Right (apply x t)

checkExpr :: Expr Desugared -> Type Typed -> Either [TypeError] ()
checkExpr e t = flip Namey.evalNamey MonadInfer.firstName $ MonadInfer.runInfer builtinEnv (check e t) >>= go where
  go (This e) = pure . Left $ e
  go (These e _) = pure . Left $ e
  go (That (_, cs)) = do
    solved <- MonadInfer.runChronicleT $ solve cs mempty
    pure $ case toEither solved of
      Left e -> Left (toList e)
      Right _ -> pure ()

equivalent, disjoint :: Type Typed -> Type Typed -> Bool
equivalent a b =
  let task = Seq.singleton (ConUnify (BecauseOf (Blame internal)) mempty (TgInternal "co") a b)
   in case flip Namey.evalNamey MonadInfer.firstName . MonadInfer.runChronicleT . flip solve mempty $ task of
     That{} -> True
     _ -> False

disjoint a b = not (a `equivalent` b)

unify :: Type Typed -> Type Typed -> Either (Seq TypeError) (Coercion Typed)
unify a b =
  let task = Seq.singleton (ConUnify (BecauseOf (Blame internal)) mempty (TgInternal "co") a b)
   in case flip Namey.evalNamey MonadInfer.firstName . MonadInfer.runChronicleT . flip solve mempty $ task of
        This e -> Left e
        These e _ -> Left e
        That (_, m, _) -> case m Map.! TgInternal "co" of
          Cast co -> Right co
          IdWrap -> Right (AssumedCo a b)
          c -> error ("not a coercion " ++ show c)

newtype Blame p = Blame Span

instance Reasonable Blame p where
  blame _ = string "a test"

instance Spanned (Blame p) where
  annotation (Blame x) =x

instance Pretty (Blame p) where
  pretty _ = empty

toEither :: These a b -> Either a b
toEither = these Left Right (\x _ -> Left x)
