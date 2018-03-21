{-# LANGUAGE TupleSections #-}
module Core.Types
  ( arity
  , approximateType
  , unify, unifyWith
  ) where

import qualified Data.Map.Strict as Map
import Core.Lower
import Core.Core

import Control.Lens

import Control.Monad.State
import Control.Applicative

import Data.Traversable
import Data.Semigroup
import Data.Foldable
import Data.VarSet
import Data.List

arity :: Type a -> Int
arity (ArrTy _ t) = 1 + arity t
arity (ForallTy _ t) = arity t
arity _ = 0

approximateAtomType :: IsVar a => Atom a -> Maybe (Type a)
approximateAtomType (Ref _ t) = pure t
approximateAtomType (Lam (TypeArgument v _) f) = ForallTy v <$> approximateType f
approximateAtomType (Lam (TermArgument _ t) f) = ArrTy t <$> approximateType f
approximateAtomType (Lit l) = pure . fmap fromVar $ case l of
  Int{} -> cotyInt
  Float{} -> cotyFloat
  Str{} -> cotyString
  LitTrue -> cotyBool
  LitFalse -> cotyBool
  Unit -> cotyUnit
  RecNil -> ExactRowsTy []

approximateType :: IsVar a => Term a -> Maybe (Type a)
approximateType (Atom a) = approximateAtomType a
approximateType (Cast _ phi) = snd <$> relates phi
approximateType (App f _) = do
  ArrTy _ d <- approximateAtomType f
  pure d
approximateType (Let _ e) = approximateType e
approximateType (Match _ xs) = case xs of
  ((_, _, t):_) -> approximateType t
  [] -> error "impossible approximateType empty match"
approximateType (Extend e rs) = RowsTy <$> approximateAtomType e <*> traverse (\(x, _, t) -> (x,) <$> approximateAtomType t) rs
approximateType (TyApp f at) = do
  ForallTy v t <- approximateAtomType f
  let replace = transform go
      go (VarTy v') | v == v' = at
      go x = x
  pure (replace t)

unify :: IsVar a => Type a -> Type a -> Maybe (Map.Map a (Type a))
unify = unifyWith mempty

unifyWith :: IsVar a => Map.Map a (Type a) -> Type a -> Type a -> Maybe (Map.Map a (Type a))
unifyWith m a b = execStateT (unify' a b) m

unify' :: IsVar a => Type a -> Type a -> StateT (Map.Map a (Type a)) Maybe ()
unify' t'@(VarTy v) t
  | t' == t = pure ()
  | otherwise = do
      x <- gets (Map.lookup v)
      case x of
        Just t' -> unify' t t'
        Nothing -> modify (Map.insert v t)
unify' t (VarTy v) = unify' (VarTy v) t
unify' (ConTy v) (ConTy v') = mempty <$ guard (v == v')
unify' (ArrTy a b) (ArrTy a' b') = liftA2 (<>) (unify' a a') (unify' b b')
unify' (RowsTy t ts) (RowsTy t' ts') = do
  mgu_t <- unify' t t'
  ts <- for (zip (sortOn fst ts) (sortOn fst ts')) $ \((_, t), (_, t')) -> unify' t t'
  pure (mgu_t <> fold ts)
unify' (ExactRowsTy ts) (ExactRowsTy ts') = fold <$> for (zip (sortOn fst ts) (sortOn fst ts')) (\((_, t), (_, t')) -> unify' t t')
unify' (ForallTy vs t) (ForallTy vs' t') = unify' t (replace vs vs' t') where
  replace f t = replaceOne (VarTy t) f
  replaceOne at var = transform (go var) where
    go v (VarTy v') | v == v' = at
    go _ x = x
unify' (AppTy f t) (AppTy f' t') = liftA2 (<>) (unify' f f') (unify' t t')
unify' _ _ = lift Nothing
