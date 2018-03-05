{-# LANGUAGE TupleSections #-}
module Core.Types
  ( arity
  , approximateType
  , unify
  ) where

import qualified Data.Map.Strict as Map
import Core.Lower
import Core.Core

import Control.Monad.State
import Control.Applicative

import Data.Traversable
import Data.Semigroup
import Data.Generics
import Data.Foldable
import Data.VarSet
import Data.List

arity :: Type a -> Int
arity (ArrTy _ t) = 1 + arity t
arity (ForallTy _ t) = arity t
arity _ = 0

approximateAtomType :: IsVar a => Atom a -> Maybe (Type a)
approximateAtomType (Ref _ t) = pure t
approximateAtomType (Lam Big (v, _) f) = ForallTy v <$> approximateType f
approximateAtomType (Lam Small (_, t) f) = ArrTy t <$> approximateType f
approximateAtomType (Lit l) = pure . fmap fromVar $ case l of
  Int{} -> cotyInt
  Str{} -> cotyString
  LitTrue -> cotyBool
  LitFalse -> cotyBool
  Unit -> cotyUnit
  RecNil -> ExactRowsTy []

doCast :: IsVar a => Coercion a -> Type a -> Maybe (Type a)
doCast (SameRepr t t') i = case unify t i of
  Just _ -> pure t'
  _ -> Nothing
doCast (Domain c) (ArrTy x t) = ArrTy <$> doCast c x <*> pure t
doCast (Codomain c) (ArrTy t x) = ArrTy t <$> doCast c x
doCast _ _ = Nothing

approximateType :: IsVar a => Term a -> Maybe (Type a)
approximateType (Atom a) = approximateAtomType a
approximateType (Cast t phi) = doCast phi =<< approximateAtomType t
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
  let replace = everywhere (mkT go)
      go (VarTy v') | v == v' = at
      go x = x
  pure (replace t)

unify :: (Ord a, Data a) => Type a -> Type a -> Maybe (Map.Map a (Type a))
unify a b = execStateT (unify' a b) mempty

unify' :: (Ord a, Data a) => Type a -> Type a -> StateT (Map.Map a (Type a)) Maybe ()
unify' (VarTy v) t = do
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
  replaceOne at var = everywhere (mkT (go var)) where
    go v (VarTy v') | v == v' = at
    go _ x = x
unify' (AppTy f t) (AppTy f' t') = liftA2 (<>) (unify' f f') (unify' t t')
unify' _ _ = lift Nothing
