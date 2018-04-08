{-# LANGUAGE TupleSections #-}
module Core.Types
  ( arity
  , approximateType
  , unify, unifyWith, unifyClosed
  , replaceTy
  ) where

import qualified Data.Map.Strict as Map
import Core.Builtin
import Core.Core

import Control.Lens

import Control.Monad.State
import Control.Applicative

import Data.Traversable
import Data.Semigroup
import Data.Foldable
import Data.VarSet(IsVar(..))
import Data.Maybe
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
  Int{} -> tyInt
  Float{} -> tyFloat
  Str{} -> tyString
  LitTrue -> tyBool
  LitFalse -> tyBool
  Unit -> tyUnit
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
unify' (ForallTy vs t) (ForallTy vs' t') = unify' t (replaceTy vs (VarTy vs') t')
unify' (AppTy f t) (AppTy f' t') = liftA2 (<>) (unify' f f') (unify' t t')
unify' _ _ = lift Nothing

replaceTy :: IsVar a => a -> Type a -> Type a -> Type a
replaceTy var at = transform (go var) where
  go v (VarTy v') | v == v' = at
  go _ x = x

unifyClosed :: IsVar a => Type a -> Type a -> Bool
unifyClosed = go mempty where
  go _ (ConTy a) (ConTy b) = a == b
  go s (VarTy a) (VarTy b) = fromMaybe a (Map.lookup a s) == b
  go s (ForallTy v ty) (ForallTy v' ty')
    | v == v' = go s ty ty'
    | otherwise = go (Map.insert v v' s) ty ty'
  go s (ArrTy a r) (ArrTy a' r') = go s a a' && go s r r'
  go s (AppTy f x) (AppTy f' x') = go s f f' && go s x x'
  go s (RowsTy f ts) (RowsTy f' ts') = go s f f' && and (zipWith (\(_, t) (_, t') -> t == t' && go s t t') (sortOn fst ts) (sortOn fst ts'))
  go s (ExactRowsTy ts) (ExactRowsTy ts') = and (zipWith (\(_, t) (_, t') -> t == t' && go s t t') (sortOn fst ts) (sortOn fst ts'))
  go _ StarTy StarTy = True
  go _ _ _ = False
