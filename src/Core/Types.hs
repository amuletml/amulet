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
import Data.List

import Syntax (Var, Resolved)

arity :: CoType a -> Int
arity (CotyArr _ t) = 1 + arity t
arity (CotyForall _ t) = arity t
arity _ = 0

approximateType :: CoTerm (Var Resolved) -> Maybe (CoType (Var Resolved))
approximateType (CotRef _ t) = pure t
approximateType (CotLam Big (v, _) f) = CotyForall v <$> approximateType f
approximateType (CotLam Small (_, t) f) = CotyArr t <$> approximateType f
approximateType (CotApp f _) = do
  CotyArr _ d <- approximateType f
  pure d
approximateType (CotLet _ e) = approximateType e
approximateType (CotMatch _ xs) = case xs of
  ((_, _, t):_) -> approximateType t
  [] -> error "impossible approximateType empty match"
approximateType (CotLit l) = pure $ case l of
  ColInt{} -> cotyInt
  ColStr{} -> cotyString
  ColTrue -> cotyBool
  ColFalse -> cotyBool
  ColUnit -> cotyUnit
  ColRecNil -> CotyExactRows []
approximateType (CotExtend e rs) = CotyRows <$> approximateType e <*> traverse (\(x, _, t) -> (x,) <$> approximateType t) rs
approximateType (CotTyApp f at) = do
  CotyForall v t <- approximateType f
  let replace = everywhere (mkT go)
      go (CotyVar v') | v == v' = at
      go x = x
  pure (replace t)

unify :: (Ord a, Data a) => CoType a -> CoType a -> Maybe (Map.Map a (CoType a))
unify a b = execStateT (unify' a b) mempty

unify' :: (Ord a, Data a) => CoType a -> CoType a -> StateT (Map.Map a (CoType a)) Maybe ()
unify' (CotyVar v) t = do
  x <- gets (Map.lookup v)
  case x of
    Just t' -> unify' t t'
    Nothing -> modify (Map.insert v t)
unify' t (CotyVar v) = unify' (CotyVar v) t
unify' (CotyCon v) (CotyCon v') = mempty <$ guard (v == v')
unify' (CotyArr a b) (CotyArr a' b') = liftA2 (<>) (unify' a a') (unify' b b')
unify' (CotyRows t ts) (CotyRows t' ts') = do
  mgu_t <- unify' t t'
  ts <- for (zip (sortOn fst ts) (sortOn fst ts')) $ \((_, t), (_, t')) -> unify' t t'
  pure (mgu_t <> fold ts)
unify' (CotyExactRows ts) (CotyExactRows ts') = fold <$> for (zip (sortOn fst ts) (sortOn fst ts')) (\((_, t), (_, t')) -> unify' t t')
unify' (CotyForall vs t) (CotyForall vs' t') = unify' t (replace vs vs' t') where
  replace f t = replaceOne (CotyVar t) f
  replaceOne at var = everywhere (mkT (go var)) where
    go v (CotyVar v') | v == v' = at
    go _ x = x
unify' (CotyApp f t) (CotyApp f' t') = liftA2 (<>) (unify' f f') (unify' t t')
unify' _ _ = lift Nothing
