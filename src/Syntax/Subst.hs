{-# LANGUAGE FlexibleInstances
  , FlexibleContexts
  , UndecidableInstances
  , MultiParamTypeClasses
  , FunctionalDependencies
  #-}
module Syntax.Subst
  ( Subst
  , Substitutable
  , ftv
  , apply
  , compose
  , Map.fromList )
  where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Semigroup

import Control.Arrow (second)

import Syntax

type Subst p = Map.Map (Var p) (Type p)

class Substitutable p a | a -> p where
  ftv :: a -> Set.Set (Var p)
  apply :: Subst p -> a -> a

instance Ord (Var p) => Substitutable p (Type p) where
  ftv TyCon{} = mempty
  ftv TySkol{} = mempty
  ftv (TyVar v) = Set.singleton v
  ftv (TyForall vs t) = ftv t Set.\\ Set.fromList vs
  ftv (TyApp a b) = ftv a <> ftv b
  ftv (TyTuple a b) = ftv a <> ftv b
  ftv (TyArr a b) = ftv a <> ftv b
  ftv (TyRows rho rows) = ftv rho <> foldMap (ftv . snd) rows
  ftv (TyExactRows rows) = foldMap (ftv . snd) rows
  ftv (TyWithConstraints eq b) = foldMap (\(a, b) -> ftv a <> ftv b) eq <> ftv b

  apply _ (TyCon a) = TyCon a
  apply _ (TySkol x) = TySkol x
  apply s t@(TyVar v) = Map.findWithDefault t v s
  apply s (TyArr a b) = TyArr (apply s a) (apply s b)
  apply s (TyApp a b) = TyApp (apply s a) (apply s b)
  apply s (TyTuple a b) = TyTuple (apply s a) (apply s b)
  apply s (TyForall v t) = TyForall v (apply s' t) where
    s' = foldr Map.delete s v
  apply s (TyRows rho rows) = TyRows (apply s rho) (map (second (apply s)) rows)
  apply s (TyExactRows rows) = TyExactRows  (map (second (apply s)) rows)
  apply s (TyWithConstraints eq b) = TyWithConstraints (map (\(a, b) -> (apply s a, apply s b)) eq) (apply s b)

instance Ord (Var p) => Substitutable p (Coercion p) where
  ftv VarCo{} = mempty
  ftv (ReflCo t t') = ftv t <> ftv t'

  apply _ x@VarCo{} = x
  apply s (ReflCo t t') = ReflCo (apply s t) (apply s t')

instance (Ord (Var p), Substitutable p a) => Substitutable p [a] where
  ftv = foldMap ftv
  apply s = map (apply s)

compose :: Ord (Var p) => Subst p -> Subst p -> Subst p
s1 `compose` s2 = fmap (apply s1) s2 <> fmap (apply s2) s1
