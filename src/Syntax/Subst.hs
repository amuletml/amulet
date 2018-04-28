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
import qualified Data.Sequence as Seq
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
  ftv TyPromotedCon{} = mempty
  ftv TySkol{} = mempty
  ftv TyType{} = mempty
  ftv (TyVar v) = Set.singleton v
  ftv (TyApp a b) = ftv a <> ftv b
  ftv (TyTuple a b) = ftv a <> ftv b
  ftv (TyRows rho rows) = ftv rho <> foldMap (ftv . snd) rows
  ftv (TyExactRows rows) = foldMap (ftv . snd) rows
  ftv (TyWithConstraints eq b) = foldMap (\(a, b) -> ftv a <> ftv b) eq <> ftv b
  ftv (TyPi binder t) = ftv binder <> (ftv t Set.\\ bound binder)

  apply _ (TyCon a) = TyCon a
  apply _ (TySkol x) = TySkol x
  apply _ (TyPromotedCon x) = TyPromotedCon x
  apply _ TyType = TyType
  apply s t@(TyVar v) = Map.findWithDefault t v s
  apply s (TyApp a b) = TyApp (apply s a) (apply s b)
  apply s (TyTuple a b) = TyTuple (apply s a) (apply s b)
  apply s (TyPi binder t) = TyPi (apply s binder) (apply s' t) where
    s' = foldr Map.delete s (Set.toList (bound binder))
  apply s (TyRows rho rows) = TyRows (apply s rho) (map (second (apply s)) rows)
  apply s (TyExactRows rows) = TyExactRows  (map (second (apply s)) rows)
  apply s (TyWithConstraints eq b) = TyWithConstraints (map (\(a, b) -> (apply s a, apply s b)) eq) (apply s b)

instance Ord (Var p) => Substitutable p (Coercion p) where
  ftv VarCo{} = mempty
  ftv (ReflCo t) = ftv t
  ftv (AssumedCo t t') = ftv t <> ftv t'
  ftv (SymCo c) = ftv c
  ftv (AppCo f x) = ftv f <> ftv x
  ftv (ArrCo f x) = ftv f <> ftv x
  ftv (ProdCo f x) = ftv f <> ftv x
  ftv (ExactRowsCo rs) = foldMap (ftv . snd) rs
  ftv (RowsCo c rs) = ftv c <> foldMap (ftv . snd) rs
  ftv (ForallCo v t cs) = ftv t <> v `Set.delete` ftv cs

  apply _ x@VarCo{} = x
  apply s (ReflCo t) = ReflCo (apply s t)
  apply s (AssumedCo t t') = AssumedCo (apply s t) (apply s t')
  apply s (SymCo x) = SymCo (apply s x)
  apply s (AppCo f x) = AppCo (apply s f) (apply s x)
  apply s (ArrCo f x) = ArrCo (apply s f) (apply s x)
  apply s (ProdCo f x) = ProdCo (apply s f) (apply s x)
  apply s (ExactRowsCo rs) = ExactRowsCo (map (second (apply s)) rs)
  apply s (RowsCo c rs) = RowsCo (apply s c) (map (second (apply s)) rs)
  apply s (ForallCo v c cs) = ForallCo v (apply s c) (apply s' cs) where
    s' = Map.delete v s

instance (Ord (Var p), Substitutable p a) => Substitutable p [a] where
  ftv = foldMap ftv
  apply s = map (apply s)

instance (Ord (Var p), Substitutable p a) => Substitutable p (Seq.Seq a) where
  ftv = foldMap ftv
  apply s = fmap (apply s)

instance Ord (Var p) => Substitutable p (TyBinder p) where
  ftv (Anon t) = ftv t
  ftv (Implicit _ k) = maybe mempty ftv k

  apply s (Anon t) = Anon (apply s t)
  apply s (Implicit v k) = Implicit v (fmap (apply s) k)

bound :: Ord (Var p) => TyBinder p -> Set.Set (Var p)
bound Anon{} = Set.empty
bound (Implicit v _) = Set.singleton v

compose :: Ord (Var p) => Subst p -> Subst p -> Subst p
s1 `compose` s2 = fmap (apply s1) s2 <> fmap (apply s2) s1
