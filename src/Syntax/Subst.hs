{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

module Syntax.Subst
  ( Subst
  , Substitutable
  , ftv
  , apply
  , compose
  , M.fromList )
  where

import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Control.Arrow (second)

import Syntax

type Subst = M.Map (Var Typed) (Type Typed)

class Substitutable a where
  ftv :: a -> S.Set (Var Typed)
  apply :: Subst -> a -> a

instance Substitutable (Type Typed) where
  ftv TyCon{} = S.empty
  ftv TyStar{} = S.empty
  ftv (TyVar v) = S.singleton v
  ftv (TyForall vs t _) = ftv t S.\\ S.fromList vs
  ftv (TyApp a b _) = ftv a `S.union` ftv b
  ftv (TyTuple a b _) = ftv a `S.union` ftv b
  ftv (TyArr a b _) = ftv a `S.union` ftv b
  ftv (TyRows rho rows _) = ftv rho `S.union` foldMap (ftv . snd) rows
  ftv (TyExactRows rows _) = foldMap (ftv . snd) rows
  ftv (TyCons cs t _) = foldMap ftv cs `S.union` ftv t

  apply _ (TyCon a l) = TyCon a l
  apply _ (TyStar l) = TyStar l
  apply s t@(TyVar v) = M.findWithDefault t v s
  apply s (TyArr a b l) = TyArr (apply s a) (apply s b) l
  apply s (TyApp a b l) = TyApp (apply s a) (apply s b) l
  apply s (TyTuple a b l) = TyTuple (apply s a) (apply s b) l
  apply s (TyForall v t l) = TyForall v (apply s' t) l where
    s' = foldr M.delete s v
  apply s (TyRows rho rows ann) = TyRows (apply s rho) (map (second (apply s)) rows) ann
  apply s (TyExactRows rows ann) = TyExactRows  (map (second (apply s)) rows) ann
  apply s (TyCons cs t ann) = TyCons (map (apply s) cs) (apply s t) ann

instance Substitutable a => Substitutable [a] where
  ftv = foldMap ftv
  apply s = map (apply s)

instance Substitutable (GivenConstraint Typed) where
  ftv (Equal a b _) = ftv a `S.union` ftv b
  apply s (Equal a b ann) = Equal (apply s a) (apply s b) ann

compose :: Subst -> Subst -> Subst
s1 `compose` s2 = M.map (apply s1) s2 `M.union` s1
