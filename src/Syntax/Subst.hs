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

import Syntax

type Subst = M.Map (Var 'TypedPhase) (Type 'TypedPhase)

class Substitutable a where
  ftv :: a -> S.Set (Var 'TypedPhase)
  apply :: Subst -> a -> a

instance Substitutable (Type 'TypedPhase) where
  ftv TyCon{} = S.empty
  ftv TyStar{} = S.empty
  ftv (TyVar v) = S.singleton v
  ftv (TyForall vs cs t) = (foldMap ftv cs `S.union` ftv t) S.\\ S.fromList vs
  ftv (TyApp a b) = ftv a `S.union` ftv b
  ftv (TyArr a b) = ftv a `S.union` ftv b

  apply _ (TyCon a) = TyCon a
  apply _ TyStar = TyStar
  apply s t@(TyVar v) = M.findWithDefault t v s
  apply s (TyArr a b) = TyArr (apply s a) (apply s b)
  apply s (TyApp a b) = TyApp (apply s a) (apply s b)
  apply s (TyForall v cs t) = TyForall v (map (apply s') cs) (apply s' t) where
    s' = foldr M.delete s v

instance Substitutable a => Substitutable [a] where
  ftv = S.unions . map ftv
  apply s = map (apply s)

compose :: Subst -> Subst -> Subst
s1 `compose` s2 = M.map (apply s1) s2 `M.union` s1
