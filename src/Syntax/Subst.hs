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
  ftv (TyVar v _) = S.singleton v
  ftv (TyForall vs cs t _) = (foldMap ftv cs `S.union` ftv t) S.\\ S.fromList vs
  ftv (TyApp a b _) = ftv a `S.union` ftv b
  ftv (TyArr a b _) = ftv a `S.union` ftv b
  ftv (TyRows rho rows _) = ftv rho `S.union` foldMap (ftv . snd) rows

  apply _ (TyCon a l) = TyCon a l
  apply _ (TyStar l) = TyStar l
  apply s t@(TyVar v _) = M.findWithDefault t v s
  apply s (TyArr a b l) = TyArr (apply s a) (apply s b) l
  apply s (TyApp a b l) = TyApp (apply s a) (apply s b) l
  apply s (TyForall v cs t l) = TyForall v (map (apply s') cs) (apply s' t) l where
    s' = foldr M.delete s v
  apply s (TyRows rho rows ann) = TyRows (apply s rho) (map (second (apply s)) rows) ann

instance Substitutable a => Substitutable [a] where
  ftv = S.unions . map ftv
  apply s = map (apply s)

compose :: Subst -> Subst -> Subst
s1 `compose` s2 = M.map (apply s1) s2 `M.union` s1
