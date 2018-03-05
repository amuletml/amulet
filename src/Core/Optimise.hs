module Core.Optimise
  ( substitute, substituteInTys, substituteInCo
  , module Core.Core
  , Var(..)
  , fresh
  ) where

import qualified Data.Map.Strict as Map

import Data.VarSet (IsVar(..))

import Control.Monad.Infer (fresh)

import Control.Arrow (second)
import Data.Triple

import Core.Core
import Syntax (Var(..))

substitute :: IsVar a => Map.Map a (Atom a) -> Term a -> Term a
substitute m = term where
  term (Atom a) = Atom (atom a)
  term (App f x) = App (atom f) (atom x)
  term (Let vs x) = Let (map (third3 term) vs) (term x)
  term (Match x vs) = Match (atom x) (map (third3 term) vs)
  term (Extend e rs) = Extend (atom e) (map (third3 atom) rs)
  term (TyApp f t) = TyApp (atom f) t
  term (Cast f t) = Cast (atom f) t

  atom x@(Ref v _) = Map.findWithDefault x v m
  atom (Lam s v b) = Lam s v (term b)
  atom x@Lit{} = x

substituteInTys :: IsVar a => Map.Map a (Type a) -> Term a -> Term a
substituteInTys m = term where
  term (Atom a) = Atom (atom a)
  term (App f x) = App (atom f) (atom x)
  term (Let vs x) = Let (map (\(v, t, e) -> (v, gotype t, term e)) vs) (term x)
  term (Match x vs) = Match (atom x) (map (\(v, t, e) -> (v, gotype t, term e)) vs)
  term (Extend e rs) = Extend (atom e) (map (third3 atom) rs)
  term (TyApp f t) = TyApp (atom f) (gotype t)
  term (Cast f t) = Cast (atom f) (coercion t)

  coercion (SameRepr t t') = SameRepr (gotype t) (gotype t')
  coercion (Domain c) = Domain (coercion c)
  coercion (Codomain c) = Codomain (coercion c)
  coercion (Symmetry c) = Symmetry (coercion c)

  atom (Ref v t) = Ref v (gotype t)
  atom (Lam s (v, t) b) = Lam s (v, gotype t) (term b)
  atom x@Lit{} = x

  gotype x@(VarTy v) = Map.findWithDefault x v m
  gotype x@ConTy{} = x
  gotype (ForallTy v t) = ForallTy v (gotype t)
  gotype (ArrTy a b) = ArrTy (gotype a) (gotype b)
  gotype (AppTy f x) = AppTy (gotype f) (gotype x)
  gotype (RowsTy v rs) = RowsTy (gotype v) (map (second gotype) rs)
  gotype (ExactRowsTy rs) = ExactRowsTy (map (second gotype) rs)
  gotype StarTy = StarTy

substituteInCo :: IsVar a => Map.Map a (Type a) -> Coercion a -> Coercion a
substituteInCo m = coercion where
  coercion (SameRepr t t') = SameRepr (gotype t) (gotype t')
  coercion (Domain c) = Domain (coercion c)
  coercion (Codomain c) = Codomain (coercion c)
  coercion (Symmetry c) = Symmetry (coercion c)

  gotype x@(VarTy v) = Map.findWithDefault x v m
  gotype x@ConTy{} = x
  gotype (ForallTy v t) = ForallTy v (gotype t)
  gotype (ArrTy a b) = ArrTy (gotype a) (gotype b)
  gotype (AppTy f x) = AppTy (gotype f) (gotype x)
  gotype (RowsTy v rs) = RowsTy (gotype v) (map (second gotype) rs)
  gotype (ExactRowsTy rs) = ExactRowsTy (map (second gotype) rs)
  gotype StarTy = StarTy
