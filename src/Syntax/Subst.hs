module Syntax.Subst
  ( Subst
  , ftv
  , apply
  , M.fromList )
  where

import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Syntax

type Subst = M.Map String Type

ftv :: Type -> S.Set String
ftv TyCon{} = S.empty
ftv (TyVar v) = S.singleton v
ftv (TyForall vs cs t) = (foldMap ftv cs `S.union` ftv t) S.\\ S.fromList vs
ftv (TyApp a b) = ftv a `S.union` ftv b
ftv (TyArr a b) = ftv a `S.union` ftv b

apply :: Subst -> Type -> Type
apply _ (TyCon a) = TyCon a
apply s t@(TyVar v) = M.findWithDefault t v s
apply s (TyArr a b) = TyArr (apply s a) (apply s b)
apply s (TyApp a b) = TyApp (apply s a) (apply s b)
apply s (TyForall v cs t) = TyForall v (map (apply s') cs) (apply s' t) where
  s' = foldr M.delete s v
