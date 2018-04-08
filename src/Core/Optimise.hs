{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE FlexibleContexts, TupleSections, PartialTypeSignatures #-}
module Core.Optimise
  ( substitute, substituteInTys, substituteInType, substituteInCo
  , module Core.Core
  , Var(..)
  , refresh, fresh, freshFrom
  , argVar
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.VarMap as VarMap
import Data.VarSet (IsVar(..))

import Control.Monad.Infer (fresh, freshFrom)
import Control.Arrow (second)
import Control.Monad.Gen

import Data.Foldable
import Data.Triple
import Data.Maybe

import Core.Core
import Syntax (Var(..))

substitute :: IsVar a => Map.Map a (Atom a) -> Term a -> Term a
substitute m = term where
  term (Atom a) = Atom (atom a)
  term (App f x) = App (atom f) (atom x)
  term (Let (One k v) x) = Let (One k (third3 term v)) (term x)
  term (Let (Many k vs) x) = Let (Many k (map (third3 term) vs)) (term x)
  term (Match x vs) = Match (atom x) (map (third3 term) vs)
  term (Extend e rs) = Extend (atom e) (map (third3 atom) rs)
  term (TyApp f t) = TyApp (atom f) t
  term (Cast f t) = Cast (atom f) t

  atom x@(Ref v _) = Map.findWithDefault x v m
  atom (Lam v b) = Lam v (term b)
  atom x@Lit{} = x

substituteInTys :: IsVar a => Map.Map a (Type a) -> Term a -> Term a
substituteInTys m = term where
  term (Atom a) = Atom (atom a)
  term (App f x) = App (atom f) (atom x)
  term (Let (One k (v, t, e)) x) = Let (One k (v, gotype t, term e)) x
  term (Let (Many k vs) x) = Let (Many k (map (\(v, t, e) -> (v, gotype t, term e)) vs)) (term x)
  term (Match x vs) = Match (atom x) (map (\(v, t, e) -> (v, gotype t, term e)) vs)
  term (Extend e rs) = Extend (atom e) (map (third3 atom) rs)
  term (TyApp f t) = TyApp (atom f) (gotype t)
  term (Cast f t) = Cast (atom f) (coercion t)

  coercion (SameRepr t t') = SameRepr (gotype t) (gotype t')
  coercion (Domain c) = Domain (coercion c)
  coercion (Codomain c) = Codomain (coercion c)
  coercion (Symmetry c) = Symmetry (coercion c)
  coercion (Application f x) = Application (coercion f) (coercion x)
  coercion (ExactRecord rs) = ExactRecord (map (second coercion) rs)
  coercion (Record c rs) = Record (coercion c) (map (second coercion) rs)
  coercion (CoercionVar x) = CoercionVar x
  coercion (Quantified v co c) = Quantified v (coercion co) (coercion c)

  atom (Ref v t) = Ref v (gotype t)
  atom (Lam arg b) = Lam (go arg) (term b) where
    go (TermArgument v t) = TermArgument v (gotype t)
    go (TypeArgument v t) = TypeArgument v (gotype t)
  atom x@Lit{} = x

  gotype = substituteInType m

substituteInType :: IsVar a => Map.Map a (Type a) -> Type a -> Type a
substituteInType m = gotype where
  gotype x@(VarTy v) = Map.findWithDefault x v m
  gotype x@ConTy{} = x
  gotype (ForallTy v c t) = ForallTy v (gotype c) (gotype t)
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
  coercion (Application f x) = Application (coercion f) (coercion x)
  coercion (ExactRecord rs) = ExactRecord (map (second coercion) rs)
  coercion (Record c rs) = Record (coercion c) (map (second coercion) rs)
  coercion (CoercionVar x) = CoercionVar x
  coercion (Quantified v a c) = Quantified v (coercion a) (coercion c)

  gotype = substituteInType m

refresh :: (MonadGen Int m, IsVar a) => Term a -> m (Term a)
refresh = refreshTerm mempty where
  refreshAtom :: (MonadGen Int m, IsVar a) => VarMap.Map a -> Atom a -> m (Atom a)
  refreshAtom s (Ref v ty) =
    let v' = fromMaybe v (VarMap.lookup (toVar v) s)
        ty' = refreshType s ty
    in pure (Ref v' ty')
  refreshAtom _ a@Lit{} = pure a
  refreshAtom s (Lam arg b) = do
    (arg', v') <- refreshArg s arg
    Lam arg' <$> refreshTerm (VarMap.insert (argVar arg) v' s) b

  refreshArg :: (MonadGen Int m, IsVar a) => VarMap.Map a -> Argument a -> m (Argument a, a)
  refreshArg s (TermArgument n ty) = do
    let TgName name _ = toVar n
        ty' = refreshType s ty
    v' <- fromVar <$> freshFrom name
    pure (TermArgument v' ty', v')
  refreshArg s (TypeArgument n ty) = do
    let TgName name _ = toVar n
        ty' = refreshType s ty
    v' <- fromVar <$> freshFrom name
    pure (TypeArgument v' ty', v')

  refreshTerm :: (MonadGen Int m, IsVar a) => VarMap.Map a -> Term a -> m (Term a)
  refreshTerm s (Atom a) = Atom <$> refreshAtom s a
  refreshTerm s (App f x) = App <$> refreshAtom s f <*> refreshAtom s x
  refreshTerm s (TyApp f ty) = TyApp <$> refreshAtom s f <*> pure (refreshType s ty)
  refreshTerm s (Let (One k (v, ty, e)) b) = do
    let TgName name _ = toVar v
    v' <- fromVar <$> freshFrom name
    let s' = VarMap.insert (toVar v) v' s
    e' <- refreshTerm s' e
    Let (One k (v', ty, e')) <$> refreshTerm s' b
  refreshTerm s (Let (Many k vs) b) = do
    s' <- foldrM (\(v, _, _) m -> do
                     let TgName name _ = toVar v
                     v' <- fromVar <$> freshFrom name
                     pure (VarMap.insert (toVar v) v' m)) s vs
    vs' <- traverse (\(v, ty, e) -> (fromJust (VarMap.lookup (toVar v) s'), ty,) <$> refreshTerm s' e) vs
    Let (Many k vs') <$> refreshTerm s' b
  refreshTerm s (Match e branches) = Match <$> refreshAtom s e
                                            <*> traverse refreshBranch branches where
    refreshBranch (test, ty, branch) = do
      s' <- foldrM (\v m -> do
                       let TgName name _ = toVar v
                       v' <- fromVar <$> freshFrom name
                       pure (VarMap.insert (toVar v) v' m)) s (patternVarsA test `asTypeOf` [])
      (refreshPattern s' test, refreshType s' ty,) <$> refreshTerm s' branch
  refreshTerm s (Extend e bs) = Extend <$> refreshAtom s e <*> traverse (third3A (refreshAtom s)) bs
  refreshTerm s (Cast e c) = Cast <$> refreshAtom s e <*> pure (refreshCoercion s c)

  refreshPattern :: IsVar a => VarMap.Map a -> Pattern a -> Pattern a
  refreshPattern s (Capture v ty) = Capture (fromJust (VarMap.lookup (toVar v) s)) (refreshType s ty)
  refreshPattern _ p@Constr{} = p
  refreshPattern s (Destr c p) = Destr c (refreshPattern s p)
  refreshPattern s (PatExtend p fs) = PatExtend (refreshPattern s p) (map (second (refreshPattern s)) fs)
  refreshPattern _ p@PatLit{} = p

  refreshType :: IsVar a => VarMap.Map a -> Type a -> Type a
  refreshType s x@(VarTy v) = maybe x VarTy (VarMap.lookup (toVar v) s)
  refreshType _ x@ConTy{} = x
  refreshType s (ForallTy v c t) = ForallTy v (refreshType s c) (refreshType s t)
  refreshType s (AppTy f x) = AppTy (refreshType s f) (refreshType s x)
  refreshType s (RowsTy v rs) = RowsTy (refreshType s v) (map (second (refreshType s)) rs)
  refreshType s (ExactRowsTy rs) = ExactRowsTy (map (second (refreshType s)) rs)
  refreshType _ StarTy = StarTy

  refreshCoercion :: IsVar a => VarMap.Map a -> Coercion a -> Coercion a
  refreshCoercion s (SameRepr t t') = SameRepr (refreshType s t) (refreshType s t')
  refreshCoercion s (Domain c) = Domain (refreshCoercion s c)
  refreshCoercion s (Codomain c) = Codomain (refreshCoercion s c)
  refreshCoercion s (Symmetry c) = Symmetry (refreshCoercion s c)
  refreshCoercion s (Application f x) = Application (refreshCoercion s f) (refreshCoercion s x)
  refreshCoercion s (ExactRecord rs) = ExactRecord (map (second (refreshCoercion s)) rs)
  refreshCoercion s (Record c rs) = Record (refreshCoercion s c) (map (second (refreshCoercion s)) rs)
  refreshCoercion s x@(CoercionVar v) = maybe x CoercionVar (VarMap.lookup (toVar v) s)
  refreshCoercion s (Quantified v a c) = Quantified v (refreshCoercion s a) (refreshCoercion s c)

argVar :: IsVar a => Argument a -> Var _
argVar (TermArgument v _) = toVar v
argVar (TypeArgument v _) = toVar v
