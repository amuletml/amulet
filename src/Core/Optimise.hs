{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}

-- | A collection of utility methods for the optimiser.
module Core.Optimise
  ( substitute, substituteInTys, substituteInType, substituteInCo
  , module Core.Core
  , module Core.Var
  , refresh, fresh, fresh', freshFrom, freshFrom'
  , argVar
  ) where

import Control.Arrow (second)
import Control.Monad.Namey
import Control.Lens

import qualified Data.VarMap as VarMap
import Data.Foldable
import Data.Triple
import Data.Maybe

import Core.Core
import Core.Var

import Syntax.Var

-- | Substitute a variable with some other atom
substitute :: IsVar a => VarMap.Map (Atom a) -> Term a -> Term a
substitute m = term where
  term (Atom a) = Atom (atom a)
  term (App f x) = App (atom f) (atom x)
  term (Lam v b) = Lam v (term b)
  term (Let (One v) x) = Let (One (third3 term v)) (term x)
  term (Let (Many vs) x) = Let (Many (map (third3 term) vs)) (term x)
  term (Match x vs) = Match (atom x) (map arm vs)
  term (Extend e rs) = Extend (atom e) (map (third3 atom) rs)
  term (Values xs) = Values (map atom xs)
  term (TyApp f t) = TyApp (atom f) t
  term (Cast f t) = Cast (atom f) t

  atom x@(Ref v _) = VarMap.findWithDefault x (toVar v) m
  atom x@Lit{} = x

  arm = armBody %~ term

-- | Substitute a type variable with some other type inside terms
substituteInTys :: forall a b. IsVar a => VarMap.Map (Type a) -> AnnTerm b a -> AnnTerm b a
substituteInTys = term where
  term :: VarMap.Map (Type a) -> AnnTerm b a -> AnnTerm b a
  term m a | VarMap.null m = a
  term m (AnnAtom z a) = AnnAtom z (atom m a)
  term m (AnnApp z f x) = AnnApp z (atom m f) (atom m x)
  term m (AnnLet z (One (v, t, e)) x) = AnnLet z (One (v, gotype m t, term m e)) (term m x)
  term m (AnnLet z (Many vs) x) = AnnLet z (Many (map (trimap id (gotype m) (term m)) vs)) (term m x)
  term m (AnnMatch z x vs) = AnnMatch z (atom m x) (map (arm m) vs)
  term m (AnnExtend z e rs) = AnnExtend z (atom m e) (map (trimap id (gotype m) (atom m)) rs)
  term m (AnnValues z xs) = AnnValues z (map (atom m) xs)
  term m (AnnTyApp z f t) = AnnTyApp z (atom m f) (gotype m t)
  term m (AnnCast z f t) = AnnCast z (atom m f) (coercion m t)
  term m (AnnLam z arg b) = AnnLam z (go arg) (term (delete m) b) where
    go (TermArgument v t) = TermArgument v (gotype m t)
    go (TypeArgument v t) = TypeArgument v (gotype m t)
    delete = case arg of
      TypeArgument v _ -> VarMap.delete (toVar v)
      _ -> id

  coercion m (SameRepr t t') = SameRepr (gotype m t) (gotype m t')
  coercion m (Domain c) = Domain (coercion m c)
  coercion m (Codomain c) = Codomain (coercion m c)
  coercion m (Symmetry c) = Symmetry (coercion m c)
  coercion m (Application f x) = Application (coercion m f) (coercion m x)
  coercion m (ExactRecord rs) = ExactRecord (map (second (coercion m)) rs)
  coercion m (Record c rs) = Record (coercion m c) (map (second (coercion m)) rs)
  coercion m (Projection rs rs') = Projection (map (second (coercion m)) rs) (map (second (coercion m)) rs')
  coercion _ (CoercionVar x) = CoercionVar x
  coercion m (Quantified v co c) = Quantified v (coercion m co) (coercion m c)

  atom m (Ref v t) = Ref v (gotype m t)
  atom _ x@Lit{} = x

  arm m a = a & armPtrn %~ ptrn m
              & armTy %~ gotype m
              & armBody %~ term m
              & armVars %~ map (_2 %~ gotype m)

  ptrn _ (Constr a) = Constr a
  ptrn m (Destr a p) = Destr a (capture m p)
  ptrn m (PatRecord fs) = PatRecord (map (second (capture m)) fs)
  ptrn m (PatValues xs) = PatValues (map (capture m) xs)
  ptrn _ l@PatLit{} = l
  ptrn _ l@PatWildcard = l

  capture m (Capture v ty) = Capture v (gotype m ty)

  gotype :: VarMap.Map (Type a) -> Type a -> Type a
  gotype = substituteInType

-- | Substitute a type variable with some other type inside a type
substituteInType :: IsVar a => VarMap.Map (Type a) -> Type a -> Type a
substituteInType = goMaybe where
  goMaybe m t | VarMap.null m = t
              | otherwise = gotype m t

  gotype m x@(VarTy v) = VarMap.findWithDefault x (toVar v) m
  gotype _ x@ConTy{} = x
  gotype m (ForallTy v c t) = ForallTy v (gotype m c) (goMaybe (remove v m) t) where
    remove (Relevant var) m = VarMap.delete (toVar var) m
    remove Irrelevant m = m
  gotype m (AppTy f x) = AppTy (gotype m f) (gotype m x)
  gotype m (RowsTy v rs) = RowsTy (gotype m v) (map (second (gotype m)) rs)
  gotype m (ExactRowsTy rs) = ExactRowsTy (map (second (gotype m)) rs)
  gotype m (ValuesTy xs) = ValuesTy (map (gotype m) xs)
  gotype _ StarTy = StarTy
  gotype _ NilTy = NilTy

-- | Substitute a type variable with some other type inside a coercion
substituteInCo :: IsVar a => VarMap.Map (Type a) -> Coercion a -> Coercion a
substituteInCo m c
  | VarMap.null m = c
  | otherwise = coercion c where

  coercion (SameRepr t t') = SameRepr (gotype t) (gotype t')
  coercion (Domain c) = Domain (coercion c)
  coercion (Codomain c) = Codomain (coercion c)
  coercion (Symmetry c) = Symmetry (coercion c)
  coercion (Application f x) = Application (coercion f) (coercion x)
  coercion (ExactRecord rs) = ExactRecord (map (second coercion) rs)
  coercion (Record c rs) = Record (coercion c) (map (second coercion) rs)
  coercion (Projection rs rs') = Projection (map (second coercion) rs) (map (second coercion) rs')
  coercion (CoercionVar x) = CoercionVar x
  coercion (Quantified v a c) = Quantified v (coercion a) (coercion c)

  gotype = substituteInType m

-- | Refresh every closed variable within a term, replacing it with some
-- fresh variable.
refresh :: (MonadNamey m, IsVar a) => Term a -> m (Term a)
refresh = refreshTerm mempty mempty where
  refreshTerm :: (MonadNamey m, IsVar a)
              => VarMap.Map (Atom a) -> VarMap.Map (Type a)
              -> Term a -> m (Term a)
  refreshTerm vm tm (Atom a) = pure $ Atom (substAtom vm tm a)
  refreshTerm vm tm (App f x) = pure $ App (substAtom vm tm f) (substAtom vm tm x)
  refreshTerm vm tm (TyApp f ty) = pure $ TyApp (substAtom vm tm f) (substituteInType tm ty)
  refreshTerm vm tm (Extend e bs) = pure $
    Extend (substAtom vm tm e) (map (trimap id (substituteInType tm) (substAtom vm tm)) bs)
  refreshTerm vm tm (Values xs) = pure $ Values (map (substAtom vm tm) xs)
  refreshTerm vm tm (Cast e c) = pure $ Cast (substAtom vm tm e) (substituteInCo tm c)

  refreshTerm vm tm (Lam (TermArgument v ty) b) = do
    v' <- freshFrom' v
    let ty' = substituteInType tm ty
        vm' = VarMap.insert (toVar v) (Ref v' ty') vm
    Lam (TermArgument v' ty') <$> refreshTerm vm' tm b
  refreshTerm vm tm (Lam (TypeArgument v ty) b) = do
    v' <- freshFrom' v
    let ty' = substituteInType tm ty
        tm' = VarMap.insert (toVar v) (VarTy v') tm
    Lam (TypeArgument v' ty') <$> refreshTerm vm tm' b

  refreshTerm vm tm (Let (One (v, ty, e)) b) = do
    v' <- freshFrom' v
    let ty' = substituteInType tm ty
        vm' = VarMap.insert (toVar v) (Ref v' ty') vm
    e' <- refreshTerm vm' tm e
    Let (One (v', ty', e')) <$> refreshTerm vm' tm b
  refreshTerm vm tm (Let (Many vs) r) = do
    (vm', vs') <- foldrM (\(v, ty, b) (m, vs') -> do
      v' <- freshFrom' v
      let ty' = substituteInType tm ty
      pure ( VarMap.insert (toVar v) (Ref v' ty') m
           , (v', ty', b):vs' )) (vm, []) vs

    vs'' <- traverse (third3A (refreshTerm vm' tm)) vs'
    Let (Many vs'') <$> refreshTerm vm' tm r

  refreshTerm vm tm (Match e branches) = Match (substAtom vm tm e) <$> traverse (refreshArm vm tm) branches where
    refreshArm :: (IsVar a, MonadNamey m)
               => VarMap.Map (Atom a) -> VarMap.Map (Type a)
               -> Arm a -> m (Arm a)
    refreshArm vm tm a = do
      (tm', ts) <- foldrM (\(v, ty) (tm, ts) -> do
        v' <- freshFrom' v
        let ty' = substituteInType tm ty
        pure (VarMap.insert (toVar v) (VarTy v') tm, (v', ty'):ts)) (tm, []) (a ^. armTyvars)
      (vm', vs) <- foldrM (\(v, ty) (vm, vs) -> do
        v' <- freshFrom' v
        let ty' = substituteInType tm' ty
        pure (VarMap.insert (toVar v) (Ref v' ty') vm, (v', ty'):vs)) (vm, []) (a ^. armVars)

      branch' <- refreshTerm vm' tm' (a ^. armBody)
      pure ( Arm { _armPtrn = substPattern vm' (a ^. armPtrn)
                 , _armTy = substituteInType tm' (a ^. armTy)
                 , _armBody = branch'
                 , _armVars = vs
                 , _armTyvars = ts } )

  substAtom :: IsVar a
            => VarMap.Map (Atom a) -> VarMap.Map (Type a)
            -> Atom a -> Atom a
  substAtom vm tm (Ref v ty) = fromMaybe (Ref v (substituteInType tm ty)) (VarMap.lookup (toVar v) vm)
  substAtom _ _ a@Lit{} = a

  substPattern :: IsVar a => VarMap.Map (Atom a) -> Pattern a -> Pattern a
  substPattern _ p@Constr{} = p
  substPattern vm (Destr c p) = Destr c (substCapture vm p)
  substPattern vm (PatRecord fs) = PatRecord (map (second (substCapture vm)) fs)
  substPattern vm (PatValues xs) = PatValues (map (substCapture vm) xs)
  substPattern _ p@PatLit{} = p
  substPattern _ p@PatWildcard = p

  substCapture :: IsVar a => VarMap.Map (Atom a) -> Capture a -> Capture a
  substCapture vm (Capture v _) =
    let Just (Ref v' ty') = VarMap.lookup (toVar v) vm
    in Capture v' ty'

-- | Get the variable bound by the given argument
argVar :: IsVar a => Argument a -> CoVar
argVar (TermArgument v _) = toVar v
argVar (TypeArgument v _) = toVar v

-- | Create a new fresh 'CoVar' with the same name as a previous
-- one.
freshFrom :: MonadNamey m => CoVar -> m CoVar
freshFrom (CoVar _ name dat) = do
  CoVar x _ _ <- fresh dat
  pure (CoVar x name dat)

-- | Create a new fresh variable with the same name as a previous
-- one.
freshFrom' :: (MonadNamey m, IsVar a) => a -> m a
freshFrom' x = fromVar <$> freshFrom (toVar x)

-- | Create a fresh 'CoVar'
fresh :: MonadNamey m => VarInfo -> m CoVar
fresh k = do
  ~(TgName nam x) <- genName
  pure (CoVar x nam k)

-- | Create a fresh variable
fresh' :: (MonadNamey m, IsVar a) => VarInfo -> m a
fresh' k = fromVar <$> fresh k
