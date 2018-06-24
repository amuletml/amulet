{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A collection of utility methods for the optimiser.
module Core.Optimise
  ( substitute, substituteInTys, substituteInType, substituteInCo
  , module Core.Core
  , module Core.Var
  , refresh, fresh, freshFrom, freshFrom'
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
  term (Let (One v) x) = Let (One (third3 term v)) (term x)
  term (Let (Many vs) x) = Let (Many (map (third3 term) vs)) (term x)
  term (Match x vs) = Match (atom x) (map arm vs)
  term (Extend e rs) = Extend (atom e) (map (third3 atom) rs)
  term (TyApp f t) = TyApp (atom f) t
  term (Cast f t) = Cast (atom f) t

  atom x@(Ref v _) = VarMap.findWithDefault x (toVar v) m
  atom (Lam v b) = Lam v (term b)
  atom x@Lit{} = x

  arm = armBody %~ term

-- | Substitute a type variable with some other type inside terms
substituteInTys :: forall a. IsVar a => VarMap.Map (Type a) -> Term a -> Term a
substituteInTys = term where
  term :: VarMap.Map (Type a) -> Term a -> Term a
  term m (Atom a) = Atom (atom m a)
  term m (App f x) = App (atom m f) (atom m x)
  term m (Let (One (v, t, e)) x) = Let (One (v, gotype m t, term m e)) (term m x)
  term m (Let (Many vs) x) = Let (Many (map (trimap id (gotype m) (term m)) vs)) (term m x)
  term m (Match x vs) = Match (atom m x) (map (arm m) vs)
  term m (Extend e rs) = Extend (atom m e) (map (trimap id (gotype m) (atom m)) rs)
  term m (TyApp f t) = TyApp (atom m f) (gotype m t)
  term m (Cast f t) = Cast (atom m f) (coercion m t)

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
  atom m (Lam arg b) = Lam (go arg) (term (delete m) b) where
    go (TermArgument v t) = TermArgument v (gotype m t)
    go (TypeArgument v t) = TypeArgument v (gotype m t)
    delete = case arg of
      TypeArgument v _ -> VarMap.delete (toVar v)
      _ -> id
  atom _ x@Lit{} = x

  arm m a = a & armPtrn %~ ptrn m
              & armTy %~ gotype m
              & armBody %~ term m
              & armVars %~ map (_2 %~ gotype m)

  ptrn m (Capture a ty) = Capture a (gotype m ty)
  ptrn _ (Constr a) = Constr a
  ptrn m (Destr a p) = Destr a (ptrn m p)
  ptrn m (PatExtend f fs) = PatExtend (ptrn m f) (map (second (ptrn m)) fs)
  ptrn _ l@PatLit{} = l

  gotype = substituteInType

-- | Substitute a type variable with some other type inside a type
substituteInType :: IsVar a => VarMap.Map (Type a) -> Type a -> Type a
substituteInType = gotype where
  gotype m x@(VarTy v) = VarMap.findWithDefault x (toVar v) m
  gotype _ x@ConTy{} = x
  gotype m (ForallTy v c t) = ForallTy v (gotype m c) (gotype (remove v m) t) where
    remove (Relevant var) m = VarMap.delete (toVar var) m
    remove Irrelevant m = m
  gotype m (AppTy f x) = AppTy (gotype m f) (gotype m x)
  gotype m (RowsTy v rs) = RowsTy (gotype m v) (map (second (gotype m)) rs)
  gotype m (ExactRowsTy rs) = ExactRowsTy (map (second (gotype m)) rs)
  gotype _ StarTy = StarTy
  gotype _ NilTy = NilTy

-- | Substitute a type variable with some other type inside a coercion
substituteInCo :: IsVar a => VarMap.Map (Type a) -> Coercion a -> Coercion a
substituteInCo m = coercion where
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
refresh = refreshTerm mempty where
  refreshAtom :: (MonadNamey m, IsVar a) => VarMap.Map a -> Atom a -> m (Atom a)
  refreshAtom s (Ref v ty) =
    let v' = fromMaybe v (VarMap.lookup (toVar v) s)
        ty' = refreshType s ty
    in pure (Ref v' ty')
  refreshAtom _ a@Lit{} = pure a
  refreshAtom s (Lam arg b) = do
    (arg', v') <- refreshArg s arg
    Lam arg' <$> refreshTerm (VarMap.insert (argVar arg) v' s) b

  refreshArg :: (MonadNamey m, IsVar a) => VarMap.Map a -> Argument a -> m (Argument a, a)
  refreshArg s (TermArgument n ty) = do
    let ty' = refreshType s ty
    v' <- freshFrom' n
    pure (TermArgument v' ty', v')
  refreshArg s (TypeArgument n ty) = do
    let ty' = refreshType s ty
    v' <- freshFrom' n
    pure (TypeArgument v' ty', v')

  refreshTerm :: (MonadNamey m, IsVar a) => VarMap.Map a -> Term a -> m (Term a)
  refreshTerm s (Atom a) = Atom <$> refreshAtom s a
  refreshTerm s (App f x) = App <$> refreshAtom s f <*> refreshAtom s x
  refreshTerm s (TyApp f ty) = TyApp <$> refreshAtom s f <*> pure (refreshType s ty)
  refreshTerm s (Let (One (v, ty, e)) b) = do
    v' <- freshFrom' v
    let s' = VarMap.insert (toVar v) v' s
    e' <- refreshTerm s' e
    Let (One (v', refreshType s' ty, e')) <$> refreshTerm s' b
  refreshTerm s (Let (Many vs) b) = do
    s' <- foldrM (\(v, _, _) m -> do
                     v' <- freshFrom' v
                     pure (VarMap.insert (toVar v) v' m)) s vs
    vs' <- traverse (trimapA (pure . get s') (pure . refreshType s') (refreshTerm s')) vs
    Let (Many vs') <$> refreshTerm s' b
  refreshTerm s (Match e branches) = Match <$> refreshAtom s e <*> refreshArms s branches where
    refreshArm :: (IsVar a, MonadNamey m) => VarMap.Map a -> Arm a -> m (Arm a, VarMap.Map a)
    refreshArm s a@Arm{ _armPtrn = test, _armBody = branch } = do
      s' <- refreshVs (a ^. armTyvars) s >>= refreshVs (a ^. armVars)
      branch' <- refreshTerm s' branch
      pure ( Arm { _armPtrn = refreshPattern s' test
                 , _armTy = refreshType s' (a ^. armTy)
                 , _armBody = branch'
                 , _armVars = map (\(v, ty) -> (get s' v, refreshType s' ty)) (a ^. armVars)
                 , _armTyvars = map (\(v, ty) -> (get s' v, refreshType s' ty)) (a ^. armTyvars)
                 }
           , s' )

    refreshArms :: (IsVar a, MonadNamey m) => VarMap.Map a -> [Arm a] -> m [Arm a]
    refreshArms s (a:as) = do
      (a', s) <- refreshArm s a
      (a':) <$> refreshArms s as
    refreshArms _ [] = pure []

    refreshVs :: (MonadNamey m, IsVar a) => [(a, Type a)] -> VarMap.Map a -> m (VarMap.Map a)
    refreshVs = flip (foldrM refreshV)

    refreshV :: (MonadNamey m, IsVar a) => (a, Type a) -> VarMap.Map a -> m (VarMap.Map a)
    refreshV (v, _) m =
      case VarMap.lookup (toVar v) m of
        Just{} -> pure m
        Nothing -> do
          v' <- freshFrom' v
          pure (VarMap.insert (toVar v) v' m)

  refreshTerm s (Extend e bs) = Extend <$> refreshAtom s e <*> traverse (trimapA pure (pure . refreshType s) (refreshAtom s)) bs
  refreshTerm s (Cast e c) = Cast <$> refreshAtom s e <*> pure (refreshCoercion s c)

  refreshPattern :: IsVar a => VarMap.Map a -> Pattern a -> Pattern a
  refreshPattern s (Capture v ty) = Capture (get s v) (refreshType s ty)
  refreshPattern _ p@Constr{} = p
  refreshPattern s (Destr c p) = Destr c (refreshPattern s p)
  refreshPattern s (PatExtend p fs) = PatExtend (refreshPattern s p) (map (second (refreshPattern s)) fs)
  refreshPattern _ p@PatLit{} = p

  refreshType :: IsVar a => VarMap.Map a -> Type a -> Type a
  refreshType s x@(VarTy v) = maybe x VarTy (VarMap.lookup (toVar v) s)
  refreshType _ x@ConTy{} = x
  refreshType s (ForallTy Irrelevant c t) = ForallTy Irrelevant (refreshType s c) (refreshType s t)
  refreshType s (ForallTy b@(Relevant v) c t) =
    let s' = VarMap.delete (toVar v) s
    in ForallTy b (refreshType s c) (refreshType s' t)
  refreshType s (AppTy f x) = AppTy (refreshType s f) (refreshType s x)
  refreshType s (RowsTy v rs) = RowsTy (refreshType s v) (map (second (refreshType s)) rs)
  refreshType s (ExactRowsTy rs) = ExactRowsTy (map (second (refreshType s)) rs)
  refreshType _ StarTy = StarTy
  refreshType _ NilTy = NilTy

  refreshCoercion :: IsVar a => VarMap.Map a -> Coercion a -> Coercion a
  refreshCoercion s (SameRepr t t') = SameRepr (refreshType s t) (refreshType s t')
  refreshCoercion s (Domain c) = Domain (refreshCoercion s c)
  refreshCoercion s (Codomain c) = Codomain (refreshCoercion s c)
  refreshCoercion s (Symmetry c) = Symmetry (refreshCoercion s c)
  refreshCoercion s (Application f x) = Application (refreshCoercion s f) (refreshCoercion s x)
  refreshCoercion s (ExactRecord rs) = ExactRecord (map (second (refreshCoercion s)) rs)
  refreshCoercion s (Record c rs) = Record (refreshCoercion s c) (map (second (refreshCoercion s)) rs)
  refreshCoercion s (Projection rs rs') = Projection (map (second (refreshCoercion s)) rs) (map (second (refreshCoercion s)) rs')
  refreshCoercion s x@(CoercionVar v) = maybe x CoercionVar (VarMap.lookup (toVar v) s)
  refreshCoercion s (Quantified v a c) = Quantified v (refreshCoercion s a) (refreshCoercion s c)

  get s v = fromJust (VarMap.lookup (toVar v) s)

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

-- | Create a new fresh variable
fresh :: MonadNamey m => VarInfo -> m CoVar
fresh k = do
  TgName nam x <- genName
  pure (CoVar x nam k)
