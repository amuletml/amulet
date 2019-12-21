{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}

-- | A collection of utility methods for the optimiser.
module Core.Optimise
  ( OptimiseInfo(..)
  , substitute, substituteInTys, substituteInType, substituteInCo
  , module Core.Core
  , module Core.Var
  , refresh, fresh, fresh', freshFrom, freshFrom'
  , argVar
  , Call, Arg(..)
  , findCalls, splitLams
  ) where

import Control.Arrow (second)
import Control.Monad.Namey
import Control.Lens

import qualified Data.VarMap as VarMap
import qualified Data.VarSet as VarSet
import Data.Foldable
import Data.Triple
import Data.Maybe

import Core.Core
import Core.Var

import Syntax.Var

-- | Options which guide the Core optimiser.
data OptimiseInfo = OptimiseInfo
  { -- | Names which are exported, and so should be preserved across
    -- optimisation.
    exportNames :: VarSet.Set
    -- | Whether to run Core lint during optimisation.
  , useLint :: Bool
  }
  deriving Show

-- | Substitute a variable with some other atom
substitute :: VarMap.Map Atom -> Term a -> Term a
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
  term (Cast f to co) = Cast (atom f) to co

  atom x@(Ref v _) = VarMap.findWithDefault x (toVar v) m
  atom x@Lit{} = x

  arm = armBody %~ term

-- | Substitute a type variable with some other type inside terms
substituteInTys :: forall a b. IsVar a => VarMap.Map Type -> AnnTerm b a -> AnnTerm b a
substituteInTys = term where
  term :: VarMap.Map Type -> AnnTerm b a -> AnnTerm b a
  term m a | VarMap.null m = a
  term m (AnnAtom z a) = AnnAtom z (atom m a)
  term m (AnnApp z f x) = AnnApp z (atom m f) (atom m x)
  term m (AnnLet z (One (v, t, e)) x) = AnnLet z (One (v, gotype m t, term m e)) (term m x)
  term m (AnnLet z (Many vs) x) = AnnLet z (Many (map (trimap id (gotype m) (term m)) vs)) (term m x)
  term m (AnnMatch z x vs) = AnnMatch z (atom m x) (map (arm m) vs)
  term m (AnnExtend z e rs) = AnnExtend z (atom m e) (map (trimap id (gotype m) (atom m)) rs)
  term m (AnnValues z xs) = AnnValues z (map (atom m) xs)
  term m (AnnTyApp z f t) = AnnTyApp z (atom m f) (gotype m t)
  term m (AnnCast z f to co) = AnnCast z (atom m f) (gotype m to) (coercion m co)
  term m (AnnLam z arg b) = AnnLam z (go arg) (term (delete m) b) where
    go (TermArgument v t) = TermArgument v (gotype m t)
    go (TypeArgument v t) = TypeArgument v (gotype m t)
    delete = case arg of
      TypeArgument v _ -> VarMap.delete (toVar v)
      _ -> id

  coercion m (SameRepr t t') = SameRepr (gotype m t) (gotype m t')
  coercion m (Symmetry c) = Symmetry (coercion m c)
  coercion m (Trans x y) = Trans (coercion m x) (coercion m y)
  coercion m (Application f x) = Application (coercion m f) (coercion m x)
  coercion m (ExactRecord rs) = ExactRecord (map (second (coercion m)) rs)
  coercion m (Record c rs) = Record (coercion m c) (map (second (coercion m)) rs)
  coercion m (Projection rs rs') = Projection (map (second (coercion m)) rs) (map (second (coercion m)) rs')
  coercion _ (CoercionVar x) = CoercionVar x
  coercion m (Quantified v co c) = Quantified v (coercion m co) (coercion m c)
  coercion _ (Nth co i) = Nth co i
  coercion m (Axiom ax co) = Axiom ax (map (coercion m) co)

  atom m (Ref v t) = Ref v (gotype m t)
  atom _ x@Lit{} = x

  arm m a = a & armPtrn %~ ptrn m
              & armTy %~ gotype m
              & armBody %~ term m
              & armVars %~ map (_2 %~ gotype m)

  ptrn _ (Constr a) = Constr a
  ptrn m (Destr a p) = Destr a (map (capture m) p)
  ptrn m (PatRecord fs) = PatRecord (map (second (capture m)) fs)
  ptrn m (PatValues xs) = PatValues (map (capture m) xs)
  ptrn _ l@PatLit{} = l
  ptrn _ l@PatWildcard = l

  capture m (Capture v ty) = Capture v (gotype m ty)

  gotype :: VarMap.Map Type -> Type -> Type
  gotype = substituteInType

-- | Substitute a type variable with some other type inside a type
substituteInType :: VarMap.Map Type -> Type -> Type
substituteInType = goMaybe where
  goMaybe m t | VarMap.null m = t
              | otherwise = gotype m t

  gotype m x@(VarTy v) = VarMap.findWithDefault x v m
  gotype _ x@ConTy{} = x
  gotype m (ForallTy v c t) = ForallTy v (gotype m c) (goMaybe (remove v m) t) where
    remove (Relevant var) m = VarMap.delete var m
    remove Irrelevant m = m
  gotype m (AppTy f x) = AppTy (gotype m f) (gotype m x)
  gotype m (RowsTy v rs) = RowsTy (gotype m v) (map (second (gotype m)) rs)
  gotype m (ExactRowsTy rs) = ExactRowsTy (map (second (gotype m)) rs)
  gotype m (ValuesTy xs) = ValuesTy (map (gotype m) xs)
  gotype _ StarTy = StarTy
  gotype _ NilTy = NilTy

-- | Substitute a type variable with some other type inside a coercion
substituteInCo :: VarMap.Map Type -> Coercion -> Coercion
substituteInCo m c
  | VarMap.null m = c
  | otherwise = coercion c where

  coercion (SameRepr t t') = SameRepr (gotype t) (gotype t')
  coercion (Symmetry c) = Symmetry (coercion c)
  coercion (Trans x y) = Trans (coercion x) (coercion y)
  coercion (Application f x) = Application (coercion f) (coercion x)
  coercion (ExactRecord rs) = ExactRecord (map (second coercion) rs)
  coercion (Record c rs) = Record (coercion c) (map (second coercion) rs)
  coercion (Projection rs rs') = Projection (map (second coercion) rs) (map (second coercion) rs')
  coercion (CoercionVar x) = CoercionVar x
  coercion (Quantified v a c) = Quantified v (coercion a) (coercion c)
  coercion (Nth co i) = Nth co i
  coercion (Axiom ax co) = Axiom ax (map coercion co)

  gotype = substituteInType m

-- | Refresh every closed variable within a term, replacing it with some
-- fresh variable.
refresh :: (MonadNamey m, IsVar a) => Term a -> m (Term a)
refresh = refreshTerm mempty mempty where
  refreshTerm :: (MonadNamey m, IsVar a)
              => VarMap.Map Atom -> VarMap.Map Type
              -> Term a -> m (Term a)
  refreshTerm vm tm (Atom a) = pure $ Atom (substAtom vm tm a)
  refreshTerm vm tm (App f x) = pure $ App (substAtom vm tm f) (substAtom vm tm x)
  refreshTerm vm tm (TyApp f ty) = pure $ TyApp (substAtom vm tm f) (substituteInType tm ty)
  refreshTerm vm tm (Extend e bs) = pure $
    Extend (substAtom vm tm e) (map (trimap id (substituteInType tm) (substAtom vm tm)) bs)
  refreshTerm vm tm (Values xs) = pure $ Values (map (substAtom vm tm) xs)
  refreshTerm vm tm (Cast e t c) = pure $ Cast (substAtom vm tm e) (substituteInType tm t) (substituteInCo tm c)

  refreshTerm vm tm (Lam (TermArgument v ty) b) = do
    v' <- freshFrom' v
    let ty' = substituteInType tm ty
        vm' = VarMap.insert (toVar v) (Ref v' ty') vm
    Lam (TermArgument (fromVar v') ty') <$> refreshTerm vm' tm b
  refreshTerm vm tm (Lam (TypeArgument v ty) b) = do
    v' <- freshFrom' v
    let ty' = substituteInType tm ty
        tm' = VarMap.insert (toVar v) (VarTy v') tm
    Lam (TypeArgument (fromVar v') ty') <$> refreshTerm vm tm' b

  refreshTerm vm tm (Let (One (v, ty, e)) b) = do
    v' <- freshFrom' v
    let ty' = substituteInType tm ty
        vm' = VarMap.insert (toVar v) (Ref v' ty') vm
    e' <- refreshTerm vm' tm e
    Let (One (fromVar v', ty', e')) <$> refreshTerm vm' tm b
  refreshTerm vm tm (Let (Many vs) r) = do
    (vm', vs') <- foldrM (\(v, ty, b) (m, vs') -> do
      v' <- freshFrom' v
      let ty' = substituteInType tm ty
      pure ( VarMap.insert (toVar v) (Ref v' ty') m
           , (fromVar v', ty', b):vs' )) (vm, []) vs

    vs'' <- traverse (third3A (refreshTerm vm' tm)) vs'
    Let (Many vs'') <$> refreshTerm vm' tm r

  refreshTerm vm tm (Match e branches) = Match (substAtom vm tm e) <$> traverse (refreshArm vm tm) branches where
    refreshArm :: (IsVar a, MonadNamey m)
               => VarMap.Map Atom -> VarMap.Map Type
               -> Arm a -> m (Arm a)
    refreshArm vm tm a = do
      (tm', ts) <- foldrM (\(v, ty) (tm, ts) -> do
        v' <- freshFrom' v
        let ty' = substituteInType tm ty
        pure (VarMap.insert (toVar v) (VarTy v') tm, (fromVar v', ty'):ts)) (tm, []) (a ^. armTyvars)
      (vm', vs) <- foldrM (\(v, ty) (vm, vs) -> do
        v' <- freshFrom' v
        let ty' = substituteInType tm' ty
        pure (VarMap.insert (toVar v) (Ref v' ty') vm, (fromVar v', ty'):vs)) (vm, []) (a ^. armVars)

      branch' <- refreshTerm vm' tm' (a ^. armBody)
      pure ( Arm { _armPtrn = substPattern vm' (a ^. armPtrn)
                 , _armTy = substituteInType tm' (a ^. armTy)
                 , _armBody = branch'
                 , _armVars = vs
                 , _armTyvars = ts } )

  substAtom :: VarMap.Map Atom -> VarMap.Map Type
            -> Atom -> Atom
  substAtom vm tm (Ref v ty) = fromMaybe (Ref v (substituteInType tm ty)) (VarMap.lookup v vm)
  substAtom _ _ a@Lit{} = a

  substPattern :: IsVar a => VarMap.Map Atom -> Pattern a -> Pattern a
  substPattern _ p@Constr{} = p
  substPattern vm (Destr c p) = Destr c (map (substCapture vm) p)
  substPattern vm (PatRecord fs) = PatRecord (map (second (substCapture vm)) fs)
  substPattern vm (PatValues xs) = PatValues (map (substCapture vm) xs)
  substPattern _ p@PatLit{} = p
  substPattern _ p@PatWildcard = p

  substCapture :: IsVar a => VarMap.Map Atom -> Capture a -> Capture a
  substCapture vm (Capture v _) =
    let Just (Ref v' ty') = VarMap.lookup (toVar v) vm
    in Capture (fromVar v') ty'

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
freshFrom' :: (MonadNamey m, IsVar a, IsVar b) => a -> m b
freshFrom' x = fromVar <$> freshFrom (toVar x)

-- | Create a fresh 'CoVar'
fresh :: MonadNamey m => VarInfo -> m CoVar
fresh k = do
  ~(TgName _ x) <- genName
  pure (CoVar x Nothing k)

-- | Create a fresh variable
fresh' :: (MonadNamey m, IsVar a) => VarInfo -> m a
fresh' k = fromVar <$> fresh k

-- | Find *all* uses of a given variable in a term. Note that this
-- includes unsaturated uses, too; If you need only the saturated
-- arguments, filter the list based on length.
findCalls :: forall a. IsVar a => a -> Term a -> [Call]
findCalls func_var term = filter (not . null) . map reverse $ call ++ concat (toList calls) where
  fn = toVar func_var
  (calls, call) = findCallsWk (VarMap.singleton fn [[]]) term

  maybe_L empty _ [] = empty
  maybe_L _ list xs = list xs

  findCallsWk :: VarMap.Map [Call] -> Term a -> (VarMap.Map [Call], [Call])
  findCallsWk calls (Let (One (v, _, term)) rest) =
    let (calls', call) = findCallsWk calls term
        calls'' = maybe_L calls' (\x -> VarMap.insert (toVar v) x calls') call
     in findCallsWk calls'' rest

  findCallsWk calls (Let (Many vs) rest) =
    let go_bind (v, _, term) =
          let (calls', call) = findCallsWk calls term
           in maybe_L calls' (\x -> VarMap.insert (toVar v) x calls') call
        calls'' = foldMap go_bind vs
     in findCallsWk calls'' rest

  findCallsWk calls (App (Ref f _) x)
    | Just call <- VarMap.lookup f calls = (calls, map (TermArg x:) call)

  findCallsWk calls (TyApp (Ref f _) x)
    | Just call <- VarMap.lookup f calls = (calls, map (TyArg x:) call)

  findCallsWk calls (Match _ as) =
    let (calls_map, result) = unzip $ map (findCallsWk calls . view armBody) as
     in (mconcat calls_map, concat result)

  findCallsWk calls (Lam _ rest) = findCallsWk calls rest

  findCallsWk calls _ = (calls, [])

type Call = [Arg]
data Arg = TyArg Type | TermArg Atom
  deriving (Eq, Show, Ord)

-- | Peel off all lambdas that prefix a term.
splitLams :: Term a -> ([Argument a], Term a)
splitLams (Lam x t) | (as, t) <- splitLams t = (x:as, t)
splitLams t = ([], t)
