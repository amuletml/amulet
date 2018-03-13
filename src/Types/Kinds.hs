{-# LANGUAGE ConstraintKinds, FlexibleContexts, LambdaCase #-}
module Types.Kinds
  ( resolveTyDeclKind
  , resolveKind
  , closeOverKind
  ) where

import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Infer

import Control.Lens

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Semigroup
import Data.Foldable

import qualified Types.Unify as U
import Types.Wellformed
import Syntax.Raise
import Syntax

type Subst = Map.Map (Var Typed) (Kind Typed)

type KindT m = StateT Subst m

type MonadSolve m
  = ( MonadReader Env m
    , MonadError TypeError m
    , MonadGen Int m
    )

resolveTyDeclKind :: MonadSolve m => Var Resolved -> [Var Resolved] -> [Constructor Resolved] -> m (Kind Typed)
resolveTyDeclKind tp vs cs = fmap closeOverKind . resolve $ do
  ks <- replicateM (length vs) freshKV
  let kind = foldr KiArr KiStar ks
  extendKind (TvName tp, kind) $
    extendManyK (zip (map TvName vs) ks) $ do
      for_ cs $ \case
        UnitCon{} -> pure ()
        ArgCon _ t _ -> giveTp (raiseT TvName t)
        c@(GeneralisedCon _ t _) -> inferGadtConKind c t (foldl TyApp (TyCon tp) (map TyVar vs))
      pure kind

resolveKind :: MonadSolve m => Type Resolved -> m (Type Typed, Kind Typed)
resolveKind t =
  let t' = raiseT TvName t
   in do
     kind <- resolve (inferKind t')
     pure (t', kind)

resolve :: MonadSolve m => KindT m (Kind Typed) -> m (Kind Typed)
resolve k = do
  (kind, subst) <- runStateT k mempty
  pure $ apply subst kind

inferKind :: MonadSolve m => Type Typed -> KindT m (Kind Typed)
inferKind tp = do
  wellformed tp
  case tp of
    TyVar x -> do
      ki <- view (types . at (unTvName x))
      maybe freshKV pure ki
    TySkol (Skolem x _ _) -> do
      ki <- view (types . at (unTvName x))
      maybe freshKV pure ki
    TyCon x -> do
      ki <- view (types . at (unTvName x))
      case ki of
        Just ki' -> instantiateKind ki'
        Nothing -> throwError (NotInScope (unTvName x))
    TyForall vs t -> do
      ks <- replicateM (length vs) freshKV
      extendManyK (zip vs ks) $ do
        k <- inferKind t
        unify k KiStar
        pure k
    TyArr a b -> do
      giveTp a
      giveTp b
      pure KiStar
    TyApp t1 t2 -> do
      x <- freshKV
      k1 <- inferKind t1
      k2 <- inferKind t2
      unify k1 (KiArr k2 x)
      case t2 of
        TyForall{} -> throwError (ImpredicativeApp t1 t2)
        _ -> pure ()
      pure x
    TyRows rho x -> do
      giveTp rho
      for_ x $ \(_, t) -> giveTp t
      pure KiStar
    TyExactRows x -> KiStar <$ for_ x (\(_, t) -> giveTp t)
    TyTuple a b -> do
      giveTp a
      giveTp b
      pure KiStar
    TyWithConstraints eq b -> do
      for_ eq $ \(a, b) -> do
        _ <- inferKind a
        () <$ inferKind b
      inferKind b

giveTp :: MonadSolve m => Type Typed -> KindT m ()
giveTp x = do
  x' <- inferKind x
  unify x' KiStar

unify :: (MonadState Subst m, MonadError TypeError m) => Kind Typed -> Kind Typed -> m ()
unify (KiVar v) (KiVar k)
  | v == k = pure ()
unify (KiVar v) t = bind v t
unify t (KiVar v) = bind v t
unify (KiArr a b) (KiArr c d) = unify a c *> unify b d
unify KiStar KiStar = pure ()
unify x y = throwError (KindsNotEqual x y)

bind :: (MonadState Subst m, MonadError TypeError m) => Var Typed -> Kind Typed -> m ()
bind v t = do
  x <- get
  case Map.lookup v x of
    Just t' -> unify t' t
    Nothing -> put (Map.singleton v t <> fmap (apply (Map.singleton v t)) x)

apply :: Map.Map (Var Typed) (Kind Typed) -> Kind Typed -> Kind Typed
apply m t@(KiVar v) = Map.findWithDefault t v m
apply m (KiArr a b) = KiArr (apply m a) (apply m b)
apply _ x = x

freeIn :: Ord (Var p) => Kind p -> Set.Set (Var p)
freeIn (KiVar v) = Set.singleton v
freeIn (KiArr a b) = freeIn a <> freeIn b
freeIn (KiForall vs k) = freeIn k Set.\\ Set.fromList vs
freeIn KiStar = mempty

closeOverKind :: Ord (Var p) => Kind p -> Kind p
closeOverKind = flip forall <*> fv where
  fv = toList . freeIn
  forall :: [Var p] -> Kind p -> Kind p
  forall [] x = x
  forall vs x = KiForall vs x

instantiateKind :: MonadSolve m => Kind Typed -> KindT m (Kind Typed)
instantiateKind (KiForall v x) = do
  fkvs <- replicateM (length v) freshKV
  pure (apply (Map.fromList (zip v fkvs)) x)
instantiateKind x = pure x

inferGadtConKind :: MonadSolve m => Constructor Resolved -> Type Resolved -> Type Resolved -> KindT m ()
inferGadtConKind c t ret' =
  let t' = raiseT TvName t
      ret = raiseT TvName ret'
      generalise (TyForall v t) = TyForall v <$> generalise t
      generalise (TyArr a t) = TyArr a <$> generalise t
      generalise ty = case U.solve 1 mempty [ConUnify (BecauseOf c) ret ty] of
        Right x -> do
          for_ (Map.toAscList x) $ \(a, b) -> do
            giveTp (TyVar a)
            giveTp b
          pure ret
        Left e -> throwError e
     in do
       giveTp t'
       _ <- generalise t'
       pure ()
