{-# LANGUAGE ConstraintKinds, FlexibleContexts, LambdaCase #-}
module Types.Kinds
  ( resolveTyDeclKind
  , resolveKind
  , closeOverKind
  ) where

import Control.Monad.Writer.Strict hiding ((<>))
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Infer

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Traversable
import Data.Semigroup
import Data.Foldable

import Types.Wellformed
import Syntax.Raise
import Syntax

type Subst = Map.Map (Var Typed) (Kind Typed)

type KindT m = WriterT [(Kind Typed, Kind Typed)] m

type MonadSolve m
  = ( MonadReader Env m
    , MonadError TypeError m
    , MonadGen Int m)

solve :: MonadSolve m => [(Var Typed, Kind Typed)] -> m Subst
solve ((x, t):xs) = do
  xs' <- solve xs
  case Map.lookup x xs' of
    Just t' -> case unify t t' of
      Just new -> solve (xs ++ new)
      Nothing -> throwError (KindsNotEqual t t')
    Nothing -> pure $ Map.insert x (apply xs' t) (fmap (apply (Map.singleton x t)) xs')
solve [] = pure mempty

resolveTyDeclKind :: MonadSolve m => Var Resolved -> [Var Resolved] -> [Constructor Resolved] -> m (Kind Typed)
resolveTyDeclKind tp vs cs = fmap closeOverKind . resolve $ do
  ks <- replicateM (length vs) freshKV
  let kind = foldr KiArr KiStar ks
  extendKind (TvName tp, kind) $
    extendManyK (zip (map TvName vs) ks) $ do
      for_ cs $ \case
        UnitCon{} -> pure ()
        ArgCon _ t _ -> giveTp (raiseT TvName t)
      pure kind

resolveKind :: MonadSolve m => Type Resolved -> m (Type Typed, Kind Typed)
resolveKind t =
  let t' = raiseT TvName t
   in do
     kind <- resolve (inferKind t')
     pure (t', kind)

resolve :: MonadSolve m => KindT m (Kind Typed) -> m (Kind Typed)
resolve k = do
  (kind, cs) <- runWriterT k
  cs' <- for cs $ \(a, b) -> case unify a b of
    Just x -> pure x
    Nothing -> throwError (KindsNotEqual a b)
  subst <- solve (concat cs')
  pure $ apply subst kind

inferKind :: MonadSolve m => Type Typed -> KindT m (Kind Typed)
inferKind tp = do
  wellformed tp
  case tp of
    TyVar x -> do
      ki <- asks (Map.lookup (unTvName x) . types)
      maybe freshKV pure ki
    TyCon x -> do
      ki <- asks (Map.lookup (unTvName x) . types)
      case ki of
        Just ki' -> instantiateKind ki'
        Nothing -> throwError (NotInScope (unTvName x))
    TyForall vs t -> do
      ks <- replicateM (length vs) freshKV
      extendManyK (zip vs ks) $ inferKind t
    TyArr a b -> do
      giveTp a
      giveTp b
      pure KiStar
    TyApp t1 t2 -> do
      x <- freshKV
      k1 <- inferKind t1
      k2 <- inferKind t2
      same k1 (KiArr k2 x)
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

giveTp :: MonadSolve m => Type Typed -> KindT m ()
giveTp x = do
  x' <- inferKind x
  same x' KiStar

same :: Monad m => Kind Typed -> Kind Typed -> KindT m ()
same a b = tell [(a, b)]

unify :: Kind Typed -> Kind Typed -> Maybe [(Var Typed, Kind Typed)]
unify (KiVar v) (KiVar k)
  | v == k = pure []
unify (KiVar v) t = Just [(v, t)]
unify t (KiVar v) = Just [(v, t)]
unify (KiArr a b) (KiArr c d) = (++) <$> unify a c <*> unify b d
unify KiStar KiStar = Just []
unify _ _ = Nothing

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
