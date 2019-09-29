{-# LANGUAGE FlexibleInstances
  , FlexibleContexts
  , UndecidableInstances
  , FunctionalDependencies
  , TypeFamilies
  , ScopedTypeVariables
  #-}
module Syntax.Subst
  ( Subst
  , Substitutable
  , tyVarOcc, foldOccMap
  , ftv, nominalTvs
  , apply
  , compose
  , Map.fromList )
  where

import qualified Data.Map.Merge.Strict as Map
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Bifunctor

import Control.Lens

import Syntax.Type
import Syntax.Var

type Subst p = Map.Map (Var p) (Type p)

class Substitutable p a | a -> p where
  ftv :: a -> Set.Set (Var p)
  apply :: Subst p -> a -> a

instance Ord (Var p) => Substitutable p (Type p) where
  ftv TyCon{} = mempty
  ftv TyPromotedCon{} = mempty
  ftv TySkol{} = mempty
  ftv TyType{} = mempty
  ftv TyLit{} = mempty
  ftv (TyVar v) = Set.singleton v
  ftv (TyWildcard v) = foldMap ftv v
  ftv (TyApp a b) = ftv a <> ftv b
  ftv (TyTuple a b) = ftv a <> ftv b
  ftv (TyRows rho rows) = ftv rho <> foldMap (ftv . snd) rows
  ftv (TyExactRows rows) = foldMap (ftv . snd) rows
  ftv (TyWithConstraints eq b) = foldMap (\(a, b) -> ftv a <> ftv b) eq <> ftv b
  ftv (TyPi binder t) = ftv binder <> (ftv t Set.\\ bound binder)
  ftv (TyParens t) = ftv t
  ftv (TyOperator l _ r) = ftv l <> ftv r

  apply _ (TyCon a) = TyCon a
  apply _ (TyLit a) = TyLit a
  apply _ (TySkol x) = TySkol x
  apply _ (TyPromotedCon x) = TyPromotedCon x
  apply s (TyWildcard v) = mkWildTy (apply s <$> v)
  apply _ TyType = TyType
  apply s t@(TyVar v) = Map.findWithDefault t v s
  apply s (TyApp a b) = TyApp (apply s a) (apply s b)
  apply s (TyTuple a b) = TyTuple (apply s a) (apply s b)
  apply s (TyPi binder t) = TyPi (apply s binder) (apply s' t) where
    s' = foldr Map.delete s (Set.toList (bound binder))
  apply s (TyRows rho rows) = TyRows (apply s rho) (map (second (apply s)) rows)
  apply s (TyExactRows rows) = TyExactRows  (map (second (apply s)) rows)
  apply s (TyWithConstraints eq b) = TyWithConstraints (map (bimap (apply s) (apply s)) eq) (apply s b)
  apply s (TyParens t) = TyParens (apply s t)
  apply s (TyOperator l o r) = TyOperator (apply s l) o (apply s r)

instance Ord (Var p) => Substitutable p (Coercion p) where
  ftv VarCo{} = mempty
  ftv (ReflCo t) = ftv t
  ftv (AssumedCo t t') = ftv t <> ftv t'
  ftv (SymCo c) = ftv c
  ftv (AppCo f x) = ftv f <> ftv x
  ftv (ArrCo f x) = ftv f <> ftv x
  ftv (ProdCo f x) = ftv f <> ftv x
  ftv (ExactRowsCo rs) = foldMap (ftv . snd) rs
  ftv (RowsCo c rs) = ftv c <> foldMap (ftv . snd) rs
  ftv (ProjCo rs rs') = foldMap (ftv . snd) rs <> foldMap (ftv . snd) rs'
  ftv (ForallCo v t cs) = ftv t <> v `Set.delete` ftv cs

  apply _ x@VarCo{} = x
  apply s (ReflCo t) = ReflCo (apply s t)
  apply s (AssumedCo t t') = AssumedCo (apply s t) (apply s t')
  apply s (SymCo x) = SymCo (apply s x)
  apply s (AppCo f x) = AppCo (apply s f) (apply s x)
  apply s (ArrCo f x) = ArrCo (apply s f) (apply s x)
  apply s (ProdCo f x) = ProdCo (apply s f) (apply s x)
  apply s (ExactRowsCo rs) = ExactRowsCo (map (second (apply s)) rs)
  apply s (RowsCo c rs) = RowsCo (apply s c) (map (second (apply s)) rs)
  apply s (ProjCo rs rs') = ProjCo (map (second (apply s)) rs) (map (second (apply s)) rs')
  apply s (ForallCo v c cs) = ForallCo v (apply s c) (apply s' cs) where
    s' = Map.delete v s

instance (Ord (Var p), Substitutable p a) => Substitutable p [a] where
  ftv = foldMap ftv
  apply s = map (apply s)

instance (Ord (Var p), Substitutable p a) => Substitutable p (Seq.Seq a) where
  ftv = foldMap ftv
  apply s = fmap (apply s)

instance Ord (Var p) => Substitutable p (TyBinder p) where
  ftv (Anon t) = ftv t
  ftv (Implicit t) = ftv t
  ftv (Invisible _ k _) = foldMap ftv k

  apply s (Anon t) = Anon (apply s t)
  apply s (Implicit t) = Implicit (apply s t)
  apply s (Invisible v k spec) = Invisible v (fmap (apply s) k) spec

bound :: TyBinder p -> Set.Set (Var p)
bound Anon{} = Set.empty
bound Implicit{} = Set.empty
bound (Invisible v _ _) = Set.singleton v

compose :: Ord (Var p) => Subst p -> Subst p -> Subst p
s1 `compose` s2 = fmap (apply s1) s2 <> fmap (apply s2) s1

newtype OccMap p = OccMap (Map.Map (Var p) Int)

instance forall p. Ord (Var p) => Semigroup (OccMap p) where
  OccMap x <> OccMap y = OccMap (x `go` y) where
    go = Map.merge Map.preserveMissing Map.preserveMissing (Map.zipWithMatched (const (+)))
    go :: Map.Map (Var p) Int -> Map.Map (Var p) Int -> Map.Map (Var p) Int

instance Ord (Var p) => Monoid (OccMap p) where
  mempty = OccMap mempty

type instance IxValue (OccMap p) = Int
type instance Index (OccMap p) = Var p
instance Ord (Var p) => Ixed (OccMap p) where
  ix k f (OccMap m) = case Map.lookup k m of
    Just v  -> f v <&> \v' -> OccMap (Map.insert k v' m)
    Nothing -> pure (OccMap m)

instance Ord (Var p) => At (OccMap p) where
  at k f (OccMap m) = OccMap <$> Map.alterF f k m

singletonOcc :: Var p -> OccMap p
singletonOcc v = OccMap (Map.singleton v 1)

removeOccs :: Ord (Var p) => OccMap p -> Set.Set (Var p) -> OccMap p
removeOccs (OccMap m) s = OccMap (m `Map.withoutKeys` s)

foldOccMap :: (Var p -> Int -> b -> b) -> b -> OccMap p -> b
foldOccMap k b (OccMap m) = Map.foldrWithKey k b m

tyVarOcc :: Ord (Var p) => Type p -> OccMap p
tyVarOcc TyCon{} = mempty
tyVarOcc TyLit{} = mempty
tyVarOcc TyPromotedCon{} = mempty
tyVarOcc TySkol{} = mempty
tyVarOcc TyType{} = mempty
tyVarOcc TyWildcard{} = mempty
tyVarOcc (TyVar v) = singletonOcc v
tyVarOcc (TyApp a b) = tyVarOcc a <> tyVarOcc b
tyVarOcc (TyTuple a b) = tyVarOcc a <> tyVarOcc b
tyVarOcc (TyRows rho rows) = tyVarOcc rho <> foldMap (tyVarOcc . snd) rows
tyVarOcc (TyExactRows rows) = foldMap (tyVarOcc . snd) rows
tyVarOcc (TyParens t) = tyVarOcc t
tyVarOcc (TyOperator l _ r) = tyVarOcc l <> tyVarOcc r
tyVarOcc (TyWithConstraints eq b) = foldMap (\(a, b) -> tyVarOcc a <> tyVarOcc b) eq <> tyVarOcc b
tyVarOcc (TyPi binder t) = tyVarOcc' binder <> (tyVarOcc t `removeOccs` bound binder) where
  bound Anon{} = Set.empty
  bound Implicit{} = Set.empty
  bound (Invisible v _ _) = Set.singleton v
  tyVarOcc' (Anon t) = tyVarOcc t
  tyVarOcc' (Implicit t) = tyVarOcc t
  tyVarOcc' (Invisible _ k _) = foldMap tyVarOcc k

nominalTvs :: Ord (Var p) => Type p -> Set.Set (Var p)
nominalTvs TyCon{} = mempty
nominalTvs TyPromotedCon{} = mempty
nominalTvs TySkol{} = mempty
nominalTvs TyType{} = mempty
nominalTvs TyLit{} = mempty
nominalTvs (TyVar v) = Set.singleton v
nominalTvs (TyWildcard _) = mempty
nominalTvs (TyApp a b) = nominalTvs a <> nominalTvs b
nominalTvs (TyTuple a b) = nominalTvs a <> nominalTvs b
nominalTvs (TyRows rho rows) = nominalTvs rho <> foldMap (nominalTvs . snd) rows
nominalTvs (TyExactRows rows) = foldMap (nominalTvs . snd) rows
nominalTvs (TyParens t) = nominalTvs t
nominalTvs (TyOperator l _ r) = nominalTvs l <> nominalTvs r
nominalTvs (TyWithConstraints eq b) = foldMap (\(a, b) -> nominalTvs a <> nominalTvs b) eq <> nominalTvs b
nominalTvs (TyPi binder t) = nominalOf binder <> (nominalTvs t Set.\\ bound binder) where
  nominalOf (Anon t) = nominalTvs t
  nominalOf (Implicit t) = nominalTvs t
  nominalOf (Invisible _ k _) = foldMap nominalTvs k
