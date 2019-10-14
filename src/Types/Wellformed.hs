{-# LANGUAGE FlexibleContexts, TypeFamilies, ScopedTypeVariables, UndecidableInstances #-}
module Types.Wellformed (wellformed, skols, Skolem(..)) where

import Control.Monad.Infer

import qualified Data.Set as Set
import Data.Foldable

import Syntax

wellformed :: (MonadChronicles TypeError m, MonadReader Env m) => Type Typed -> m ()
wellformed tp = case tp of
  TyCon{} -> pure ()
  TyVar{} -> pure ()
  TySkol{} -> pure ()
  TyType{} -> pure ()
  TyPromotedCon{} -> pure ()
  TyWildcard{} -> pure ()
  TyLit{} -> pure ()
  TyPi a b -> do
    case a of
      Invisible _ k _ -> traverse_ wellformed k
      Anon a -> wellformed a
      Implicit a -> wellformed a
    wellformed b
  TyApp a b -> wellformed a *> wellformed b
  TyTuple a b -> wellformed a *> wellformed b
  TyRows rho rows -> do
    case rho of
      TyRows{} -> pure ()
      TyExactRows{} -> pure ()
      TyVar{} -> pure ()
      _ -> confesses (CanNotInstance tp rho)
    traverse_ (wellformed . snd) rows
  TyExactRows rows -> traverse_ (wellformed . snd) rows
  TyWithConstraints eqs b -> do
    for_ eqs $ \(a, b) -> wellformed a *> wellformed b
    wellformed b
  TyParens t -> wellformed t
  TyOperator l _ r -> wellformed l *> wellformed r

skols :: (Show (Var p), Ord (Var p)) => Type p -> Set.Set (Skolem p)
skols TyCon{}  = mempty
skols TyLit{}  = mempty
skols TyVar{}  = mempty
skols TyType{} = mempty
skols TyPromotedCon{}  = mempty
skols TyWildcard{}  = mempty
skols (TySkol x) = Set.singleton x
skols (TyApp a b) = skols a <> skols b
skols (TyPi b t)
  | Invisible v k _ <- b =
    Set.filter (\(Skolem _ v' _ _) -> v /= v') (foldMap skols k <> skols t)
  | Anon a <- b = skols a <> skols t
  | Implicit a <- b = skols a <> skols t
skols (TyRows r rs) = skols r <> foldMap (skols . snd) rs
skols (TyExactRows rs) = foldMap (skols . snd) rs
skols (TyTuple a b) = skols a <> skols b
skols (TyWithConstraints cs a) =
  foldMap (\(x, y) -> skols x <> skols y) cs <> skols a
skols (TyParens t) = skols t
skols (TyOperator l _ r) = skols l <> skols r
