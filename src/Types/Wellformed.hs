{-# LANGUAGE FlexibleContexts, TypeFamilies, ScopedTypeVariables, UndecidableInstances #-}
module Types.Wellformed (wellformed, arity, normType, skols, Skolem(..)) where

import Control.Monad.Infer

import qualified Data.Set as Set
import Data.Foldable
import Data.Function
import Data.List (unionBy)

import Syntax

wellformed :: (MonadChronicles TypeError m, MonadReader Env m) => Type Typed -> m ()
wellformed tp = case tp of
  TyCon{} -> pure ()
  TyVar{} -> pure ()
  TySkol{} -> pure ()
  TyType{} -> pure ()
  TyPromotedCon{} -> pure ()
  TyWildcard{} -> pure ()
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

arity :: Type p -> Int
arity (TyArr _ t) = 1 + arity t
arity (TyForall _ _ t) = arity t
arity _ = 0

-- Make a type into its equivalent in prenex normal form.
normType :: forall p. Ord (Var p) => Type p -> Type p
normType = flatten . uncurry collect . runWriter . spread . applyCons where
  collect t xs = case Set.toList xs of
    [] -> t
    xs -> foldr TyPi t xs

  spread :: Type p -> Writer (Set.Set (TyBinder p)) (Type p)
  spread (TyPi b@Invisible{} t) = spread t <* tell (Set.singleton b)
  spread (TyArr a t) = TyArr a <$> spread t
  spread x = pure x

  flatten (TyRows r rs) =
    case r of
      TyRows r' rs' -> flatten (TyRows r' (unionBy ((==) `on` fst) rs rs'))
      TyExactRows rs' -> flatten (TyExactRows (unionBy ((==) `on` fst) rs rs'))
      _ -> TyRows r rs
  flatten (TyForall v k t) = TyForall v (fmap flatten k) (flatten t)
  flatten (TyArr a b) = TyArr (flatten a) (flatten b)
  flatten (TyApp a b) = TyApp (flatten a) (flatten b)
  flatten (TyTuple a b) = TyTuple (flatten a) (flatten b)
  flatten t = t

skols :: (Show (Var p), Ord (Var p)) => Type p -> Set.Set (Skolem p)
skols TyCon{}  = mempty
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
