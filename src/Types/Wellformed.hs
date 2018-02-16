{-# LANGUAGE FlexibleContexts, TypeFamilies, ScopedTypeVariables #-}
module Types.Wellformed (wellformed, arity, normType, skols) where

import Control.Monad.Except
import Control.Monad.Infer

import qualified Data.Set as Set
import Data.Foldable
import Data.Generics
import Data.List (nub)

import Syntax

import Pretty(Pretty)

wellformed :: (Pretty (Var p), MonadError TypeError m) => Type p -> m ()
wellformed tp = case tp of
  TyCon{} -> pure ()
  TyVar{} -> pure ()
  TySkol{} -> pure ()
  TyForall _ t -> wellformed t
  TyArr a b -> wellformed a *> wellformed b
  TyApp a b -> wellformed a *> wellformed b
  TyTuple a b -> wellformed a *> wellformed b
  TyRows rho rows -> do
    case rho of
      TyRows{} -> pure ()
      TyExactRows{} -> pure ()
      TyVar{} -> pure ()
      _ -> throwError (CanNotInstance tp rho)
    traverse_ (wellformed . snd) rows
  TyExactRows rows -> traverse_ (wellformed . snd) rows

arity :: Type p -> Int
arity (TyArr _ t) = 1 + arity t
arity (TyForall _ t) = arity t
arity _ = 0

-- Make a type into its equivalent in prenex normal form.
normType :: forall p. Eq (Var p) => Type p -> Type p
normType = uncurry collect . runWriter . spread where
  collect t [] = t
  collect t xs = TyForall (nub xs) t

  spread :: Type p -> Writer [Var p] (Type p)
  spread (TyForall vs t) = spread t <* tell vs
  spread (TyArr a t) = TyArr a <$> spread t
  spread x = pure x

skols :: (Ord (Var p), Data p, Data (Ann p), Data (Var p))
      => Type p -> Set.Set (Var p)
skols = everything mappend (mkQ mempty go) where
  go (TySkol v) = Set.singleton v
  go _ = Set.empty


{-
   Commentary:

   Abandon all hope, ye who enter here.

   FIXME: lmao no kinds. We need them. I removed the broken stuff we
   called "kinds" so I could actually refactor things without GHC
   complaining every 3 nanoseconds, so now the only way to make sure
   types are well formed is through this module. Yay.

   Unfortunately, this is the best I can do right now.


   Checks we peform:
    [1] Polymorphic record types' holes may only be instanced to
    something "row-y", i.e. an exact record, another polymorphic record,
    or a type variable. All other instancings are malformed.
     -}
