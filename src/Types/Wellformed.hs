{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
module Types.Wellformed (wellformed, arity, normType) where

import Control.Monad.Except
import Control.Monad.Infer
import Control.Arrow

import Data.Function
import Data.Foldable
import Data.List (union, unionBy)

import Syntax

import Pretty(Pretty)

wellformed :: (Pretty (Var p), MonadError TypeError m) => Type p -> m ()
wellformed tp = case tp of
  TyCon{} -> pure ()
  TyVar{} -> pure ()
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

normType :: Eq (Var p) => Type p -> Type p
normType (TyForall vs (TyForall vs' tp)) = TyForall (vs `union` vs') (normType tp)
normType (TyForall [] tp) = normType tp
normType (TyRows x rs')
  | TyRows rho rs <- normType x = normType (TyRows rho (normRows (unionBy ((==) `on` fst) rs rs')))
  | TyExactRows rs <- normType x = normType (TyExactRows (normRows (unionBy ((==) `on` fst) rs rs')))
    where normRows = map (second normType)
normType x = x

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
