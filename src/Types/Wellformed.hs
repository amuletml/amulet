{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
module Types.Wellformed (wellformed, arity) where

import Control.Monad.Except

import Control.Monad.Infer
import Syntax

import Pretty(Pretty)

wellformed :: (Pretty (Var p), MonadError TypeError m) => Type p -> m ()
wellformed tp = case tp of
  TyCon{} -> pure ()
  TyVar{} -> pure ()
  TyStar{} -> pure ()
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
    mapM_ (wellformed . snd) rows
  TyExactRows rows -> mapM_ (wellformed . snd) rows
  TyCons cs t -> mapM wellformedC cs *> wellformed t

wellformedC :: (Pretty (Var p), MonadError TypeError m) => GivenConstraint p -> m ()
wellformedC (Equal a b _) = wellformed a *> wellformed b

arity :: Type p -> Int
arity (TyArr _ t) = 1 + arity t
arity (TyForall _ t) = arity t
arity (TyCons _ t) = arity t
arity _ = 0

{-
   Commentary:

   Abandon all hope, ye who enter here.

   Obviously, this module begs a bit of explaining. Since the kind
   inference engine in Types.Infer isn't usable in Type Typed, here we
   implement a very dumb wellformedness check that will reject
   obviously-wrong types, such as { int | field : type}. Hopefully in
   the future we alleviate the need for this module with a *proper* kind
   system

   Unfortunately, this is the best I can do right now.


   Checks we peform:
    [1] Polymorphic record types' holes may only be instanced to
    something "row-y", i.e. an exact record, another polymorphic record,
    or a type variable. All other instancings are malformed.
     -}
