{-# LANGUAGE FlexibleContexts #-}
module Types.Wellformed where

import Control.Monad.Except

import Control.Monad.Infer
import Syntax

import Pretty(Pretty)

wellformed :: (Pretty (Var p), MonadError TypeError m) => Type p -> m ()
wellformed TyCon{} = pure ()
wellformed TyVar{} = pure ()
wellformed TyStar{} = pure ()
wellformed (TyForall _ t _) = wellformed t
wellformed (TyArr a b _) = wellformed a *> wellformed b
wellformed (TyApp a b _) = wellformed a *> wellformed b
wellformed tp@(TyRows rho rows _) = do
  case rho of
    TyRows{} -> pure ()
    TyExactRows{} -> pure ()
    TyVar{} -> pure ()
    _ -> throwError (CanNotInstance tp rho)
  mapM_ (wellformed . snd) rows
wellformed (TyExactRows rows _) = mapM_ (wellformed . snd) rows

{-
   Commentary:

   Surrender all hope, ye who enter here.

   Obviously, this module begs a bit of explaining. Since the kind
   inference engine in Types.Infer isn't usable in Type Typed, here we
   implement a very dumb wellformedness check that will reject
   obviously-wrong types, such as { int | field : type}[1]. Hopefully in
   the future we alleviate the need for this with a *proper* kind system
   - so that we can reject instancing the hole in a polymorphic record
   type to something that doesn't have the *kind* "list of rows".

   Unfortunately, this is the best I can do right now.


   [1] That's the only check we perform right now, because it's
   basically the only thing we can check for, anyway - there aren't many
   wellformed-ness conditions.
     -}
