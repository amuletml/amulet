{-# LANGUAGE FlexibleContexts, ScopedTypeVariables,
   ViewPatterns, LambdaCase, TypeFamilies #-}
module Types.Infer.Class
  ( WrapFlavour(..)
  , inferClass
  , inferInstance
  , reduceClassContext
  , extendTySyms
  ) where

import Prelude hiding (lookup)

import qualified Data.Map.Strict as Map
import Data.Reason

import Control.Monad.Infer

import Syntax.Implicits
import Syntax.Subst
import Syntax.Types
import Syntax.Var
import Syntax

import GHC.Stack

extendTySyms :: Foldable t => t TySymInfo -> Map.Map VarResolved TySymInfo -> Map.Map VarResolved TySymInfo

inferClass :: forall m. MonadInfer Typed m
           => Toplevel Desugared
           -> m ( [Toplevel Typed]
                , Telescope Typed
                , ClassInfo
                , ImplicitScope ClassInfo Typed
                , [TySymInfo]
                )
inferInstance :: forall m. MonadInfer Typed m
              => Toplevel Desugared
              -> m ( [Toplevel Typed]
                   , Var Typed
                   , Type Typed
                   , ClassInfo
                   , [TySymInfo]
                   )

reduceClassContext :: forall m. (MonadInfer Typed m, HasCallStack)
                   => ImplicitScope ClassInfo Typed
                   -> Ann Desugared
                   -> [Constraint Typed]
                   -> m ( Type Typed -> Type Typed
                        , WrapFlavour -> Expr Typed -> Expr Typed
                        , [Need Typed], Subst Typed)

data WrapFlavour = Thin | Full
type Need t = (Var t, Type t, SomeReason)
