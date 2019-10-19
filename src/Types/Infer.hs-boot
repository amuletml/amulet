{-# LANGUAGE FlexibleContexts, TupleSections, ScopedTypeVariables,
   ViewPatterns, LambdaCase, TypeFamilies #-}
module Types.Infer
  ( inferProgram
  , closeOver

  , infer, check, solveEx
  , infer', instantiateTc
  ) where

import Prelude hiding (lookup)

import qualified Data.Map.Strict as Map
import Data.These

import Control.Monad.Infer

import Syntax.Subst
import Syntax.Var
import Syntax.Types (TySyms)
import Syntax

import Types.Kinds

inferProgram :: MonadNamey m => Env -> [Toplevel Desugared] -> m (These [TypeError] ([Toplevel Typed], Env))

check :: forall m. MonadInfer Typed m => Expr Desugared -> Type Typed -> m (Expr Typed)

infer :: MonadInfer Typed m => Expr Desugared -> m (Expr Typed, Type Typed)
infer' :: MonadInfer Typed m => Expr Desugared -> m (Expr Typed, Type Typed)

instantiateTc :: MonadInfer Typed m => SomeReason -> Type Typed -> m (Expr Typed -> Expr Typed, Type Typed)

solveEx :: TySyms -> Subst Typed -> Map.Map (Var Typed) (Wrapper Typed) -> Expr Typed -> Expr Typed
