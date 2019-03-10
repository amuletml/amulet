{-# LANGUAGE FlexibleContexts, TupleSections, ScopedTypeVariables,
   ViewPatterns, LambdaCase #-}
module Types.Infer
  ( inferProgram
  , closeOver

  , infer, check, solveEx
  ) where

import Prelude hiding (lookup)

import qualified Data.Map.Strict as Map
import Data.These

import Control.Monad.Infer

import Syntax.Subst
import Syntax.Var
import Syntax

import Types.Kinds

inferProgram :: MonadNamey m => Env -> [Toplevel Desugared] -> m (These [TypeError] ([Toplevel Typed], Env))

check :: forall m. MonadInfer Typed m => Expr Desugared -> Type Typed -> m (Expr Typed)

infer :: MonadInfer Typed m => Expr Desugared -> m (Expr Typed, Type Typed)

solveEx :: Type Typed -> Subst Typed -> Map.Map (Var Typed) (Wrapper Typed) -> Expr Typed -> Expr Typed
