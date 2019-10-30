{-# LANGUAGE ConstraintKinds, FlexibleContexts, ViewPatterns #-}
module Types.Unify.Magic (magicClass, magicTyFun) where

import Control.Monad.Infer

import qualified Data.Sequence as Seq

import Syntax.Implicits
import Syntax.Types
import Syntax.Var
import Syntax

import Types.Unify.Base

type Solver m = SomeReason -> ImplicitScope ClassInfo Typed -> Type Typed -> m (Maybe (Wrapper Typed))
type TfSolver m = ImplicitScope ClassInfo Typed -> [Type Typed] -> Type Typed -> m (Maybe (Coercion Typed))
type Reporter m = SomeReason -> ImplicitScope ClassInfo Typed -> Type Typed -> Seq.Seq TypeError -> m ()

magicClass :: MonadSolve m => Var Typed -> Maybe (Solver m, Reporter m)
magicTyFun :: MonadSolve m => Var Typed -> Maybe (TfSolver m)
