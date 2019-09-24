module Types.Holes (findHoleCandidate) where

import Control.Monad.Namey

import Syntax.Subst (Subst)
import Syntax.Types (Env)
import Syntax (Type, Expr, Typed)

import Data.Span (Span)

findHoleCandidate :: MonadNamey m => Subst Typed -> Span -> Env -> Type Typed -> m [Expr Typed]
