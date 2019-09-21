{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}
module Types.Unify.Magic (magicClass) where

import Control.Monad.Infer

import Syntax.Builtin
import Syntax.Var
import Syntax

import Data.Span

type Solver m = Type Typed -> m (Maybe (Span -> Wrapper Typed))
type MonadSolve m = (MonadNamey m, MonadWriter [Constraint Typed] m, MonadChronicles TypeError m)

magicClass :: MonadSolve m => Var Typed -> Maybe (Solver m)
magicClass v
  | v == tyKStrName = Just (solveKnownLit knownStrName knownStrTy knownStrTy' tyKStr tyString)
  | v == tyKIntName = Just (solveKnownLit knownIntName knownIntTy knownIntTy' tyKInt tyInt)
  | otherwise = Nothing

solveKnownLit :: MonadSolve m
              => Var Resolved -> Type Typed -> (Type Typed -> Type Typed) -> Type Typed -> Type Typed
              -> Solver m
solveKnownLit name ty ty' constraint return (TyApp _ str) =
  case str of
    TyLit l -> pure $ pure (solution l)
    _ -> pure Nothing
  where
    solution :: Lit -> Span -> Wrapper Typed
    solution t span = ExprApp $
      App (ExprWrapper (TypeApp (TyLit t))
            (VarRef name (span, ty))
            (span, ty' (TyLit t)))
          (Literal t (span, return))
          (span, TyApp constraint (TyLit t))
solveKnownLit _ _ _ _ _ _ = error "kind error in solveKnownString"
