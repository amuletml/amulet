{-# LANGUAGE RankNTypes, GADTs, FlexibleContexts #-}
module Types.Infer.Let
  ( inferLetTy
  , fakeLetTys
  , rename
  , skolCheck
  , PatternStrat
  , localGenStrat
  ) where

import qualified Data.Set as Set

import Control.Monad.Infer

import Data.Reason

import Syntax.Subst
import Syntax.Types
import Syntax

inferLetTy :: forall m. MonadInfer Typed m
           => (Set.Set (Var Typed) -> Expr Typed -> Type Typed -> m (Type Typed))
           -> PatternStrat
           -> [Binding Desugared]
           -> m ( [Binding Typed]
                , Telescope Typed
                , Set.Set (Var Typed)
                )

fakeLetTys :: MonadInfer Typed m
           => [Binding Desugared]
           -> m ([Binding Typed], Telescope Typed, Set.Set (Var Typed))

rename :: Type Typed -> (Type Typed, Subst Typed)

skolCheck :: ( MonadChronicles TypeError m
             , MonadReader Env m
             )
          => Var Typed -> SomeReason -> Type Typed -> m (Type Typed)

data PatternStrat

localGenStrat :: forall p m. (Var p ~ Var Desugared, Respannable (Ann p), MonadInfer Typed m)
              => Set.Set (Var Typed) -> Expr p -> Type Typed -> m (Type Typed)
