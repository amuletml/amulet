{-# LANGUAGE ConstraintKinds, FlexibleContexts, LambdaCase #-}
module Types.Kinds
  ( resolveKind
  , resolveTyDeclKind
  , annotateKind
  , closeOver
  , checkAgainstKind, getKind, liftType
  )
  where

import Control.Monad.Infer

import Syntax

type MonadKind m =
  ( MonadError TypeError m
  , MonadReader Env m
  , MonadGen Int m
  )

type Kind = Type

resolveKind :: MonadKind m
            => SomeReason -> Type Resolved -> m (Type Typed)

getKind :: MonadKind m
        => SomeReason -> Type Resolved -> m (Kind Typed)

checkAgainstKind :: MonadInfer Typed m
                 => SomeReason -> Type Resolved -> Type Typed
                 -> m (Type Typed)

annotateKind :: MonadKind m => SomeReason -> Type Typed -> m (Type Typed)

resolveTyDeclKind :: MonadKind m
                  => SomeReason
                  -> Var Resolved -> [Var Resolved]
                  -> [Constructor Resolved]
                  -> m (Type Typed)

closeOver :: MonadKind m => SomeReason -> Type Typed -> m (Type Typed)
liftType :: MonadKind m => SomeReason -> Type Resolved -> m (Type Typed)
