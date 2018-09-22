{-# LANGUAGE
  ConstraintKinds
, FlexibleContexts
, ScopedTypeVariables
, TemplateHaskell #-}
module Core.Optimise.Reduce.Base
  ( module Core.Core
  , module Core.Var

  , VarDef(..), unknownDef, basicDef
  , ReduceScope
  , varScope, typeScope, ctorScope
  , MonadReduce
  , runReduce, runReduceN

  , changing, changed
  , isCtor
  , lookupVar, lookupTerm
  , lookupRawVar, lookupRawTerm
  , unwrapTy
  ) where

import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Namey
import Control.Arrow
import Control.Lens

import qualified Data.VarMap as VarMap
import Data.Maybe

import Core.Core
import Core.Var

-- | A definition within the current scope
data VarDef a
  = VarDef
  { varDef      :: Maybe (a, Term a)
  , varNotAmong :: [Pattern a]
  }
  deriving (Show)

-- | A basic variable definition
basicDef :: a -> Term a -> VarDef a
basicDef v t = VarDef (Just (v, t)) []

-- | Unknown variable definition
unknownDef :: VarDef a
unknownDef = VarDef Nothing []

-- ^ A read-only scope within the reducer monad
data ReduceScope a
  = RScope
  { _varScope  :: VarMap.Map (VarDef a)    -- ^ Lookup of variables to their definition.
  , _typeScope :: VarMap.Map [(a, Type a)] -- ^ Lookup of types to their list of constructors.
  , _ctorScope :: VarMap.Map (a, Type a)   -- ^ Lookup of constructors to their parent type and signature type.
  }
  deriving (Show)

makeLenses ''ReduceScope

type MonadReduce a m =
  ( IsVar a
  , MonadNamey m
  , MonadReader (ReduceScope a) m
  , MonadWriter (Sum Int) m
  )

-- | Run the reduce monad in the default scope, returning the modified
-- expression and the number of changes made.
runReduce :: MonadNamey m
          => ReaderT (ReduceScope a) (WriterT (Sum Int) m) x
          -> m (x, Int)
runReduce = (second getSum<$>) . runWriterT . flip runReaderT emptyScope where
  emptyScope = RScope mempty mempty mempty

-- | Run the reduce monad N times, or until no more changes occur.
runReduceN :: MonadNamey m
           => (x -> ReaderT (ReduceScope a) (WriterT (Sum Int) m) x)
           -> Int -> x
           -> m x
runReduceN task = go where
  go 0 x = pure x
  go n x = do
    (x', ch) <- runReduce (task x)
    if ch > 0 then go (n - 1) x' else pure x'

-- | Mark this transformation as having changed the term
changing :: MonadReduce a m => m x -> m x
changing = (tell (Sum 1)>>)

-- | Return a changed term
changed :: MonadReduce a m => x -> m x
changed = (<$tell (Sum 1))

isCtor :: IsVar a => a -> ReduceScope a -> Bool
isCtor var = VarMap.member (toVar var) . view ctorScope

-- | Look up a variable within the current scope
lookupVar :: IsVar a => a -> ReduceScope a -> VarDef a
lookupVar v = fromMaybe unknownDef . VarMap.lookup (toVar v) . view varScope

-- | Look up a variable within the current scope
lookupTerm :: IsVar a => a -> ReduceScope a -> Maybe (Term a)
lookupTerm v s =
  case VarMap.lookup (toVar v) (s ^. varScope) of
    Just VarDef { varDef = Just (_, t) } -> Just t
    _ -> Nothing

-- | Find the raw version of the provided variable, that is the variable
-- with all type applications eliminated.
lookupRawVar :: IsVar a => a -> ReduceScope a -> a
lookupRawVar v s =
  case lookupTerm v s of
    Just (TyApp (Ref v' _) _) -> lookupRawVar v' s
    Just (Atom (Ref v' _)) -> v'
    _ -> v

-- | Find the raw definition of the provide variable, that is the
-- definition once all type applications have been removed from this
-- variable.
lookupRawTerm :: IsVar a => a -> ReduceScope a -> Maybe (Term a)
lookupRawTerm v s = lookupTerm (lookupRawVar v s) s

-- | Extract the type name from a constructor's type. This is a little grim.
unwrapTy :: Type a -> Maybe a
unwrapTy (ForallTy _ _ t) = unwrapTy t
unwrapTy (AppTy t _) = unwrapTy t
unwrapTy (ConTy v) = Just v
unwrapTy _ = Nothing
