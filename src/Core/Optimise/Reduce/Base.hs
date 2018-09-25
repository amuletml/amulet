{-# LANGUAGE
  ConstraintKinds
, FlexibleContexts
, ScopedTypeVariables
, TemplateHaskell #-}
module Core.Optimise.Reduce.Base
  ( module Core.Occurrence
  , module Core.Arity
  , module Core.Core
  , module Core.Var

  , DefInfo(..), VarDef(..)
  , basicDef, basicRecDef
  , unknownDef, unknownRecDef
  , ReduceScope
  , varScope, typeScope, ctorScope, ariScope

  , VarSub(..)
  , ReduceState
  , varSubst

  , MonadReduce
  , runReduce, runReduceN

  , changing, changed
  , isCtor
  , lookupVar, lookupTerm
  , lookupRawVar, lookupRawTerm
  , unwrapTy
  ) where

import Control.Monad.Namey
import Control.Monad.RWS
import Control.Lens

import qualified Data.VarMap as VarMap
import qualified Data.VarSet as VarSet
import Data.Maybe

import qualified Core.Arity as Arity
import Core.Occurrence
import Core.Arity hiding (Arity(..), emptyScope)
import Core.Core
import Core.Var

-- | Information about a known definition
data DefInfo a
  = DefInfo
  { defVar       :: a
  , defTerm      :: Term a
  }
  deriving (Show)

-- | A definition within the current scope
data VarDef a
  = VarDef
  { varDef      :: Maybe (DefInfo a)
  , varNotAmong :: [Pattern a]
  , varLoopBreak :: !Bool
  }
  deriving (Show)

-- | A basic variable definition
basicDef :: a -> Term a -> VarDef a
basicDef v t = VarDef (Just (DefInfo v t)) [] False

-- | A basic recursive variable definition
basicRecDef :: a -> Term a -> VarDef a
basicRecDef v t = VarDef (Just (DefInfo v t)) [] True

-- | Unknown variable definition
unknownDef :: VarDef a
unknownDef = VarDef Nothing [] False

-- | Unknown variable definition
unknownRecDef :: VarDef a
unknownRecDef = VarDef Nothing [] True

-- | A read-only scope within the reducer monad
data ReduceScope a
  = RScope
  { _varScope  :: VarMap.Map (VarDef a)    -- ^ Lookup of variables to their definition.
  , _typeScope :: VarMap.Map [(a, Type a)] -- ^ Lookup of types to their list of constructors.
  , _ctorScope :: VarMap.Map (a, Type a)   -- ^ Lookup of constructors to their parent type and signature type.
  , _ariScope :: ArityScope                -- ^ The current arity scope
  }
  deriving (Show)

-- | A substitution in the current scope
data VarSub a
  = SubTodo (AnnTerm VarSet.Set (OccursVar a)) -- ^ A substitution which has not been visited
  | SubDone (Term a) -- ^ A substitution which has been visited but could not be inlined
  deriving (Show)

-- | A "mutable" state within the reducer monad
data ReduceState a
  = RState
  { _varSubst :: VarMap.Map (VarSub a) -- ^ Potential candidates for inlined variables
  }
  deriving (Show)

makeLenses ''ReduceScope
makeLenses ''ReduceState

type MonadReduce a m =
  ( IsVar a
  , MonadNamey m
  , MonadReader (ReduceScope a) m
  , MonadState (ReduceState a) m
  , MonadWriter (Sum Int) m
  )

-- | Run the reduce monad in the default scope, returning the modified
-- expression and the number of changes made.
runReduce :: MonadNamey m
          => RWST (ReduceScope a) (Sum Int) (ReduceState a) m x
          -> m (x, Int)
runReduce m = fmap getSum <$> evalRWST m emptyScope emptyState where
  emptyScope = RScope mempty mempty mempty Arity.emptyScope
  emptyState = RState mempty

-- | Run the reduce monad N times, or until no more changes occur.
runReduceN :: MonadNamey m
           => (x -> RWST (ReduceScope a) (Sum Int) (ReduceState a) m x)
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

-- | Mark a term as having changed and return it
changed :: MonadReduce a m => x -> m x
changed = (<$tell (Sum 1))

-- | Determine if this variable is a constructor
isCtor :: IsVar v => v -> ReduceScope a -> Bool
isCtor var = VarMap.member (toVar var) . view ctorScope

-- | Look up a variable within the current scope
lookupVar :: IsVar v => v -> ReduceScope a -> VarDef a
lookupVar v = fromMaybe unknownDef . VarMap.lookup (toVar v) . view varScope

-- | Look up a variable within the current scope
lookupTerm :: IsVar v => v -> ReduceScope a -> Maybe (Term a)
lookupTerm v s =
  case VarMap.lookup (toVar v) (s ^. varScope) of
    Just VarDef { varDef = Just DefInfo { defTerm = t } } -> Just t
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
