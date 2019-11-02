{-# LANGUAGE MultiWayIf, FlexibleContexts, ScopedTypeVariables,
   TemplateHaskell, TupleSections, ViewPatterns,
   LambdaCase, ConstraintKinds, CPP, TypeFamilies, OverloadedStrings, RecordWildCards #-}
module Types.Unify.Base
  ( MonadSolve
  , SolveState(..), capture
  , SolveScope(..)
  , SolverInfo
  , HasSolverInfo(..), getSolveInfo

  , bindSkol, don'tTouch, depth, solveInfo, guarded
  , solveTySubst, solveAssumptions, solveCoSubst, solveFuel
  , runSolve

  , emptyState
  , emptyFuel

-- Monadic user functions:
  , doWork, polyInstSafe

-- Type shape predicates:
  , prettyConcrete, concretish, concreteUnderOne
  ) where

import Control.Monad.State
import Control.Monad.Infer
import Control.Lens hiding (Empty, (:>))

import Syntax.Subst
import Syntax.Types
import Syntax

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Prelude hiding (lookup)

type SolverInfo = Map.Map (Var Typed) (Either ClassInfo TySymInfo)

-- | The scope for running the solver in. This records:
-- * Whether or not we can assign skolems to 'solveAssumptions' in the
-- solver state
-- * The set of variables we are not allowed to unify
-- * The current class reduction stack
-- * The solver info
-- * Whether or not we're in a guarded position
data SolveScope
  = SolveScope { _bindSkol :: Bool
               , _don'tTouch :: Set.Set (Var Typed)
               , _depth :: [Type Typed]
               , _solveInfo :: SolverInfo
               , _guarded :: Bool
               }
  deriving (Eq, Show, Ord)

data SolveState
  = SolveState { _solveTySubst :: Subst Typed
               , _solveAssumptions :: Subst Typed
               , _solveCoSubst :: Map.Map (Var Typed) (Wrapper Typed)
               , _solveFuel :: Int
               }
  deriving (Eq, Show)

-- | Number of inversions/type family reductions allowed to be done per
-- /individual/ constraint.
emptyFuel :: Int
emptyFuel = 20

emptyState :: SolveState
emptyState = SolveState mempty mempty mempty emptyFuel

runSolve :: MonadNamey m
         => Bool
         -> SolverInfo
         -> SolveState
         -> WriterT [Constraint Typed] (StateT SolveState (ReaderT SolveScope m)) a
         -> m ([Constraint Typed], SolveState)
runSolve skol info s x = runReaderT (runStateT (execWriterT act) s) emptyScope where
  act = (,) <$> genName <*> x
  emptyScope = SolveScope skol mempty [] info False

makeLenses ''SolveState
makeLenses ''SolveScope

type MonadSolve m =
  ( MonadNamey m
  , MonadState SolveState m
  , MonadReader SolveScope m
  , MonadChronicles TypeError m
  , MonadWriter [Constraint Typed] m
  )

-- | A class for environments which have solver information.
class HasSolverInfo env where
  asksSolveInfo :: env -> SolverInfo

instance HasSolverInfo Env where
  asksSolveInfo e = (Left <$> e ^. classDecs) <> (Right <$> e ^. tySyms)

getSolveInfo :: (MonadReader env m, HasSolverInfo env) => m SolverInfo
getSolveInfo = asks asksSolveInfo

prettyConcrete :: Type Typed -> Bool
prettyConcrete TyVar{} = False
prettyConcrete (TyWildcard t) = maybe False prettyConcrete t
prettyConcrete _ = True

concretish :: Type Typed -> Bool
concretish TyVar{} = False
concretish (TyApp f x) = concretish f && concretish x
concretish (TyWildcard t) = maybe False concretish t
concretish _ = True

concreteUnderOne :: Type Typed -> Bool
concreteUnderOne t = all prettyConcrete (appsView t)

polyInstSafe :: MonadReader SolveScope m => m a -> m a
polyInstSafe = local (guarded .~ True)

capture :: MonadState b m => m a -> m (a, b)
capture m = do
  x <- get
  r <- m
  st <- get
  put x
  pure (r, st)

doWork :: (MonadState SolveState m, MonadChronicles TypeError m) => m TypeError -> m ()
doWork e = do
  x <- use solveFuel
  solveFuel .= (x - 1)
  unless (x >= 0) $
    confesses =<< e
