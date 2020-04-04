{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE
    ConstraintKinds
  , FlexibleContexts
  , ScopedTypeVariables
  , TemplateHaskell
  , OverloadedStrings
  #-}
module Core.Optimise.Reduce.Base
  ( module Core.Occurrence
  , module Core.Arity
  , module Core.Core
  , module Core.Var

  , DefInfo(..), VarDef(..)
  , basicDef, basicRecDef
  , foreignDef
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
import Core.Builtin
import Core.Occurrence
import Core.Arity hiding (Arity(..), emptyScope)
import Core.Core
import Core.Var

-- | Information about a known definition
data DefInfo a
  = NoInfo
  | DefInfo
    { defVar       :: a
    , defTerm      :: Term a
    }
  | ForeignInfo
    { defVar       :: a
    , defForeign   :: Foreign
    }
  deriving (Show)

-- | A definition within the current scope
data VarDef a
  = VarDef
  { varDef       :: DefInfo a
  , varNotAmong  :: [Pattern a]
  , varLoopBreak :: !Bool
  }
  deriving (Show)

-- | A basic variable definition
basicDef :: a -> Term a -> VarDef a
basicDef v t = VarDef (DefInfo v t) [] False

-- | A basic recursive variable definition
basicRecDef :: a -> Term a -> VarDef a
basicRecDef v t = VarDef (DefInfo v t) [] True

-- | A foreign variable definition
foreignDef :: a -> Foreign -> VarDef a
foreignDef v f = VarDef (ForeignInfo v f) [] False

-- | Unknown variable definition
unknownDef :: VarDef a
unknownDef = VarDef NoInfo [] False

-- | Unknown variable definition
unknownRecDef :: VarDef a
unknownRecDef = VarDef NoInfo [] True

-- | A read-only scope within the reducer monad
data ReduceScope a
  = RScope
  { _varScope  :: VarMap.Map (VarDef a)  -- ^ Lookup of variables to their definition.
  , _typeScope :: VarMap.Map [(a, Type)] -- ^ Lookup of types to their list of constructors.
  , _ctorScope :: VarMap.Map (a, Type)   -- ^ Lookup of constructors to their parent type and signature type.
  , _ariScope :: ArityScope              -- ^ The current arity scope
  }
  deriving (Show)

-- | A substitution in the current scope
data VarSub a
  = SubTodo (AnnTerm VarSet.Set (OccursVar a)) -- ^ A substitution which has not been visited
  | SubDone (Term a) -- ^ A substitution which has been visited but could not be inlined
  deriving (Show)

-- | A "mutable" state within the reducer monad
newtype ReduceState a
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
runReduce :: forall m a x. (MonadNamey m, IsVar a)
          => RWST (ReduceScope a) (Sum Int) (ReduceState a) m x
          -> m (x, Int)
runReduce m = fmap getSum <$> evalRWST m emptyScope emptyState where
  emptyScope = RScope builtinVars builtinTys builtinCtors Arity.emptyScope
  emptyState = RState mempty

  arrTy = ForallTy Irrelevant
  prodTy a b = RowsTy NilTy [("_1", a), ("_2", b)]
  name :: CoVar
  name = tyvarA

  builtinTys :: VarMap.Map [(a, Type)]
  builtinTys = VarMap.fromList
    [(fromVar vList,
      [ (fromVar vCONS, ForallTy (Relevant name) StarTy $
          VarTy name `prodTy` AppTy tyList (VarTy name) `arrTy` AppTy tyList (VarTy name))
      , (fromVar vNIL, ForallTy (Relevant name) StarTy $ AppTy tyList (VarTy name)) ])
    ]

  builtinCtors :: VarMap.Map (a, Type)
  builtinCtors = VarMap.fromList
    [ (vCONS, (fromVar vList, ForallTy (Relevant name) StarTy $
          VarTy name `prodTy` AppTy tyList (VarTy name) `arrTy` AppTy tyList (VarTy name)))
    , (vNIL, (fromVar vList, ForallTy (Relevant name) StarTy $ AppTy tyList (VarTy name))) ]

  builtinVars :: VarMap.Map (VarDef a)
  builtinVars = VarMap.fromList
    [ ( fromVar vStrVal
      , VarDef { varDef = DefInfo (fromVar vStrVal) fakeStrVal
               , varNotAmong = []
               , varLoopBreak = False }
      )
    , ( fromVar vKSTR
      , VarDef { varDef = DefInfo (fromVar vKSTR) fakeKSTR
               , varNotAmong = []
               , varLoopBreak = False }
      )
    , ( fromVar vIntVal
      , VarDef { varDef = DefInfo (fromVar vIntVal) fakeIntVal
               , varNotAmong = []
               , varLoopBreak = False }
      )
    , ( fromVar vKINT
      , VarDef { varDef = DefInfo (fromVar vKINT) fakeKINT
               , varNotAmong = []
               , varLoopBreak = False }
      )
    ]

-- | Run the reduce monad N times, or until no more changes occur.
runReduceN :: (MonadNamey m, IsVar a)
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
    Just VarDef { varDef = DefInfo { defTerm = t } } -> Just t
    _ -> Nothing

-- | Find the raw version of the provided variable, that is the variable
-- with all type applications eliminated.
lookupRawVar :: IsVar v => v -> ReduceScope a -> CoVar
lookupRawVar v s =
  case lookupTerm v s of
    Just (TyApp (Ref v' _) _) -> lookupRawVar v' s
    Just (Atom (Ref v' _)) -> v'
    _ -> toVar v

-- | Find the raw definition of the provide variable, that is the
-- definition once all type applications have been removed from this
-- variable.
lookupRawTerm :: IsVar v => v -> ReduceScope a -> Maybe (Term a)
lookupRawTerm v s = lookupTerm (lookupRawVar v s) s

-- | Extract the type name from a constructor's type. This is a little grim.
unwrapTy :: Type -> Maybe CoVar
unwrapTy (ForallTy _ _ t) = unwrapTy t
unwrapTy (AppTy t _) = unwrapTy t
unwrapTy (ConTy v) = Just v
unwrapTy _ = Nothing
