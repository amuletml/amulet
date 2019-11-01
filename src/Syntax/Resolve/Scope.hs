{-# LANGUAGE OverloadedStrings, LambdaCase, FlexibleContexts, TemplateHaskell #-}

-- | Track variables in the current scope while resolving 'Syntax'. This
-- also tracks ambiguous definitions and modules.
module Syntax.Resolve.Scope
  ( Slot(..)
  , VarName
  , Signature(..), vals, types, modules
  , Context(..), scope, tyvars, nonRecs
  , emptyContext
  , exportedNames
  , tagVar
  , withVal, withVals, extendVals
  , withTy, withTys
  , withMod
  , extendTyvar, extendTyvars
  ) where

import Control.Lens hiding (Context)

import qualified Data.VarSet as VarSet
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Foldable
import Data.Function
import Data.Span
import Data.List

import Control.Monad.Reader
import Control.Monad.Namey

import Syntax.Var

import Core.Var

-- | A variable in the current scope
data Slot
  = SVar (Var Resolved)       -- ^ Can be resolved to a specific variable
  | SAmbiguous [Var Resolved] -- ^ Can be resolved to 2 or more variables.
  deriving Show

type VarName = T.Text

-- | The signature for a module.
data Signature = Signature
  { -- | All value variables in the module.
    _vals :: Map.Map VarName Slot
    -- | All types in the module
  , _types :: Map.Map VarName Slot
    -- | All child modules in the module. If the signature is 'Nothing',
    -- this means the module could not be resolved.
  , _modules :: Map.Map VarName (Var Resolved, Maybe Signature)
  }
  deriving Show

instance Semigroup Signature where
  (Signature v t m) <> (Signature v' t' m') = Signature (Map.union v' v) (Map.union t' t) (Map.union m' m)

instance Monoid Signature where
  mempty = Signature mempty mempty mempty

-- | The current state of the resolver.
data Context = Context
  { -- | All declared items in the current scope
    _scope :: Signature
    -- | All type variables in the current scope
  , _tyvars :: Map.Map VarName Slot
    -- | Non-recursive names whose definitions we are within.
  , _nonRecs :: Map.Map VarName Span
  }
  deriving Show

makeLenses ''Signature
makeLenses ''Context

-- | An empty context for resolving
emptyContext :: Context
emptyContext = Context mempty mempty mempty

-- | Get all names exported by a module.
exportedNames :: Signature -> VarSet.Set
exportedNames sig
   = foldMap ofSlot (sig ^. vals)
  <> foldMap (exportedNames . fold . snd) (sig ^. modules)
  where
    ofSlot (SVar (TgName n i)) = VarSet.singleton (CoVar i (Just n) ValueVar)
    ofSlot (SVar TgInternal{}) = mempty
    ofSlot SAmbiguous{} = mempty

-- | Convert a parsed variable into a resolved one. This requires that
-- the variable is unqualified.
tagVar :: MonadNamey m => Var Parsed -> m (Var Resolved)
tagVar (Name n) = TgName n <$> gen
tagVar x = error ("Cannot tag variable " ++ show x)

insertV :: Var Parsed -> a -> Map.Map VarName a -> Map.Map VarName a
insertV (Name v) x = Map.insert v x
insertV v@InModule{} _ = error ("Cannot insert InModule" ++ show v)

-- | Insert one or more variables into a map. If multiple variables with
-- the same name are defined, this will be considered as ambiguous.
insertVs :: [(Var Parsed, Var Resolved)]
        -> Map.Map VarName Slot -> Map.Map VarName Slot
insertVs vs scope
  = foldr (\case
      [(v, v')] -> insertV v (SVar v')
      vs@((v,_):_) -> insertV v (SAmbiguous (map snd vs))
      [] -> id) scope
  . groupBy ((==) `on` fst)
  . sortOn fst
  $ vs

-- | Extend a signature with a variable.
withVal :: Var Parsed -> Var Resolved -> Signature -> Signature
withVal v v' = vals %~ insertV v (SVar v')

-- | Extend a signature with multiples variables.
withVals :: [(Var Parsed, Var Resolved)] -> Signature -> Signature
withVals vs = vals %~ insertVs vs

-- | Create a scope with multiple variables and evaluate the provided
-- monad within it.
extendVals :: MonadReader Context m => [(Var Parsed, Var Resolved)] -> m a -> m a
extendVals vs = local (scope . vals %~ insertVs vs)

-- | Extend a signature with a type.
withTy :: Var Parsed -> Var Resolved -> Signature -> Signature
withTy v v' = types %~ insertV v (SVar v')

-- | Extend a signature with multiple types.
withTys :: [(Var Parsed, Var Resolved)] -> Signature -> Signature
withTys vs = types %~ insertVs vs

-- | Extend a signature with a module.
withMod :: Var Parsed -> Var Resolved -> Maybe Signature -> Signature -> Signature
withMod v v' m = modules %~ insertV v (v', m)

-- | Create a scope with a type variable and evaluate the provided monad
-- within it.
extendTyvar :: MonadReader Context m => Var Parsed -> Var Resolved -> m a -> m a
extendTyvar v v' = local (tyvars %~ insertV v (SVar v'))

-- | Create a scope with multiple type variables and evaluate the
-- provided monad within it.
extendTyvars :: MonadReader Context m => [(Var Parsed, Var Resolved)] -> m a -> m a
extendTyvars vs = local (tyvars %~ insertVs vs)

gen :: MonadNamey m => m Int
gen = (\(TgName _ i) -> i) <$> genName
