{-# LANGUAGE OverloadedStrings, LambdaCase, FlexibleContexts #-}

-- | Track variables in the current scope while resolving 'Syntax'. This
-- also tracks ambiguous definitions and modules.
module Syntax.Resolve.Scope
  ( Scope(..)
  , ScopeVariable(..)
  , ModuleScope(..)
  , emptyScope, emptyModules
  , tagVar, tagModule
  , extend, extendN
  , extendTy, extendTyN
  , extendTyvar, extendTyvarN
  , extendM
  , insertN'
  ) where

import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Function
import Data.List

import Control.Monad.Reader
import Control.Monad.Namey

import Syntax.Var

-- | A variable in the current scope
data ScopeVariable
  = SVar (Var Resolved)       -- ^ Can be resolved to a specific variable
  | SAmbiguous [Var Resolved] -- ^ Can be resolved to 2 or more variables.
  deriving (Eq, Ord, Show)

-- | The current scope of the resolver.
data Scope = Scope
             { -- | All variables in the current scope
               varScope    :: Map.Map (Var Parsed) ScopeVariable
               -- | All types in the current scope
             , tyScope     :: Map.Map (Var Parsed) ScopeVariable
               -- | All type variables in the current scope
             , tyvarScope  :: Map.Map (Var Parsed) ScopeVariable
               -- | The current module we are resolving in
             , modStack    :: [T.Text]
             }
  deriving (Show)

-- | A mapping of fully-qualified module names to the variables within
-- them.
--
-- There is only one 'ModuleScope' for the resolution process.
newtype ModuleScope = ModuleScope (Map.Map (Var Parsed) (Var Resolved, Scope))
  deriving (Show)

-- | An empty scope, suitable for a new module
emptyScope :: Scope
emptyScope = Scope mempty mempty mempty mempty

-- | An empty module scope
emptyModules :: ModuleScope
emptyModules = ModuleScope mempty

-- | Convert a parsed variable into a resolved one. This requires that
-- the variable is unqualified.
tagVar :: MonadNamey m => Var Parsed -> m (Var Resolved)
tagVar (Name n) = TgName n <$> gen
tagVar x = error ("Cannot tag variable " ++ show x)

-- | Convert a parsed module name into a resolved one. This works on both
-- unqualified and qualified module names.
tagModule :: MonadNamey m => Var Parsed -> m (Var Resolved)
tagModule n = TgName (T.intercalate (T.singleton '.') (expand n)) <$> gen where
  expand (Name n) = [n]
  expand (InModule m n) = m:expand n

-- | Insert one or more variables into a map. If multiple variables with
-- the same name are defined, this will be considered as ambiguous.
insertN :: Map.Map (Var Parsed) ScopeVariable
        -> [(Var Parsed, Var Resolved)] -> Map.Map (Var Parsed) ScopeVariable
insertN scope = foldr (\case
                          [(v, v')] -> Map.insert v (SVar v')
                          vs@((v,_):_) -> Map.insert v (SAmbiguous (map snd vs))
                          [] -> undefined) scope
                . groupBy ((==) `on` fst)
                . sortOn fst

-- | Insert one or more variables into a map. If a variable is declared
-- multiple times, we will prefer the latest definition.
insertN' :: Map.Map (Var Parsed) ScopeVariable
         -> [(Var Parsed, Var Resolved)] -> Map.Map (Var Parsed) ScopeVariable
insertN' = foldl' (\s (v, v') -> Map.insert v (SVar v') s)

-- | Create a scope with one variable and evaluate the provided monad
-- within it.
extend :: MonadReader Scope m => (Var Parsed, Var Resolved) -> m a -> m a
extend (v, v') =
  local (\x -> x { varScope = Map.insert v (SVar v') (varScope x) })

-- | Create a scope with one or more variables and evaluate the provided
-- monad within it.
extendN :: MonadReader Scope m => [(Var Parsed, Var Resolved)] -> m a -> m a
extendN vs =
  local (\x -> x { varScope = insertN (varScope x) vs })

-- | Create a scope with one type and evaluate the provided monad within
-- it.
extendTy :: MonadReader Scope m => (Var Parsed, Var Resolved) -> m a -> m a
extendTy (v, v') =
  local (\x -> x { tyScope = Map.insert v (SVar v') (tyScope x) })

-- | Create a scope with one or more types and evaluate the provided
-- monad within it.
extendTyN :: MonadReader Scope m => [(Var Parsed, Var Resolved)] -> m a -> m a
extendTyN vs =
  local (\x -> x { tyScope = insertN (tyScope x) vs })

-- | Create a scope with one type variables and evaluate the provided
-- monad within it.
extendTyvar :: MonadReader Scope m => (Var Parsed, Var Resolved) -> m a -> m a
extendTyvar (v, v') =
  local (\x -> x { tyvarScope = Map.insert v (SVar v') (tyvarScope x) })

-- | Create a scope with one or more type variables and evaluate the
-- provided monad within it.
extendTyvarN :: MonadReader Scope m => [(Var Parsed, Var Resolved)] -> m a -> m a
extendTyvarN vs =
  local (\x -> x { tyvarScope = insertN (tyvarScope x) vs })

-- | Enter a child module and evaluate the provide monad within it.
extendM :: MonadReader Scope m => Var Parsed -> m a -> m a
extendM m = local (\x -> x { modStack = extend m (modStack x) }) where
  extend (Name n) xs = n:xs
  extend (InModule m n) xs = m:extend n xs

gen :: MonadNamey m => m Int
gen = (\(TgName _ i) -> i) <$> genName
