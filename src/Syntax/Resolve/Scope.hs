{-# LANGUAGE OverloadedStrings, LambdaCase, TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

module Syntax.Resolve.Scope
  ( Scope(..)
  , ScopeVariable(..)
  , ModuleScope(..)
  , builtinScope, emptyScope, emptyModules
  , tagVar, tagModule
  , extend, extendN
  , extendTy, extendTyN
  ) where

import qualified Data.Text as T
import qualified Data.Map as Map
import Data.List
import Data.Function
import Control.Monad.Gen
import Control.Monad.Reader

import Syntax

data ScopeVariable
  = SVar (Var Resolved)
  | SAmbiguous [Var Resolved]
  deriving (Eq, Ord, Show)

data Scope = Scope { varScope :: Map.Map (Var Parsed) ScopeVariable
                   , tyScope  :: Map.Map (Var Parsed) ScopeVariable
                   , modStack :: [T.Text]
                   }

newtype ModuleScope = ModuleScope (Map.Map (Var Parsed) (Var Resolved, Scope))

builtinScope :: Scope
builtinScope = Scope
               { varScope = build [ "+", "-", "*", "/", "**", "^"
                                  , "<", ">", ">=", "<=", "==", "<>"
                                  , "||", "&&" ]
               , tyScope  =  build [ "int", "string", "bool", "unit" ]
               , modStack = []
               }
  where build :: [T.Text] -> Map.Map (Var Parsed) ScopeVariable
        build = foldr (\v -> Map.insert (Name v) (SVar (TgInternal v))) mempty

emptyScope :: Scope
emptyScope = Scope mempty mempty mempty

emptyModules :: ModuleScope
emptyModules = ModuleScope mempty

tagVar :: MonadGen Int m => Var Parsed -> m (Var Resolved)
tagVar (Name n) = TgName n <$> gen
tagVar x = error ("Cannot tag variable " ++ show x)

tagModule :: MonadGen Int m => Var Parsed -> m (Var Resolved)
tagModule n = TgName (T.intercalate (T.singleton '.') (expand n)) <$> gen where
  expand (Name n) = [n]
  expand (InModule m n) = m:expand n

insertN :: Map.Map (Var Parsed) ScopeVariable -> [(Var Parsed, Var Resolved)] -> Map.Map (Var Parsed) ScopeVariable
insertN scope = foldr (\case
                          [(v, v')] -> Map.insert v (SVar v')
                          vs@((v,_):_) -> Map.insert v (SAmbiguous (map snd vs))
                          [] -> undefined) scope
                . groupBy ((==) `on` fst)
                . sortOn fst

extend :: (MonadGen Int m, MonadReader Scope m) => (Var Parsed, Var Resolved) -> m a -> m a
extend (v, v') =
  local (\x -> x { varScope = Map.insert v (SVar v') (varScope x) })

extendN :: (MonadGen Int m, MonadReader Scope m) => [(Var Parsed, Var Resolved)] -> m a -> m a
extendN vs =
  local (\x -> x { varScope = insertN (varScope x) vs })

extendTy :: (MonadGen Int m, MonadReader Scope m) => (Var Parsed, Var Resolved) -> m a -> m a
extendTy (v, v') =
  local (\x -> x { tyScope = Map.insert v (SVar v') (tyScope x) })

extendTyN :: (MonadGen Int m, MonadReader Scope m) => [(Var Parsed, Var Resolved)] -> m a -> m a
extendTyN vs =
  local (\x -> x { tyScope = insertN (tyScope x) vs })

-- extendM :: (MonadGen Int m, MonadReader Scope m) => (Var Parsed, Scope) -> m a -> m a
-- extendM (v, env) = local (\x -> x { modScope = extendModule (modScope x) v }) where
--   extendModule mods (Name n) =
--     Map.insert n <$> (case Map.lookup n mods of
--                         Nothing -> (,env) <$> tagVar (Name n)
--                         Just (v', env') -> pure (v', env `update` env')) <*> pure mods
--   extendModule mods (InModule m n) =
--     let scope = Map.findWithDefault emptyScope m mods
--     in Map.insert m (scope { modScope = extendModule (modScope scope) n }) mods


-- update :: Scope -> Scope -> Scope
-- (Scope v t m) `update` (Scope v' t' m') = Scope (v `Map.union` v')
--                                                 (t `Map.union` t')
--                                                 (Map.unionWith update m m')
