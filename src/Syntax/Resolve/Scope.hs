{-# LANGUAGE OverloadedStrings, LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Syntax.Resolve.Scope
  ( Scope(..)
  , ScopeVariable(..)
  , ModuleScope(..)
  , builtinScope, emptyScope, emptyModules
  , tagVar, tagModule
  , extend, extendN
  , extendTy, extendTyN
  , extendTyvar, extendTyvarN
  , extendM
  , insertN
  ) where

import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Function
import Data.List

import Control.Monad.Reader
import Control.Monad.Gen

import Syntax

data ScopeVariable
  = SVar (Var Resolved)
  | SAmbiguous [Var Resolved]
  deriving (Eq, Ord, Show)

data Scope = Scope { varScope    :: Map.Map (Var Parsed) ScopeVariable
                   , tyScope     :: Map.Map (Var Parsed) ScopeVariable
                   , tyvarScope  :: Map.Map (Var Parsed) ScopeVariable
                   , modStack    :: [T.Text]
                   }

newtype ModuleScope = ModuleScope (Map.Map (Var Parsed) (Var Resolved, Scope))

builtinScope :: Scope
builtinScope = Scope
               { varScope = build [ "+", "-", "*", "/", "**", "^"
                                  , "+.", "-.", "*.", "/.", "**.", "^."
                                  , "<", ">", ">=", "<=", "==", "<>"
                                  , "||", "&&"
                                  , "@@", "lazy", "force" ]
               , tyScope  =  build [ "int", "string", "bool", "unit", "float", "lazy" ]
               , tyvarScope = mempty
               , modStack = []
               }
  where build :: [T.Text] -> Map.Map (Var Parsed) ScopeVariable
        build = foldr (\v -> Map.insert (Name v) (SVar (TgInternal v))) mempty

emptyScope :: Scope
emptyScope = Scope mempty mempty mempty mempty

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

extendTyvar :: (MonadGen Int m, MonadReader Scope m) => (Var Parsed, Var Resolved) -> m a -> m a
extendTyvar (v, v') =
  local (\x -> x { tyvarScope = Map.insert v (SVar v') (tyvarScope x) })

extendTyvarN :: (MonadGen Int m, MonadReader Scope m) => [(Var Parsed, Var Resolved)] -> m a -> m a
extendTyvarN vs =
  local (\x -> x { tyvarScope = insertN (tyvarScope x) vs })

extendM :: MonadReader Scope m => Var Parsed -> m a -> m a
extendM m = local (\x -> x { modStack = extend m (modStack x) }) where
  extend (Name n) xs = n:xs
  extend (InModule m n) xs = m:extend n xs
