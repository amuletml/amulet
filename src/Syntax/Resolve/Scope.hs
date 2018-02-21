{-# LANGUAGE OverloadedStrings, LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Syntax.Resolve.Scope
  ( Scope(..)
  , ScopeVariable(..)
  , builtinScope, tagVar
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
                   }
  deriving (Eq, Ord, Show)

builtinScope :: Scope
builtinScope = Scope
               { varScope = build [ "+", "-", "*", "/", "**", "^"
                                  , "<", ">", ">=", "<=", "==", "<>"
                                  , "||", "&&" ]
               , tyScope = build [ "int", "string", "bool", "unit" ]
               }
  where build :: [T.Text] -> Map.Map (Var Parsed) ScopeVariable
        build = foldr (\v -> Map.insert (Name v) (SVar (TgInternal v))) mempty


tagVar :: MonadGen Int m => Var Parsed -> m (Var Resolved)
tagVar (Name n) = TgName n <$> gen

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
