{-# LANGUAGE OverloadedStrings #-}
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

insert :: Var Parsed -> Var Resolved -> Map.Map (Var Parsed) ScopeVariable -> Map.Map (Var Parsed) ScopeVariable
insert v v' scope = case Map.lookup v scope of
                      Nothing -> Map.insert v (SVar v') scope
                      Just (SVar x) -> Map.insert v (SAmbiguous [v', x]) scope
                      Just (SAmbiguous xs) -> Map.insert v (SAmbiguous (v':xs)) scope

extend :: (MonadGen Int m, MonadReader Scope m) => (Var Parsed, Var Resolved) -> m a -> m a
extend (v, v') =
  local (\x -> x { varScope = insert v v' (varScope x) })

extendN :: (MonadGen Int m, MonadReader Scope m) => [(Var Parsed, Var Resolved)] -> m a -> m a
extendN vs =
  local (\x -> x { varScope = foldr (uncurry insert) (varScope x) vs })

extendTy :: (MonadGen Int m, MonadReader Scope m) => (Var Parsed, Var Resolved) -> m a -> m a
extendTy (v, v') =
  local (\x -> x { tyScope = insert v v' (tyScope x) })

extendTyN :: (MonadGen Int m, MonadReader Scope m) => [(Var Parsed, Var Resolved)] -> m a -> m a
extendTyN vs =
  local (\x -> x { tyScope = foldr (uncurry insert) (tyScope x) vs })
