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
import Data.List
import Control.Arrow
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

insertN :: [(Var Parsed, Var Resolved)] -> Map.Map (Var Parsed) ScopeVariable -> Map.Map (Var Parsed) ScopeVariable
insertN vs scope = let vs' = nub (map fst vs)
                   in if length vs == length vs'
                      -- If we have no duplicates then insert as normal
                      then foldr (uncurry Map.insert . second SVar) scope vs
                      -- If we do have duplicates then group them together
                      else foldr (\(v, _) -> case map snd $ filter ((==v) . fst) vs of
                                               [v'] -> Map.insert v (SVar v')
                                               vs -> Map.insert v (SAmbiguous vs)) scope vs

  -- case Map.lookup v scope of
                   --    Nothing -> Map.insert v (SVar v') scope
                   --    Just (SVar x) -> Map.insert v (SAmbiguous [v', x]) scope
                   --    Just (SAmbiguous xs) -> Map.insert v (SAmbiguous (v':xs)) scope

extend :: (MonadGen Int m, MonadReader Scope m) => (Var Parsed, Var Resolved) -> m a -> m a
extend (v, v') =
  local (\x -> x { varScope = Map.insert v (SVar v') (varScope x) })

extendN :: (MonadGen Int m, MonadReader Scope m) => [(Var Parsed, Var Resolved)] -> m a -> m a
extendN vs =
  local (\x -> x { varScope = insertN vs (varScope x) })

extendTy :: (MonadGen Int m, MonadReader Scope m) => (Var Parsed, Var Resolved) -> m a -> m a
extendTy (v, v') =
  local (\x -> x { tyScope = Map.insert v (SVar v') (tyScope x) })

extendTyN :: (MonadGen Int m, MonadReader Scope m) => [(Var Parsed, Var Resolved)] -> m a -> m a
extendTyN vs =
  local (\x -> x { tyScope = insertN vs (tyScope x) })
