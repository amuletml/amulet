{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Syntax.Resolve.Scope
  ( Scope(..)
  , builtinScope, tagVar
  , extend, extendN
  , extendTy, extendTyN
  ) where

import qualified Data.Text as T
import qualified Data.Map as Map
import Control.Monad.Gen
import Control.Monad.Reader

import Syntax

data Scope = Scope { varScope :: Map.Map (Var Parsed) (Var Resolved)
                   , tyScope  :: Map.Map (Var Parsed) (Var Resolved)
                   }
  deriving (Eq, Ord, Show)

builtinScope :: Scope
builtinScope = Scope
               { varScope = build [ "+", "-", "*", "/", "**", "^"
                                  , "<", ">", ">=", "<=", "==", "<>"
                                  , "||", "&&" ]
               , tyScope = build [ "int", "string", "bool", "unit" ]
               }
  where build :: [T.Text] -> Map.Map (Var Parsed) (Var Resolved)
        build = foldr (\v -> Map.insert (Name v) (TgInternal v)) mempty


tagVar :: MonadGen Int m => Var Parsed -> m (Var Resolved)
tagVar (Name n) = TgName n <$> gen

extend :: (MonadGen Int m, MonadReader Scope m) => (Var Parsed, Var Resolved) -> m a -> m a
extend (v, v') =
  local (\x -> x { varScope = Map.insert v v' (varScope x) })

extendN :: (MonadGen Int m, MonadReader Scope m) => [(Var Parsed, Var Resolved)] -> m a -> m a
extendN vs =
  local (\x -> x { varScope = foldr (uncurry Map.insert) (varScope x) vs })

extendTy :: (MonadGen Int m, MonadReader Scope m) => (Var Parsed, Var Resolved) -> m a -> m a
extendTy (v, v') =
  local (\x -> x { tyScope = Map.insert v v' (tyScope x) })

extendTyN :: (MonadGen Int m, MonadReader Scope m) => [(Var Parsed, Var Resolved)] -> m a -> m a
extendTyN vs =
  local (\x -> x { tyScope = foldr (uncurry Map.insert) (tyScope x) vs })
