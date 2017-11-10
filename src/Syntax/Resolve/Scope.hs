{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Syntax.Resolve.Scope
  ( Scope(..)
  , builtinScope, tagVar
  , extend, extendN
  , extendTy, extendTyN
  ) where

import qualified Data.Text as T
import qualified Data.Map as M
import Control.Monad.Gen
import Control.Monad.Reader

import Syntax

data Scope = Scope { varScope :: M.Map (Var Parsed) (Var Resolved)
                   , tyScope  :: M.Map (Var Parsed) (Var Resolved)
                   }
  deriving (Eq, Ord, Show)

builtinScope :: Scope
builtinScope = Scope
               { varScope = build [ "+", "-", "*", "/", "**", "^"
                                  , "<", ">", ">=", "<=", "==", "<>"
                                  , "||", "&&" ]
               , tyScope = build [ "int", "string", "bool", "unit" ]
               }
  where build :: [T.Text] -> M.Map (Var Parsed) (Var Resolved)
        build = foldr (\v -> M.insert (Name v) (ReName (TgInternal v))) M.empty


tagVar :: MonadGen Int m => Var Parsed -> m (Var Resolved)
tagVar (Name n) = ReName . TgName n <$> gen

extend :: (MonadGen Int m, MonadReader Scope m) => (Var Parsed, Var Resolved) -> m a -> m a
extend (v, v') =
  local (\x -> x { varScope = M.insert v v' (varScope x) })

extendN :: (MonadGen Int m, MonadReader Scope m) => [(Var Parsed, Var Resolved)] -> m a -> m a
extendN vs =
  local (\x -> x { varScope = foldr (uncurry M.insert) (varScope x) vs })

extendTy :: (MonadGen Int m, MonadReader Scope m) => (Var Parsed, Var Resolved) -> m a -> m a
extendTy (v, v') =
  local (\x -> x { tyScope = M.insert v v' (tyScope x) })

extendTyN :: (MonadGen Int m, MonadReader Scope m) => [(Var Parsed, Var Resolved)] -> m a -> m a
extendTyN vs =
  local (\x -> x { tyScope = foldr (uncurry M.insert) (tyScope x) vs })
