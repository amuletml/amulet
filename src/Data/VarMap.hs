{-# LANGUAGE GeneralizedNewtypeDeriving, DerivingStrategies, FlexibleInstances #-}
module Data.VarMap
  ( Map
  , fromList
  , lookup, member
  , insert, singleton
  , union
  , (<>), mempty
  ) where

import qualified Data.IntMap.Strict as Map
import Prelude hiding (lookup)

import Syntax.Pretty (Var(..), Resolved)

import Data.Semigroup
import Data.Coerce

newtype Map a
  = Map (Map.IntMap a)
  deriving (Eq, Show, Ord)
  deriving newtype (Semigroup, Monoid)

insert :: Var Resolved -> a -> Map a -> Map a
insert (TgName _ x) v (Map k) = Map (Map.insert x v k)
insert _ _ x = x

fromList :: [(Var Resolved, a)] -> Map a
fromList = foldr (uncurry insert) mempty

member :: Var Resolved -> Map a -> Bool
member (TgName _ x) (Map m) = Map.member x m
member _ _ = False

lookup :: Var Resolved -> Map a -> Maybe a
lookup (TgName _ x) (Map m) = Map.lookup x m
lookup _ _ = Nothing

union :: Map a -> Map a -> Map a
union (Map a) (Map b) = Map (Map.union a b)

singleton :: Var Resolved -> a ->  Map a
singleton (TgName _ x) v = coerce (Map.singleton x v)
singleton _ _ = Map Map.empty
