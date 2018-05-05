{-# LANGUAGE GeneralizedNewtypeDeriving, DerivingStrategies, FlexibleInstances #-}
module Data.VarMap
  ( Map
  , fromList
  , lookup, member
  , insert, delete
  , map, singleton, union, unionSemigroup
  , (<>), mempty
  ) where

import qualified Data.IntMap.Strict as Map
import qualified Data.IntMap.Merge.Strict as Map
import Prelude hiding (lookup, map)

import Syntax.Pretty (Var(..), Resolved)

import Data.Coerce

newtype Map a
  = Map (Map.IntMap a)
  deriving (Eq, Show, Ord)
  deriving newtype (Semigroup, Monoid)

insert :: Var Resolved -> a -> Map a -> Map a
insert (TgName _ x) v (Map k) = Map (Map.insert x v k)
insert _ _ x = x

delete :: Var Resolved -> Map a -> Map a
delete (TgName _ x) (Map k) = Map (Map.delete x k)
delete _ x = x

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

map :: (a -> b) -> Map a -> Map b
map f (Map a) = Map (Map.map f a)

singleton :: Var Resolved -> a ->  Map a
singleton (TgName _ x) v = coerce (Map.singleton x v)
singleton _ _ = Map Map.empty

unionSemigroup :: Semigroup a => Map a -> Map a -> Map a
unionSemigroup (Map l) (Map r) = Map (Map.merge Map.preserveMissing Map.preserveMissing (Map.zipWithMatched (const (<>))) l r)
