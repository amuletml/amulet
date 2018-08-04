{-# LANGUAGE GeneralizedNewtypeDeriving, DerivingStrategies, FlexibleInstances #-}
module Data.VarMap
  ( Map
  , fromList, toList, null
  , lookup, member, findWithDefault
  , insert, delete
  , map, mapWithKey, singleton, union, unionSemigroup
  , difference, intersection
  , foldrWithKey
  , (<>), mempty
  ) where

import qualified Data.HashMap.Strict as Map
import qualified Data.Text as T
import qualified Data.List as L
import Data.Coerce
import Prelude hiding (lookup, map, null)

import Control.Arrow

import Core.Var

newtype Map a
  = Map (Map.HashMap Int a)
  deriving (Eq, Show, Ord)
  deriving newtype (Semigroup, Monoid, Functor, Foldable)

null :: Map a -> Bool
null (Map m) = Map.null m

insert :: CoVar -> a -> Map a -> Map a
insert (CoVar x _ _) v (Map k) = Map (Map.insert x v k)

delete :: CoVar -> Map a -> Map a
delete (CoVar x _ _) (Map k) = Map (Map.delete x k)

fromList :: [(CoVar, a)] -> Map a
fromList = foldr (uncurry insert) mempty

toList :: Map a -> [(CoVar, a)]
toList (Map m) = L.map (first create) (Map.toList m)

member :: CoVar -> Map a -> Bool
member (CoVar x _ _) (Map m) = Map.member x m

findWithDefault :: a -> CoVar -> Map a -> a
findWithDefault d (CoVar x _ _) (Map m) = Map.lookupDefault d x m

lookup :: CoVar -> Map a -> Maybe a
lookup (CoVar x _ _) (Map m) = Map.lookup x m

union :: Map a -> Map a -> Map a
union (Map a) (Map b) = Map (Map.union a b)

difference :: Map a -> Map b -> Map a
difference (Map a) (Map b) = Map (Map.difference a b)

intersection :: Map a -> Map b -> Map a
intersection (Map a) (Map b) = Map (Map.intersection a b)

map :: (a -> b) -> Map a -> Map b
map f (Map a) = Map (Map.map f a)

mapWithKey :: (CoVar -> a -> b) -> Map a -> Map b
mapWithKey f (Map a) = Map (Map.mapWithKey (f . create) a)

-- filter :: (CoVar -> Bool) -> Map a -> Map a
-- filter f (Map m) = Map (Map.filter (f . create) m)

singleton :: CoVar -> a ->  Map a
singleton (CoVar x _ _) v = coerce (Map.singleton x v)

unionSemigroup :: Semigroup a => Map a -> Map a -> Map a
unionSemigroup (Map l) (Map r) = Map (Map.unionWith (<>) l r)

foldrWithKey :: (CoVar -> a -> b -> b) -> b -> Map a -> b
foldrWithKey f b (Map m) = Map.foldrWithKey (f . create) b m

create :: Int -> CoVar
create i = CoVar i (T.singleton '?') ValueVar
