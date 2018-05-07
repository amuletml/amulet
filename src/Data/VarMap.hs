{-# LANGUAGE GeneralizedNewtypeDeriving, DerivingStrategies, FlexibleInstances #-}
module Data.VarMap
  ( Map
  , fromList, toList
  , lookup, member, findWithDefault
  , insert, insertWith, delete
  , map, singleton, union, unionSemigroup, unionOn
  , foldrWithKey
  , (<>), mempty
  ) where

import qualified Data.IntMap.Strict as Map
import qualified Data.IntMap.Merge.Strict as Map
import qualified Data.Text as T
import qualified Data.List as L
import Data.Coerce
import Prelude hiding (lookup, map)

import Control.Arrow

import Core.Var

newtype Map a
  = Map (Map.IntMap a)
  deriving (Eq, Show, Ord)
  deriving newtype (Semigroup, Monoid, Functor, Foldable)

insert :: CoVar -> a -> Map a -> Map a
insert (CoVar x _ _) v (Map k) = Map (Map.insert x v k)

insertWith :: (a -> a -> a) -> CoVar -> a -> Map a -> Map a
insertWith f (CoVar x _ _) v (Map k) = Map (Map.insertWith f x v k)

delete :: CoVar -> Map a -> Map a
delete (CoVar x _ _) (Map k) = Map (Map.delete x k)

fromList :: [(CoVar, a)] -> Map a
fromList = foldr (uncurry insert) mempty

toList :: Map a -> [(CoVar, a)]
toList (Map m) = L.map (first create) (Map.toList m)

member :: CoVar -> Map a -> Bool
member (CoVar x _ _) (Map m) = Map.member x m

findWithDefault :: a -> CoVar -> Map a -> a
findWithDefault d (CoVar x _ _) (Map m) = Map.findWithDefault d x m

lookup :: CoVar -> Map a -> Maybe a
lookup (CoVar x _ _) (Map m) = Map.lookup x m

union :: Map a -> Map a -> Map a
union (Map a) (Map b) = Map (Map.union a b)

map :: (a -> b) -> Map a -> Map b
map f (Map a) = Map (Map.map f a)

singleton :: CoVar -> a ->  Map a
singleton (CoVar x _ _) v = coerce (Map.singleton x v)

unionSemigroup :: Semigroup a => Map a -> Map a -> Map a
unionSemigroup  = unionOn (<>)

unionOn :: (a -> a -> a) -> Map a -> Map a -> Map a
unionOn f (Map l) (Map r) = Map (Map.merge Map.preserveMissing Map.preserveMissing (Map.zipWithMatched (const f)) l r)

foldrWithKey :: (CoVar -> a -> b -> b) -> b -> Map a -> b
foldrWithKey f b (Map m) = Map.foldrWithKey (f . create) b m

create :: Map.Key -> CoVar
create i = CoVar i (T.singleton '?') ValueVar
