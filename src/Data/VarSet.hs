{-# LANGUAGE GeneralizedNewtypeDeriving, DerivingStrategies, FlexibleInstances #-}
module Data.VarSet
  ( Set
  , null, size
  , fromList, toList, singleton
  , foldr, filter
  , member, notMember
  , insert, delete
  , difference, union, intersection
  , (<>), mempty, isEmpty
  ) where

import qualified Data.HashSet as Set
import qualified Data.List as L

import Prelude hiding (foldr, filter, null)

import Core.Var

import Data.Coerce

newtype Set
  = Set (Set.HashSet CoVar)
  deriving (Eq, Show, Ord)
  deriving newtype (Semigroup, Monoid)

null :: Set -> Bool
null (Set s) = Set.null s

size :: Set -> Int
size (Set x) = Set.size x

insert :: CoVar -> Set -> Set
insert x (Set k) = Set (Set.insert x k)

fromList :: [CoVar] -> Set
fromList = L.foldr insert mempty

toList :: Set -> [CoVar]
toList (Set s) = Set.toList s

foldr :: (CoVar -> a -> a) -> a -> Set -> a
foldr f a (Set s) = Set.foldr f a s

member :: CoVar -> Set -> Bool
member x set = Set.member x (coerce set)

notMember :: CoVar -> Set -> Bool
notMember x set = not (Set.member x (coerce set))

difference :: Set -> Set -> Set
difference = coerce Set.difference

union :: Set -> Set -> Set
union = coerce Set.union

intersection :: Set -> Set -> Set
intersection = coerce Set.intersection

singleton :: CoVar -> Set
singleton x = coerce (Set.singleton x)

delete :: CoVar -> Set -> Set
delete x set = coerce (Set.delete x (coerce set))

filter :: (CoVar -> Bool) -> Set -> Set
filter f (Set s) = Set (Set.filter f s)

isEmpty :: Set -> Bool
isEmpty = coerce Set.null
