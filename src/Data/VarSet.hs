{-# LANGUAGE GeneralizedNewtypeDeriving, DerivingStrategies, FlexibleInstances #-}
module Data.VarSet
  ( Set
  , fromList, toList, singleton
  , foldr, filter
  , member, notMember
  , insert, delete
  , difference, union, intersection
  , (<>), mempty, isEmpty
  ) where

import qualified Data.HashSet as Set
import qualified Data.Text as T
import qualified Data.List as L
import Prelude hiding (foldr, filter)

import Core.Var

import Data.Coerce

newtype Set
  = Set (Set.HashSet Int)
  deriving (Eq, Show, Ord)
  deriving newtype (Semigroup, Monoid)

insert :: CoVar -> Set -> Set
insert (CoVar x _ _) (Set k) = Set (Set.insert x k)

fromList :: [CoVar] -> Set
fromList = L.foldr insert mempty

toList :: Set -> [CoVar]
toList (Set s) = map fake (Set.toList s)

foldr :: (CoVar -> a -> a) -> a -> Set -> a
foldr f a (Set s) = Set.foldr (f . fake) a s

member :: CoVar -> Set -> Bool
member (CoVar x _ _) set = Set.member x (coerce set)

notMember :: CoVar -> Set -> Bool
notMember (CoVar x _ _) set = not (Set.member x (coerce set))

difference :: Set -> Set -> Set
difference = coerce Set.difference

union :: Set -> Set -> Set
union = coerce Set.union

intersection :: Set -> Set -> Set
intersection = coerce Set.intersection

singleton :: CoVar -> Set
singleton (CoVar x _ _) = coerce (Set.singleton x)

delete :: CoVar -> Set -> Set
delete (CoVar x _ _) set = coerce (Set.delete x (coerce set))

filter :: (CoVar -> Bool) -> Set -> Set
filter f (Set s) = Set (Set.filter (f . fake) s)

isEmpty :: Set -> Bool
isEmpty = coerce Set.null

fake :: Int -> CoVar
fake x = CoVar x (T.singleton '?') ValueVar
