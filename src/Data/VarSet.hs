{-# LANGUAGE GeneralizedNewtypeDeriving, DerivingStrategies, FlexibleInstances #-}
module Data.VarSet
  ( Set
  , fromList, toList
  , member, notMember, insert
  , difference, union, intersection, singleton, delete
  , (<>), mempty, isEmpty
  ) where

import qualified Data.IntSet as Set
import qualified Data.Text as T

import Core.Var

import Data.Coerce

newtype Set
  = Set Set.IntSet
  deriving (Eq, Show, Ord)
  deriving newtype (Semigroup, Monoid)

insert :: CoVar -> Set -> Set
insert (CoVar x _ _) (Set k) = Set (Set.insert x k)

fromList :: [CoVar] -> Set
fromList = foldr insert mempty

toList :: Set -> [CoVar]
toList (Set s) = map (\x -> CoVar x (T.singleton '?') ValueVar) (Set.toList s)

member :: CoVar -> Set -> Bool
member (CoVar x _ _) set = Set.member x (coerce set)

notMember :: CoVar -> Set -> Bool
notMember (CoVar x _ _) set = Set.notMember x (coerce set)

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

isEmpty :: Set -> Bool
isEmpty = coerce Set.null
