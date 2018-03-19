{-# LANGUAGE GeneralizedNewtypeDeriving, DerivingStrategies, FlexibleInstances #-}
module Data.VarSet
  ( Set
  , fromList, toList
  , member, notMember, insert
  , difference, union, intersection, singleton, delete
  , (<>), mempty, isEmpty
  , IsVar(..)
  ) where

import qualified Data.IntSet as Set
import qualified Data.Text as T

import Syntax.Pretty (Var(..), Resolved)

import Data.Semigroup
import Data.Coerce
import Data.Data

import Pretty(Pretty)

newtype Set
  = Set Set.IntSet
  deriving (Eq, Show, Ord)
  deriving newtype (Semigroup, Monoid)

insert :: Var Resolved -> Set -> Set
insert (TgName _ x) (Set k) = Set (Set.insert x k)
insert _ x = x

fromList :: [Var Resolved] -> Set
fromList = foldr insert mempty

toList :: Set -> [Var Resolved]
toList (Set s) = map (TgName (T.singleton '?')) (Set.toList s)

member :: Var Resolved -> Set -> Bool
member (TgName _ x) set = Set.member x (coerce set)
member _ _ = False

notMember :: Var Resolved -> Set -> Bool
notMember (TgName _ x) set = Set.notMember x (coerce set)
notMember _ _ = True

difference :: Set -> Set -> Set
difference = coerce Set.difference

union :: Set -> Set -> Set
union = coerce Set.union

intersection :: Set -> Set -> Set
intersection = coerce Set.intersection

singleton :: Var Resolved -> Set
singleton (TgName _ x) = coerce (Set.singleton x)
singleton _ = coerce Set.empty

delete :: Var Resolved -> Set -> Set
delete (TgName _ x) set = coerce (Set.delete x (coerce set))
delete _ set = set

isEmpty :: Set -> Bool
isEmpty = coerce Set.null

class (Data a, Eq a, Ord a, Pretty a, Show a) => IsVar a where
  toVar :: a -> Var Resolved
  fromVar :: Var Resolved -> a

instance IsVar (Var Resolved) where
  toVar = id
  fromVar = id
