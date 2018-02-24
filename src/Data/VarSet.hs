{-# LANGUAGE GeneralizedNewtypeDeriving, DerivingStrategies, FlexibleInstances #-}
module Data.VarSet
  ( Set
  , fromList
  , member, insert
  , difference, union, singleton, delete
  , (<>), mempty
  , IsVar(..)
  ) where

import qualified Data.IntSet as Set

import Syntax.Pretty (Var(..), Resolved)

import Data.Semigroup
import Data.Coerce

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

member :: Var Resolved -> Set -> Bool
member (TgName _ x) set = Set.member x (coerce set)
member _ _ = False

difference :: Set -> Set -> Set
difference = coerce Set.difference

union :: Set -> Set -> Set
union = coerce Set.union

singleton :: Var Resolved -> Set
singleton (TgName _ x) = coerce (Set.singleton x)
singleton _ = coerce Set.empty

delete :: Var Resolved -> Set -> Set
delete (TgName _ x) set = coerce (Set.delete x (coerce set))
delete _ set = set

class (Eq a, Ord a, Pretty a, Show a) => IsVar a where
  toVar :: a -> Var Resolved

instance IsVar (Var Resolved) where
  toVar = id
