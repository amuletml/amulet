{-# LANGUAGE GeneralizedNewtypeDeriving, DerivingStrategies #-}
module Data.VarSet
  ( Set
  , fromList
  , member, insert
  , difference, singleton, delete
  , (<>), mempty
  ) where

import qualified Data.IntSet as Set

import Syntax (Var(..), Resolved)

import Data.Semigroup
import Data.Coerce

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

singleton :: Var Resolved -> Set
singleton (TgName _ x) = coerce (Set.singleton x)
singleton _ = coerce (Set.empty)

delete :: Var Resolved -> Set -> Set
delete (TgName _ x) set = coerce (Set.delete x (coerce set))
delete _ set = set
