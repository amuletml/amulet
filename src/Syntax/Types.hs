{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances, StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Syntax.Types
  ( Telescope, one, foldTele, teleFromList, mapTele
  , Scope(..), namesInScope, inScope
  , Env, freeInEnv, difference, envOf, scopeFromList, toMap
  , values, types, typeVars, constructors, relevantTVs
  , Origin(..)

  , focus
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Data.Semigroup

import Control.Arrow
import Control.Lens

import Syntax.Pretty
import Syntax.Subst

-- A bag of bindings, returned from pattern match checking
newtype Telescope p =
  Telescope { getTele :: Map.Map (Var Resolved) (Type p) }
  deriving newtype (Semigroup, Monoid)

deriving instance (Ord (Var p), Ord (Ann p)) => Ord (Telescope p)
deriving instance (Ord (Ann p), Ord (Var p)) => Eq (Telescope p)

type instance Index (Telescope p) = Var Resolved
type instance IxValue (Telescope p) = Type p
instance Ord (Var p) => Ixed (Telescope p) where
  ix k f (Telescope m) = Telescope <$> ix k f m

instance Ord (Var p) => At (Telescope p) where
  at k f (Telescope m) = Telescope <$> at k f m

newtype Scope p f =
  Scope { getScope :: Map.Map (Var p) f }

deriving instance (Show (Var p), Show f) => Show (Scope p f)
deriving instance (Ord (Var p), Ord f) => Ord (Scope p f)
deriving instance (Ord (Var p), Ord f) => Eq (Scope p f)
deriving instance Ord (Var p) => Semigroup (Scope p f)
deriving instance Ord (Var p) => Monoid (Scope p f)

instance (Ord (Var p), Ord (Ann p)) => Traversable (Scope p) where
  traverse f (Scope m) = Scope <$> traverse f m

instance Ord (Var p) => Foldable (Scope p) where
  foldMap f (Scope m) = foldMap f m

instance (Ord (Var p), Ord (Ann p)) => Functor (Scope p) where
  fmap f (Scope m) = Scope (fmap f m)

type instance Index (Scope p f) = Var p
type instance IxValue (Scope p f) = f
instance Ord (Var p) => Ixed (Scope p f) where
  ix k f (Scope m) = Scope <$> ix k f m

instance Ord (Var p) => At (Scope p f) where
  at k f (Scope m) = Scope <$> at k f m

data Env
  = Env { _values       :: Scope Resolved (Type Typed)
        , _types        :: Scope Resolved (Type Typed)
        , _relevantTVs  :: Scope Resolved (Type Typed)
        , _typeVars     :: Set.Set (Var Resolved)
        , _constructors :: Set.Set (Var Resolved)
        }
  deriving (Eq, Show, Ord)

makeLenses ''Env

(\\) :: Ord (Var p) => Scope p f -> Scope p f -> Scope p f
Scope x \\ Scope y = Scope (x Map.\\ y)

instance Monoid Env where
  mappend = (<>)
  mempty = Env mempty mempty mempty mempty mempty

instance Semigroup Env where
  Env a b s c d <> Env a' b' s' c' d' = Env (a <> a') (b <> b') (s <> s') (c <> c') (d <> d')

difference :: Env -> Env -> Env
difference (Env ma mb ms mc md) (Env ma' mb' ms' mc' md') = Env (ma \\ ma') (mb \\ mb') (ms \\ ms') (mc Set.\\ mc') (md Set.\\ md')

freeInEnv :: Env -> Set.Set (Var Typed)
freeInEnv = foldMap ftv . view values

envOf :: Scope Resolved (Type Typed) -> Scope Resolved (Type Typed) -> Env
envOf a b = Env a b mempty mempty mempty

scopeFromList :: Ord (Var p) => [(Var p, f)] -> Scope p f
scopeFromList = Scope . Map.fromList

namesInScope :: Scope p f -> [Var p]
namesInScope (Scope m) = Map.keys m

inScope :: Ord (Var p) => Var p -> Scope p f -> Bool
inScope v (Scope m) = v `Map.member` m

focus :: Telescope t -> Scope Resolved (Type t) -> Scope Resolved (Type t)
focus m s = Scope (getScope s <> getTele m)

class Degrade r where
  degrade :: Var r -> Var Resolved

instance Degrade Resolved where degrade = id
instance Degrade Typed where degrade = unTvName

one :: Degrade k => Var k -> Type p -> Telescope p
one k t = Telescope (Map.singleton (degrade k) t)

foldTele :: Monoid m => (Type p -> m) -> Telescope p -> m
foldTele f x = foldMap f (getTele x)

mapTele :: Ord (Var p) => (Type p -> Type p) -> Telescope p -> Telescope p
mapTele f (Telescope x) = Telescope (fmap f x)

teleFromList :: Degrade p
             => [(Var p, Type p)] -> Telescope p
teleFromList = Telescope . Map.fromList . map (first degrade)

toMap :: Scope p f -> Map.Map (Var p) f
toMap = getScope

data Origin
  = Supplied -- the programmer supplied this type
  | Guessed -- the compiler invented this type
  deriving Show
