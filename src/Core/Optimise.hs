{-# LANGUAGE ConstraintKinds, ScopedTypeVariables, TupleSections, GeneralizedNewtypeDeriving, DerivingStrategies, BangPatterns #-}
module Core.Optimise
  ( substitute, substituteInTys
  , module Core.Core
  , Var(..)
  , fresh
  ) where

import qualified Data.Map.Strict as Map

import Data.Generics (everywhere, mkT)
import Data.VarSet (IsVar(..))

import Control.Monad.Infer (fresh)

import Core.Core
import Syntax

substitute :: IsVar a => Map.Map a (CoAtom a) -> CoTerm a -> CoTerm a
substitute m = everywhere (mkT subst) where
  subst e@(CoaRef v _) = Map.findWithDefault e v m
  subst e = e

substituteInTys :: IsVar a => Map.Map a (CoType a) -> CoTerm a -> CoTerm a
substituteInTys m = everywhere (mkT subst) where
  subst t@(CotyVar v) = Map.findWithDefault t v m
  subst x = x
