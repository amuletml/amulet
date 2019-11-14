{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeOperators, TypeSynonymInstances #-}
module Generics.Constructor
  ( ConstructorName(..)
  , conNameOf
  ) where

import GHC.Generics

-- | Get the constructor name of a type.
conNameOf :: (Generic a, ConstructorName (Rep a)) => a -> String
conNameOf = gconNameOf . from

class ConstructorName f where
  gconNameOf :: f a -> String

instance (ConstructorName f, ConstructorName g) => ConstructorName (f :+: g) where
  gconNameOf (L1 x) = gconNameOf x
  gconNameOf (R1 x) = gconNameOf x

instance (ConstructorName f) => ConstructorName (D1 c f) where
  gconNameOf (M1 x) = gconNameOf x

instance (Constructor c) => ConstructorName (C1 c f) where
  gconNameOf x = conName x
