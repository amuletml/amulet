{-# LANGUAGE
  DefaultSignatures
, FlexibleInstances
, FlexibleContexts
, TypeOperators
, GADTs
, MultiParamTypeClasses
, TypeFamilies #-}

-- | Many terms in the compiler are associated with a 'Span' of source
-- code, such as expressions or patterns. We refer to these types as
-- 'Spanned'.
module Data.Spanned
  ( Spanned(..)
  , Annotated(..)
  , AnnotatedVia(..)
  ) where

import Data.Span
import Data.Data

-- | A type which is associated with some bit of source code
class Spanned a where
  -- Extract the span from this value
  spanOf :: a -> Span
  default spanOf :: Data a => a -> Span
  spanOf = get . gmapQ getSpan where
    get (Just x:_) = x
    get (Nothing:xs) = get xs
    get [] = internal

    getSpan d = case cast d of
                  Just x -> Just (x :: Span)
                  Nothing -> Nothing

instance Spanned Span where
  spanOf = id

instance Spanned a => Spanned (a, b) where
  spanOf (x, _) = spanOf x

class Annotated e where
  type Annotation e
  annotation :: e -> Annotation e

-- | A wrapper for 'Annotated', useful for deriving-via.
newtype AnnotatedVia e s = AnnotatedVia e

instance (Annotated e, Spanned s, s ~ (Annotation e)) => Spanned (AnnotatedVia e s) where
  spanOf (AnnotatedVia x) = spanOf . annotation $ x
