{-# LANGUAGE DefaultSignatures, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE TypeOperators, GADTs #-}

-- | Many terms in the compiler are associated with a 'Span' of source
-- code, such as expressions or patterns. We refer to these types as
-- 'Spanned'.
module Data.Spanned (Spanned(..)) where

import Data.Span
import Data.Data

-- | A type which is associated with some bit of source code
class Spanned a where
  -- Extract the span from this value
  annotation :: a -> Span
  default annotation :: Data a => a -> Span
  annotation = get . gmapQ getSpan where
    get (Just x:_) = x
    get (Nothing:xs) = get xs
    get [] = internal

    getSpan d = case cast d of
                  Just x -> Just (x :: Span)
                  Nothing -> Nothing

instance Spanned Span where
  annotation x = x

instance Spanned a => Spanned (a, b) where
  annotation (x, _) = annotation x
