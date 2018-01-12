{-# LANGUAGE DefaultSignatures, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE TypeOperators, GADTs #-}
module Data.Spanned (Spanned(..)) where

import Data.Span
import Data.Data

class Spanned a where
  annotation :: a -> Span
  default annotation :: Data a => a -> Span
  annotation = get . gmapQ getSpan where
    get (Just x:_) = x
    get (Nothing:xs) = get xs
    get [] = internal

    getSpan d = case cast d of
                  Just x -> Just (x :: Span)
                  Nothing -> Nothing
