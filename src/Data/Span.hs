{-# OPTIONS_GHC -funbox-strict-fields #-}

module Data.Span
  ( Span(fileName)
  , mkSpan
  , spanStart
  , spanEnd
  ) where

import Pretty
import Text.Parsec.Pos

data Span
  = Span { fileName :: SourceName
         , col1 :: !Column, line1 :: !Line
         , col2 :: !Column, line2 :: !Line }
  deriving (Eq, Ord, Show)

mkSpan :: SourcePos -> SourcePos -> Maybe Span
mkSpan a b
  | sourceName a == sourceName b
  = Just Span { fileName = sourceName a
              , col1 = sourceColumn a, line1 = sourceLine a
              , col2 = sourceColumn b, line2 = sourceLine b }
  | otherwise = Nothing

spanStart, spanEnd :: Span -> SourcePos
spanStart Span { fileName = n, line1 = l, col1 = c } = newPos n l c
spanEnd   Span { fileName = n, line2 = l, col2 = c } = newPos n l c

instance Pretty Span where
  pprint Span { fileName = n
              , line1 = l1, col1 = c1
              , line2 = l2, col2 = c2 }
    = n <+> "[" <+>  l1 <+> ":" <+> c1 <+> " .. " <+> l2 <+> ":" <+> c2 <+> "]"
