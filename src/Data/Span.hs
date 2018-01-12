{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Data.Span
  ( Span(fileName)
  , internal
  , mkSpan, mkSpanUnsafe, mkSpan1
  , newPos
  , spanStart
  , spanEnd
  ) where

import Pretty
import Text.Parsec.Pos

import Data.Semigroup
import Data.Data

data Span
  = Span { fileName :: SourceName
         , col1 :: !Column, line1 :: !Line
         , col2 :: !Column, line2 :: !Line }
  deriving (Eq, Ord, Show, Data)

internal :: Span
internal = Span "<wired in>" 0 0 0 0

mkSpan :: SourcePos -> SourcePos -> Maybe Span
mkSpan a b
  | sourceName a == sourceName b
  = Just Span { fileName = sourceName a
              , col1 = sourceColumn a, line1 = sourceLine a
              , col2 = sourceColumn b, line2 = sourceLine b }
  | otherwise = Nothing

mkSpanUnsafe :: SourcePos -> SourcePos -> Span
mkSpanUnsafe a b = Span { fileName = sourceName a
                        , col1 = sourceColumn a, line1 = sourceLine a
                        , col2 = sourceColumn b, line2 = sourceLine b }

mkSpan1 :: SourcePos -> Span
mkSpan1 a = Span { fileName = sourceName a
                 , col1 = sourceColumn a, line1 = sourceLine a
                 , col2 = sourceColumn a, line2 = sourceLine a }

spanStart, spanEnd :: Span -> SourcePos
spanStart Span { fileName = n, line1 = l, col1 = c } = newPos n l c
spanEnd   Span { fileName = n, line2 = l, col2 = c } = newPos n l c

instance Pretty Span where
  pprint Span { fileName = n
              , line1 = l1, col1 = c1
              , line2 = l2, col2 = c2 }
    | n == "<wired in>" = kwClr "internal"
    | otherwise = n <+> "[" <+>  l1 <+> ":" <+> c1 <+> " .. " <+> l2 <+> ":" <+> c2 <+> "]"

instance Semigroup Span where
  x@(Span fa ca1 la1 _ _) <> y@(Span fb _ _ cb2 lb2)
    | fa == fb = Span fa ca1 la1 cb2 lb2
    | x == internal = y
    | y == internal = x
    | otherwise = error $ "<> spans of different files: " ++ fa ++ ", " ++ fb
