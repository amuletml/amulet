{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Data.Span
  ( Span(fileName)
  , internal
  , mkSpan, mkSpanUnsafe, mkSpan1
  , spanStart
  , spanEnd
  , formatSpan
  ) where

import Text.Pretty.Semantic (Pretty(..))
import Text.Pretty

import Data.Data
import Data.Position

data Span
  = Span { fileName :: SourceName
         , col1 :: !Column, line1 :: !Line
         , col2 :: !Column, line2 :: !Line }
  deriving (Eq, Ord, Show, Data)

internal :: Span
internal = Span "<wired in>" 0 0 0 0

mkSpan :: SourcePos -> SourcePos -> Maybe Span
mkSpan a b
  | spFile a == spFile b
  = Just Span { fileName = spFile a
              , col1 = spCol a, line1 = spLine a
              , col2 = spCol b, line2 = spLine b }
  | otherwise = Nothing

mkSpanUnsafe :: SourcePos -> SourcePos -> Span
mkSpanUnsafe a b = Span { fileName = spFile a
                        , col1 = spCol a, line1 = spLine a
                        , col2 = spCol b, line2 = spLine b }

mkSpan1 :: SourcePos -> Span
mkSpan1 a = Span { fileName = spFile a
                 , col1 = spCol a, line1 = spLine a
                 , col2 = spCol a, line2 = spLine a }

spanStart, spanEnd :: Span -> SourcePos
spanStart Span { fileName = n, line1 = l, col1 = c } = SourcePos n l c
spanEnd   Span { fileName = n, line2 = l, col2 = c } = SourcePos n l c

formatSpan :: Span -> Doc a
formatSpan Span { fileName = n
              , line1 = l1, col1 = c1
              , line2 = l2, col2 = c2 }
    | n == "<wired in>" = string "internal"
    | otherwise = string n <> brackets (int l1 <> colon <> int c1 <+> string ".." <> int l2 <> colon <> int c2)

instance Pretty Span where
  pretty = formatSpan

instance Semigroup Span where
  x@(Span fa ca1 la1 _ _) <> y@(Span fb _ _ cb2 lb2)
    | fa == fb = Span fa ca1 la1 cb2 lb2
    | x == internal = y
    | y == internal = x
    | otherwise = error $ "<> spans of different files: " ++ fa ++ ", " ++ fb

