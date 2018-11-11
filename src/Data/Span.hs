{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | Spans, created by the lexer, represent the length and position of a
-- token (indirectly) for use in error reporting. Spans are kept alive
-- through much of the compiler runtime, up to Core generation.
module Data.Span
  ( Span(fileName)
  , internal
  , mkSpan, mkSpanUnsafe, mkSpan1
  , spanStart
  , spanEnd
  , formatSpan
  , includes
  ) where

import Text.Pretty.Semantic (Pretty(..))
import Text.Pretty

import Data.Data
import Data.Position

-- ^ The span from which a token was parsed.
data Span
  = Span { fileName :: SourceName -- ^ What file name this token came from
         , col1 :: !Column -- ^ Starting column
         , line1 :: !Line -- ^ Starting line
         , col2 :: !Column -- ^ Ending column
         , line2 :: !Line -- ^ Ending line
         }
  deriving (Eq, Ord, Show, Data)

-- | Compiler-generated structures get the 'internal' span for lack of
-- somewhere better to assign them to.
internal :: Span
internal = Span "<wired in>" 0 0 0 0

-- | Make a 'Span' from two 'SourcePos'itions. If the files differ, this
-- returns 'Nothing'.
mkSpan :: SourcePos -> SourcePos -> Maybe Span
mkSpan a b
  | spFile a == spFile b
  = Just Span { fileName = spFile a
              , col1 = spCol a, line1 = spLine a
              , col2 = spCol b, line2 = spLine b }
  | otherwise = Nothing

-- | Make a 'Span', see 'mkSpan'. If the files differ, the file from the
-- first 'SourcePos'ition is picked.
mkSpanUnsafe :: SourcePos -> SourcePos -> Span
mkSpanUnsafe a b = Span { fileName = spFile a
                        , col1 = spCol a, line1 = spLine a
                        , col2 = spCol b, line2 = spLine b }

-- | Make a 'Span' from a single 'SourcePos'ition.
mkSpan1 :: SourcePos -> Span
mkSpan1 a = Span { fileName = spFile a
                 , col1 = spCol a, line1 = spLine a
                 , col2 = spCol a, line2 = spLine a }

-- | The 'SourcePos'ition a span starts at
spanStart :: Span -> SourcePos
spanStart Span { fileName = n, line1 = l, col1 = c } = SourcePos n l c

-- | The 'SourcePos'ition a span ends at
spanEnd :: Span -> SourcePos
spanEnd   Span { fileName = n, line2 = l, col2 = c } = SourcePos n l c

includes :: Span -> Span -> Bool
includes (Span f c1 l1 c2 l2) (Span f' c1' l1' c2' l2') =
  f == f' && l1 == l1' && l2 == l2' && c1 > c1' && c2 > c2'

-- | Format a 'Span' for human-readable (i.e., error message)
-- pretty-printing.
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
