{-# LANGUAGE OverloadedStrings
           , MultiParamTypeClasses
           , FunctionalDependencies
           , FlexibleContexts #-}
module Text.Pretty.Note
  ( NoteKind(..)
  , NoteStyle
  , NoteDoc
  , Note(..)
  , FileMap
  , fileSpans, format, toAnsi
  ) where

import qualified Data.Text as T
import Data.Foldable
import Data.Position
import Data.Spanned
import Data.Span
import Data.List

import Text.Pretty.Semantic (Style, pretty)
import Text.Pretty.Ansi
import Text.Pretty

data NoteKind = WarningMessage | ErrorMessage
  deriving (Show, Eq, Ord)

data NoteStyle
  = LinePrefix | LineHighlight
  | NoteKind NoteKind
  deriving (Show, Eq)

type NoteDoc a = Doc (Either NoteStyle a)

class Spanned a => Note a b | a -> b where
  diagnosticKind :: a -> NoteKind

  formatNote :: ([Span] -> NoteDoc b) -> a -> NoteDoc b

type FileMap = [(SourceName, T.Text)]

format :: Note a Style => ([Span] -> NoteDoc Style) -> a -> NoteDoc Style
format f x =
  let a = annotation x
      c = case diagnosticKind x of
            WarningMessage -> annotate (NoteKind WarningMessage) "warning"
            ErrorMessage -> annotate (NoteKind ErrorMessage) "error"
      body = formatNote f x
  in (Right <$> pretty a <> colon) <+> (Left <$> c) <##> body

toAnsi :: NoteStyle -> AnsiStyle
toAnsi LinePrefix = BrightColour Blue
toAnsi LineHighlight = AnsiStyles [BrightColour White, Underlined]
toAnsi (NoteKind WarningMessage) = BrightColour Yellow
toAnsi (NoteKind ErrorMessage) = BrightColour Red

fileSpans :: FileMap -> [Span] -> NoteDoc b
fileSpans files locs@(loc:_) =
  let
    locs' = overlapping (sortBy compareFiles locs)

    startLine = spLine . spanStart . head $ locs'
    endLine = spLine . spanEnd . last $ locs'
    lines = T.lines (maybe T.empty snd (find ((==fileName loc) . fst) files))

    lineWidth = 1 + floor (logBase 10 (fromIntegral endLine :: Double)) :: Int
    putLine before body = annotate (Left LinePrefix) (text (T.justifyRight lineWidth ' ' before)
                                                       <> space <> pipe <> space)
                          <> body

  in vsep (putLine mempty mempty : buildLines putLine startLine lines locs' ++ [ putLine mempty mempty ])
  where
    pipe = char '│'

    compareFiles a b
      | fileName a == fileName b =
      let a' = spanStart a
          b' = spanStart b
      in (spLine a', spCol a') `compare` (spLine b', spCol b')
      | otherwise = error "cannot compare spans from different files"
fileSpans _ [] = error "must have at least one span"

buildLines :: (T.Text -> NoteDoc a -> NoteDoc a) -> Int -> [T.Text] -> [Span] -> [NoteDoc a]
buildLines b s = go s . drop (s - 1) where
  go n (t:ts) (x:xs) = case spLine (spanStart x) - n of
    -- If we need this line then emit it
    0 -> b (T.pack (show n)) (buildLine t n (x:xs)) :
         go (n + 1) ts (dropLines n (x:xs))
    -- If we need the next line, just emit this one plain
    1 -> b (T.pack (show n)) (text t) : go (n + 1) ts (x:xs)
    -- Otherwise drop the required number of lines
    m -> b mempty (string "...") : go (n + m) (drop m (t:ts)) (x:xs)
  go _ _ _ = []

  -- Drop lines finishing before this one, and patch up the start position of
  -- any lines overlapping with it.
  dropLines _ [] = []
  dropLines n (x:xs)
    | spLine e <= n = dropLines n xs
    | spLine s <= n = mkSpanUnsafe (SourcePos f (n + 1) 1) (spanEnd x):xs
    | otherwise = x:xs

    where (f, s, e) = (fileName x, spanStart x, spanEnd x)

buildLine :: T.Text -> Int -> [Span] -> NoteDoc a
buildLine = go 1 where
  go c t l (x:xs) | spLine s <= l =
    let start = if spLine s < l then 1 else spCol s
        len = if spLine e > l then T.length t else spCol e - start + 1

        (before, remaining) = splitPadded (start - c) t
        (body, after) = splitPadded len remaining
    in text before <> annotate (Left LineHighlight) (text body) <> go (start + len) after l xs
    where (s, e) = (spanStart x, spanEnd x)
          splitPadded i t | i <= T.length t = T.splitAt i t
                          | otherwise = (t <> T.replicate (i - T.length t) (T.singleton ' '), mempty)
  go _ t _ _ = text t


overlapping :: [Span] -> [Span]
overlapping = go where
  go (x:y:xs)
    | spanEnd x `after` spanStart y
    = go (mkSpanUnsafe (spanStart x) (maxPos (spanEnd x) (spanEnd y)):xs)
    | otherwise = x : go (y:xs)
  go x = x

  after l r = case spLine l `compare` spLine r of
                EQ -> spCol l >= spCol r
                LT -> False
                GT -> True

  maxPos l r = if after l r then l else r
