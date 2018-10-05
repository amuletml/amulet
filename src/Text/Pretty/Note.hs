{-# LANGUAGE
  FunctionalDependencies
, MultiParamTypeClasses
, OverloadedStrings #-}

-- | A series of utilities for generating annotating source positions
-- with error messages.
module Text.Pretty.Note
  ( NoteKind(..)
  , NoteStyle(..)
  , NoteDoc
  , Highlighted
  , Note(..)
  , FileMap
  , defaultHighlight, fileSpans, format, toAnsi
  ) where

import qualified Data.Text as T
import Data.Foldable
import Data.Position
import Data.Spanned
import Data.Span
import Data.List

import Text.Pretty.Ansi
import Text.Pretty

-- | The severity of a note
data NoteKind = WarningMessage | ErrorMessage
  deriving (Show, Eq, Ord)

-- | The document style for a note
data NoteStyle
  = LinePrefix | LineHighlight
  | NoteKind NoteKind
  deriving (Show, Eq)

-- | A document styled with either 'NoteKind' or 'a'.
type NoteDoc a = Doc (Either NoteStyle a)

-- | A highlighted string. Built from pairs of annotation functions and the
-- corresponding text.
type Highlighted a = [(T.Text -> NoteDoc a, T.Text)]

-- | Some diagnostic "note" which can be reported
class Spanned a => Note a b | a -> b where
  -- | The kind of this note
  diagnosticKind :: a -> NoteKind
  -- | Convert a note into some document
  formatNote :: ([Span] -> NoteDoc b) -- ^ A function which renders one or more 'Span's in a readable manner.
             -> a -- ^ The note to convert
             -> NoteDoc b

-- | A mapping of file names and their contents
type FileMap = [(SourceName, T.Text)]

-- | Format a note. This simply wraps 'formatNote' with some additional
-- information.
format :: Note a b => ([Span] -> NoteDoc b) -> a -> NoteDoc b
format f x =
  let a = annotation x
      c = case diagnosticKind x of
            WarningMessage -> annotate (NoteKind WarningMessage) "warning"
            ErrorMessage -> annotate (NoteKind ErrorMessage) "error"
      body = formatNote f x
  in (Right <$> formatSpan a <> colon) <+> (Left <$> c) <##> body

-- | Convert a note style to an ANSI style
toAnsi :: NoteStyle -> AnsiStyle
toAnsi LinePrefix = BrightColour Blue
toAnsi LineHighlight = AnsiStyles [BrightColour White]
toAnsi (NoteKind WarningMessage) = BrightColour Yellow
toAnsi (NoteKind ErrorMessage) = BrightColour Red

-- | The default highlighting function
defaultHighlight :: T.Text -> Highlighted a
defaultHighlight x = [(text, x)]

-- | A pretty printer for 'formatNote' and 'format', which displays the
-- source code associated with each span argument.
fileSpans :: FileMap -> (T.Text -> Highlighted a)
          -> [Span] -> NoteDoc a
fileSpans files hlight locs =
  case overlapping . sortBy compareFiles . filter (/=internal) $ locs of
    [] -> mempty
    locs'@(loc:_) ->
      let
        startLine = spLine . spanStart . head $ locs'
        endLine = spLine . spanEnd . last $ locs'
        lines = T.lines (maybe T.empty snd (find ((==fileName loc) . fst) files))

        lineWidth = 1 + floor (logBase 10 (fromIntegral endLine :: Double)) :: Int
        putLine before body = annotate (Left LinePrefix) (text (T.justifyRight lineWidth ' ' before)
                                                           <> space <> pipe <> space)
                              <> body

      in vsep (putLine mempty mempty : buildLines hlight putLine startLine lines locs')
  where
    pipe = char '│'

    compareFiles a b
      | fileName a == fileName b =
      let a' = spanStart a
          b' = spanStart b
      in (spLine a', spCol a') `compare` (spLine b', spCol b')
      | otherwise = error "cannot compare spans from different files"

-- | Highlight some lines from the provided document.
buildLines :: (T.Text -> Highlighted a)
           -- ^ The highlighter function
           -> (T.Text -> NoteDoc a -> NoteDoc a)
           -- ^ Build a completed line from it's descriptor and contents
           -> Int      -- ^ The line to start at
           -> [T.Text] -- ^ Each line within the file
           -> [Span]   -- ^ The spans to highlight
           -> [NoteDoc a]
buildLines hlight build start = go start . drop (start - 1) where
  go n (t:ts) (x:xs) = case spLine (spanStart x) - n of
    -- If we need this line then emit it
    0 -> build (T.pack (show n)) (annDoc (hlight t))
       : build mempty (buildLine (T.length t) n (x:xs))
       : go (n + 1) ts (dropLines n (x:xs))
    -- If we need the next line, just emit this one plain
    1 -> build (T.pack (show n)) (annDoc (hlight t)) : go (n + 1) ts (x:xs)
    -- Otherwise drop the required number of lines
    m -> build mempty (string "...") : go (n + m) (drop m (t:ts)) (x:xs)
  go _ _ [] = []
  go n [] xs = go n [mempty] xs

  -- | Drop lines finishing before this one, and patch up the start position of
  -- any lines overlapping with it.
  dropLines _ [] = []
  dropLines n (x:xs)
    | spLine (spanStart x) <= n = dropLines n xs
    | otherwise = x:xs

-- | Build a line, highlighting a series of provided spans
buildLine :: Int -- The current line's length
          -> Int -- ^ The current line number
          -> [Span] -- ^ The spans to highlight. These may start before the
                    -- current line and extend beyond it.
          -> NoteDoc a
buildLine tlen = go 1 where
  go c l (x:xs) | spLine s <= l =
    let start = if spLine s < l then 1 else spCol s
        len = if spLine e > l then tlen - start + 1 else spCol e - start + 1

        before = string (replicate (start - c) ' ')
        body = string (replicate len '^')
    in before <> annotate (Left LineHighlight) body <> go (start + len) l xs
    where (s, e) = (spanStart x, spanEnd x)
  go _ _ _ = mempty

annDoc :: Highlighted a -> NoteDoc a
annDoc = foldr ((<>) . uncurry ($)) mempty

-- | Merge overlapping spans together into one.
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
