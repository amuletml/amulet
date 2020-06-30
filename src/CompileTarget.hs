{-# LANGUAGE GADTs, FlexibleContexts, MultiParamTypeClasses, RankNTypes #-}
module CompileTarget where

import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import Data.Position
import Data.Spanned

import Text.Pretty.Semantic
import Text.Pretty.Note

-- | Some arbitrary error which occurs from parsing.
data ParseError where
  ParseError :: forall a. (Note a Style, Pretty a) => a -> ParseError

instance Show ParseError where
  show (ParseError x) = show (pretty x)

instance Pretty ParseError where
  pretty (ParseError x) = pretty x

instance Spanned ParseError where
  spanOf (ParseError x) = spanOf x

instance Note ParseError Style where
  diagnosticKind (ParseError x) = diagnosticKind x
  formatNote f (ParseError x) = formatNote f x
  noteId (ParseError x) = noteId x

-- | Information about a compile target, which is required for the
-- frontend.
--
-- This does not provide information on how to actually compile core into
-- executable code.
data Target = Target
  { name :: T.Text -- ^ The name of this target.

    -- | Try to parse code generated by this parser,
  , parse :: SourcePos -> L.Text -> Either ParseError ()

    -- | Highlight this code for error reporting.
  , highlight :: T.Text -> Highlighted Style
  }

instance Show Target where
  show (Target name _ _) = "Target " ++ show name
