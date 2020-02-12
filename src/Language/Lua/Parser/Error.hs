{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses #-}

-- | Represents and handles errors within the parsing process
module Language.Lua.Parser.Error
  ( ParseError(..)
  ) where

import Data.Position
import Data.Spanned
import Data.Span
import Data.Char

import Text.Pretty.Semantic (Pretty(..), Style)
import Text.Pretty.Note
import Text.Pretty

import Language.Lua.Parser.Token

-- | An error in the parsing process
--
-- It's worth noting that not all errors are irrecoverable, though all
-- suggest some form of incorrect code. One should use 'diagnosticKind'
-- to determine how serious an error is.
data ParseError
  -- | Represents an error with an arbitrary message. This is used by
  -- 'fail'
  = Failure SourcePos String

  -- | An unexpected character in the input text
  | UnexpectedCharacter SourcePos Char
  -- | The end of the file was unexpectedly reached
  | UnexpectedEnd SourcePos
  -- | A string was not correctly terminated (due to a new line or the
  -- end of a file)
  | UnclosedString SourcePos SourcePos
  -- | A comment was not correctly terminated (due to the end of the
  -- file).
  | UnclosedComment SourcePos SourcePos

  -- | An unexpected token appeared in the lexer stream
  | UnexpectedToken Token [String]
  -- | An expression, where a statement was expected
  | MalformedStatement
  deriving (Eq, Show)

instance Pretty ParseError where
  pretty (Failure _ s) = string s

  pretty (UnexpectedCharacter _ c) | isPrint c = "Unexpected character '" <> string [c] <> "'"
                                   | otherwise = "Unexpected character" <+> shown c
  pretty (UnexpectedEnd _) = string "Unexpected end of input"
  pretty (UnclosedString _ p) = "Unexpected end of input, expecting to close string started at" <+> prettyPos p
  pretty (UnclosedComment _ p) = "Unexpected end of input, expecting to close comment started at" <+> prettyPos p

  pretty (UnexpectedToken (Token s _ _) []) = "Unexpected" <+> string (show s)
  pretty (UnexpectedToken (Token s _ _) [x]) = "Unexpected" <+> string (show s) <> ", expected" <+> string x
  pretty (UnexpectedToken (Token s _ _) xs) =
    "Unexpected" <+> string (show s) <> ", expected one of" <+> hsep (punctuate comma (map string xs))

  pretty MalformedStatement = "Malformed statement: received an expression instead"

instance Spanned ParseError where
  spanOf (Failure p _) = mkSpan1 p

  spanOf (UnexpectedCharacter p _) = mkSpan1 p
  spanOf (UnexpectedEnd p) = mkSpan1 p
  spanOf (UnclosedString p _) = mkSpan1 p
  spanOf (UnclosedComment p _) = mkSpan1 p

  spanOf (UnexpectedToken t _) = spanOf t
  spanOf MalformedStatement = internal

prettyPos :: SourcePos -> Doc a
prettyPos p = shown (spLine p) <> colon <> shown (spCol p)

instance Note ParseError Style where
  diagnosticKind _ = ErrorMessage

  formatNote f (UnclosedString p s)
    = indent 2 "Unexpected end of input, expecting to close string"
      <##> f [mkSpan1 s, mkSpan1 p]
  formatNote f (UnclosedComment p s)
    = indent 2 "Unexpected end of input, expecting to close comment"
      <##> f [mkSpan1 s, mkSpan1 p]
  formatNote f x
    = indent 2 (Right <$> pretty x)
      <##> f [spanOf x]
