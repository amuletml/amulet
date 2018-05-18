{-# LANGUAGE OverloadedStrings #-}
module Parser.Error
  ( ParseError(..)
  ) where

import Data.Position
import Data.Spanned
import Data.Span
import Data.Char

import Parser.Token
import Text.Pretty.Semantic

data ParseError
  = Failure SourcePos String

  -- Lexing errors
  | UnexpectedCharacter SourcePos Char
  | UnexpectedEnd SourcePos
  | UnclosedString SourcePos SourcePos
  | UnclosedComment SourcePos SourcePos

  -- Parsing errors
  | UnexpectedToken Token [String]
  -- Various warnings
  | UnalignedIn SourcePos SourcePos
  | UnindentContext SourcePos SourcePos
  deriving (Show)

instance Pretty ParseError where
  pretty (Failure _ s) = string s

  pretty (UnexpectedCharacter _ c) | isPrint c = "Unexpected character '" <> string [c] <> "'"
                                   | otherwise = "Unexpected character" <+> shown c
  pretty (UnexpectedEnd _) = string "Unexpected end of input"
  pretty (UnclosedString _ p) = "Unexpected end of input, expecting to close string started at" <+> prettyPos p
  pretty (UnclosedComment _ p) = "Unexpected end of input, expecting to close comment started at" <+> prettyPos p

  pretty (UnexpectedToken (Token s _) []) = "Unexpected" <+> string (friendlyName s)
  pretty (UnexpectedToken (Token s _) [x]) = "Unexpected" <+> string (friendlyName s) <> ", expected" <+> string x
  pretty (UnexpectedToken (Token s _) xs) = "Unexpected" <+> string (friendlyName s) <> ", expected one of" <+> hsep (punctuate comma (map string xs))

  pretty (UnalignedIn _ p) = "The in is misaligned with the corresponding 'let'" <+> parens ("located at" <+> prettyPos p)
  pretty (UnindentContext _ p) = "Possible incorrect indentation" </> "This token is outside the context started at" <+> prettyPos p

instance Spanned ParseError where
  annotation (Failure p _) = mkSpan1 p

  annotation (UnexpectedCharacter p _) = mkSpan1 p
  annotation (UnexpectedEnd p) = mkSpan1 p
  annotation (UnclosedString p _) = mkSpan1 p
  annotation (UnclosedComment p _) = mkSpan1 p

  annotation (UnexpectedToken (Token _ p) _) = mkSpan1 p

  annotation (UnalignedIn p _) = mkSpan1 p
  annotation (UnindentContext p _) = mkSpan1 p

prettyPos :: SourcePos -> Doc
prettyPos p = shown (spLine p) <> colon <> shown (spCol p)
