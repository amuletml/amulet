module Parser.Error
  ( ParseError(..)
  ) where

import Data.Position
import Data.Spanned
import Data.Span

import Parser.Token
import Pretty

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
  | UnindentIn SourcePos SourcePos
  | UnindentContext SourcePos SourcePos
  deriving (Show)

instance Pretty ParseError where
  pretty (Failure _ s) = string s

  pretty (UnexpectedCharacter _ c) = string "Unexpected character" <+> shown c
  pretty (UnexpectedEnd _) = string "Unexpected end of input"
  pretty (UnclosedString _ p) = string "Unexpected end of input, expecting to close string started at" <+> prettyPos p
  pretty (UnclosedComment _ p) = string "Unexpected end of input, expecting to close comment started at" <+> prettyPos p

  pretty (UnexpectedToken (Token s _) []) = string "Unexpected" <+> string (friendlyName s)
  pretty (UnexpectedToken (Token s _) [x]) = string "Unexpected" <+> string (friendlyName s) <> string ", expected" <+> string x
  pretty (UnexpectedToken (Token s _) xs) = string "Unexpected" <+> string (friendlyName s) <> string ", expected one of" <+> hsep (punctuate comma (map string xs))

  pretty (UnindentIn _ p) = string "The in is misaligned with the corresponding 'let'" <+> parens (string "located at" <+> prettyPos p)
  pretty (UnindentContext _ p) = string "Possible incorrect indentation" </> string "This token is outside the context started at" <+> prettyPos p

instance Spanned ParseError where
  annotation (Failure p _) = mkSpan1 p

  annotation (UnexpectedCharacter p _) = mkSpan1 p
  annotation (UnexpectedEnd p) = mkSpan1 p
  annotation (UnclosedString p _) = mkSpan1 p
  annotation (UnclosedComment p _) = mkSpan1 p

  annotation (UnexpectedToken (Token _ p) _) = mkSpan1 p

  annotation (UnindentIn p _) = mkSpan1 p
  annotation (UnindentContext p _) = mkSpan1 p

prettyPos :: SourcePos -> Doc
prettyPos p = shown (spLine p) <> colon <> shown (spCol p)
