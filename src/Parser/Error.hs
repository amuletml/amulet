module Parser.Error where

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
  -- Parsing errors
  | UnexpectedToken Token [String]
  -- Various warnings
  | UnindentIn SourcePos SourcePos
  | UnindentContext SourcePos SourcePos
  | MisalignedArm SourcePos SourcePos
  deriving (Show)

instance Pretty ParseError where
  pretty (Failure _ s) = string s

  pretty (UnexpectedCharacter _ c) = string "Unexpected character" <+> shown c
  pretty (UnexpectedEnd _) = string "Unexpected end of input"

  pretty (UnexpectedToken (Token s _) []) = string "Unexpected" <+> shown s
  pretty (UnexpectedToken (Token s _) [x]) = string "Unexpected" <+> shown s <> string ", expected" <+> string x
  pretty (UnexpectedToken (Token s _) xs) = string "Unexpected" <+> shown s <> string ", expected one of" <+> hsep (punctuate comma (map string xs))

  pretty (UnindentIn _ p) = string "The in is misaligned with the corresponding 'let'" <+> parens (string "located at" <+> shown (spLine p) <> colon <> shown (spCol p))
  pretty (MisalignedArm _ _) = string "This match arm is misaligned with other arms in the pattern"

instance Spanned ParseError where
  annotation (Failure p _) = mkSpan1 p

  annotation (UnexpectedCharacter p _) = mkSpan1 p
  annotation (UnexpectedEnd p) = mkSpan1 p

  annotation (UnexpectedToken (Token _ p) _) = mkSpan1 p

  annotation (UnindentIn p _) = mkSpan1 p
  annotation (UnindentContext p _) = mkSpan1 p
  annotation (MisalignedArm p _) = mkSpan1 p
