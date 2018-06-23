{-| So whilst Alex does support UTF8, it's rather hard to get it to work
  with character categories (such as only accepting upper case ones). The
  answer is simple: we cheat.

  Our lexer operates on latin1 encoded strings, mapping ASCII characters
  to themselves and other characters to elements in the range
  0xf0-0xf7. This allows us to match entire blocks of characters with a
  single character class.

  As Alex just tracks the number of characters consumed, extracting the
  actual character requires no real effort - it's only 'alexGetByte'
  which requires this special handling.
-}
module Parser.Unicode
  ( UnicodeClass
  , classify
  , asFakeWord
  ) where

import Data.Word (Word8)
import Data.Char

-- | A less specfic version of 'GeneralCategory', grouping characters
-- into a couple of key categories.
data UnicodeClass
  -- These are used to designate the beginning of symbols
  = Upper | Lower | Symbol

  -- Generic and digit can be used in identifiers, but not at the start of one
  | Generic | Digit

  | Whitespace

  -- These are  guaranteed parse order. The only difference is that "graphic" is printable, while
  -- other may not be.
  | OtherGraphic | Other
  deriving (Eq, Show)

-- | Determine the class for a given character.
classify :: Char -> UnicodeClass
classify c = case generalCategory c of
  -- See classification descriptions in
  -- <http://www.unicode.org/reports/tr44/tr44-14.html#GC_Values_Table>

  -- Cased letters
  UppercaseLetter       -> Upper
  LowercaseLetter       -> Lower
  TitlecaseLetter       -> Upper

  ModifierLetter        -> Generic
  OtherLetter           -> Lower

  NonSpacingMark        -> Generic
  SpacingCombiningMark  -> OtherGraphic
  EnclosingMark         -> OtherGraphic

  DecimalNumber         -> Digit
  LetterNumber          -> Generic
  OtherNumber           -> Digit

  ConnectorPunctuation  -> Symbol
  DashPunctuation       -> Symbol
  OpenPunctuation       -> OtherGraphic
  ClosePunctuation      -> OtherGraphic
  InitialQuote          -> OtherGraphic
  FinalQuote            -> OtherGraphic
  OtherPunctuation      -> Symbol

  MathSymbol            -> Symbol
  CurrencySymbol        -> Symbol
  ModifierSymbol        -> Symbol
  -- So this _could_ be Lower or something, just so we can allow for emoji variables.
  -- Hrmrm, maybe not.
  OtherSymbol           -> Symbol

  Space                 -> Whitespace

  -- This is all the wacky things in C* and Z* groups
  _                     -> Other

-- | Convert a character class into a fake byte which will be used by
-- "Parser.Lexer"
asFakeWord :: UnicodeClass -> Word8
asFakeWord Upper = 0xf0
asFakeWord Lower = 0xf1
asFakeWord Symbol = 0xf2
asFakeWord Generic = 0xf3
asFakeWord Digit = 0xf4
asFakeWord Whitespace = 0xf5
asFakeWord OtherGraphic = 0xf6
asFakeWord Other = 0xf7
