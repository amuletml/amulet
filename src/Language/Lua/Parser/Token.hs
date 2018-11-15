module Language.Lua.Parser.Token where

import Data.Text (unpack, Text)
import Data.Position
import Data.Spanned
import Data.Span

-- | The raw classification of a token without additional metadata. This
-- is the underlying representation of what the lexer produces and the
-- parser consumes.
data TokenClass
  = TcAnd      -- ^ An @and@ token.
  | TcBreak    -- ^ A @break@ token.
  | TcDo       -- ^ A @do@ token.
  | TcElse     -- ^ An @else@ token.
  | TcElseIf   -- ^ An @elseif@ token.
  | TcEnd      -- ^ An @end@ token.
  | TcFalse    -- ^ A @false@ token.
  | TcFor      -- ^ A @for@ token.
  | TcFunction -- ^ A @function@ .
  | TcIf       -- ^ An @if@ token.
  | TcIn       -- ^ An @in@ token.
  | TcLocal    -- ^ A @local@ token.
  | TcNil      -- ^ A @nil@ token.
  | TcNot      -- ^ A @not@ token.
  | TcOr       -- ^ An @or@ token.
  | TcRepeat   -- ^ A @repeat@ token.
  | TcReturn   -- ^ A @return@ token.
  | TcThen     -- ^ A @then@ token.
  | TcTrue     -- ^ A @true@ token.
  | TcUntil    -- ^ A @until@ token.
  | TcWhile    -- ^ A @while@ token.

  | TcColon -- ^ A @:@ token.
  | TcComma -- ^ A @,@ token.
  | TcDot -- ^ A @.@ token.
  | TcDots -- ^ A @...@ token.
  | TcEquals -- ^ A @=@ token.
  | TcSemicolon -- ^ A @;@ token.

  | TcOParen -- ^ A @(@ token.
  | TcCParen -- ^ A @)@ token.
  | TcOBrace -- ^ A @{@ token.
  | TcCBrace -- ^ A @}@ token.
  | TcOSquare -- ^ A @[@ token.
  | TcCSquare -- ^ A @]@ token.

  | TcAdd | TcSub | TcMul | TcDiv | TcPow | TcMod
  | TcConcat
  | TCEq | TCNe | TcLt | TcGt | TcLe | TcGe
  | TcLen

  | TcIdentifier Text -- ^ Identifiers (@foo@)

  | TcQuoteE Text -- ^ Quoted expressions (@%foo@)
  | TcQuoteS Text -- ^ Quoted statements (@\@foo()@)
  | TcQuoteV Text -- ^ Quoted identifiers (@$foo@)

  | TcInteger Int -- ^ Integer literal
  | TcFloat Double -- ^ Floating-point literal
  | TcString Text -- ^ String literal

  | TcWhitespace Text -- ^ One of more whitespace characters, only appearing in lexing streams
  | TcComment Text    -- ^ The body of a comment, including the `(*` and `*)`.

  | TcEOF -- ^ End of file
  deriving (Eq, Ord)

instance Show TokenClass where
  show TcAnd = "and"
  show TcBreak = "break"
  show TcDo = "do"
  show TcElse = "else"
  show TcElseIf = "elseif"
  show TcEnd = "end"
  show TcFalse = "false"
  show TcFor = "for"
  show TcFunction = "function"
  show TcIf = "if"
  show TcIn = "in"
  show TcLocal = "local"
  show TcNil = "nil"
  show TcNot = "not"
  show TcOr = "or"
  show TcRepeat = "repeat"
  show TcReturn = "return"
  show TcThen = "then"
  show TcTrue = "true"
  show TcUntil = "until"
  show TcWhile = "while"

  show TcColon = ":"
  show TcComma = ","
  show TcDot = "."
  show TcDots = "..."
  show TcEquals = "="
  show TcSemicolon = ";"

  show TcOParen = "("
  show TcCParen = ")"
  show TcOBrace = "{"
  show TcCBrace = "}"
  show TcOSquare = "["
  show TcCSquare = "]"

  show TcAdd = "+"
  show TcSub = "-"
  show TcMul = "*"
  show TcDiv = "/"
  show TcPow = "^"
  show TcMod = "%"

  show TcConcat = ".."

  show TCEq = "=="
  show TCNe = "~="
  show TcLt = "<"
  show TcGt = ">"
  show TcLe = "<="
  show TcGe = ">="

  show TcLen = "#"

  show (TcIdentifier t) = unpack t

  show (TcQuoteE t) = '%':unpack t
  show (TcQuoteS t) = '@':unpack t
  show (TcQuoteV t) = '$':unpack t

  show (TcString t) = show (unpack t)
  show (TcInteger i) = show i
  show (TcFloat i) = show i

  show (TcWhitespace t) = concatMap escape (unpack t) where
    escape '\t' = "\\t"
    escape '\n' = "\\n"
    escape c    = [c]
  show (TcComment t) = unpack t

  show TcEOF = "<eof>"

-- | A token, with its class, start, and end position.
data Token = Token !TokenClass !SourcePos !SourcePos deriving Show

instance Spanned Token where
  annotation (Token _ s e) = mkSpanUnsafe s e
