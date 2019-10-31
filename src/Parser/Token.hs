-- | Handles tokens and token ranges within a lexer stream.
--
-- Tokens fall into one of four categories (or classes).
--
--  * Standard tokens, which represent a symbol or keyword.
--  * Those with additional data, such as 'TcOp'.
--  * Virtual tokens (those emitted by the "Parser.Context"), such as
--    'TcVBegin'. These always start with @TCV@.
--  * Trivial tokens (those normally treated as whitespace), such as
--    'TcWhitespace'.
module Parser.Token where

import Data.Text (unpack, Text)
import Data.Position
import Data.Spanned
import Data.Span

-- | The raw classification of a token without additional metadata. This
-- is the underlying representation of what the lexer produces and the
-- parser consumes.
data TokenClass
  = TcArrow      -- ^ A @->@ token.
  | TcGenerator  -- ^ A @<-@ token.
  | TcEqual      -- ^ A @=@ token.
  | TcImplies    -- ^ A @=>@ token.
  | TcPipe       -- ^ A @|@ token.
  | TcStar       -- ^ A @*@ token.
  | TcTilde      -- ^ A @~@ token.
  | TcUnderscore -- ^ A @_@ token.

  | TcAnd      -- ^ The @and@ keyword.
  | TcAs       -- ^ The @as@ keyword.
  | TcBegin    -- ^ The @begin@ keyword.
  | TcClass    -- ^ The @class@ keyword.
  | TcDeriving -- ^ The @deriving@ keyword.
  | TcElse     -- ^ The @else@ keyword.
  | TcEnd      -- ^ The @end@ keyword.
  | TcExternal -- ^ The @external@ keyword.
  | TcFalse    -- ^ The @false@ keyword.
  | TcForall   -- ^ The @forall@ keyword.
  | TcFun      -- ^ The @fun@ keyword.
  | TcFunction -- ^ The @function@ keyword.
  | TcIf       -- ^ The @if@ keyword.
  | TcImport   -- ^ The @import@ keyword.
  | TcIn       -- ^ The @in@ keyword.
  | TcInclude  -- ^ The @include@ keyword.
  | TcInstance -- ^ The @instance@ keyword.
  | TcLazy     -- ^ The @lazy@ keyword.
  | TcLet      -- ^ The @let@ keyword.
  | TcMatch    -- ^ The @match@ keyword.
  | TcModule   -- ^ The @module@ keyword.
  | TcOf       -- ^ The @of@ keyword.
  | TcOpen     -- ^ The @open@ keyword.
  | TcPrivate  -- ^ The @private@ keyword.
  | TcRec      -- ^ The @rec@ keyword.
  | TcThen     -- ^ The @then@ keyword.
  | TcTrue     -- ^ The @true@ keyword.
  | TcType     -- ^ The @type@ keyword.
  | TcVal      -- ^ The @val@ keyword.
  | TcWhen     -- ^ The @when@ keyword.
  | TcWith     -- ^ The @with@ keyword.

  | TcDot       -- ^ A @.@ token.
  | TcComma     -- ^ A @,@ token.
  | TcColon     -- ^ A @:@ token.
  | TcBang      -- ^ A @!@ token.
  | TcSemicolon -- ^ A @;@ token.
  | TcTopSep    -- ^ A @;;@ token.
  | TcOBanana   -- ^ A @(|@ token.
  | TcCBanana   -- ^ A @|)@ token.
  | TcOParen    -- ^ A @(@ token.
  | TcCParen    -- ^ A @)@ token.
  | TcAt        -- ^ A @@{@ token.
  | TcOBrace    -- ^ A @{@ token.
  | TcCBrace    -- ^ A @}@ token.
  | TcOSquare   -- ^ A @[@ token.
  | TcCSquare   -- ^ A @]@ token.

  | TcOp Text                    -- ^ Operators (@+@)
  | TcIdentifier Text            -- ^ Identifiers (@foo@)
  | TcOpIdent Text               -- ^ Backtick ops (@`foo`@)
  | TcConIdent Text              -- ^ Constructors (@Foo@)
  | TcIdentifierQual [Text] Text -- ^ Qualified identifiers (@Foo.bar@)
  | TcOpIdentQual [Text] Text    -- ^ Qualified backtick ops (@`Foo.bar`@)
  | TcConIdentQual [Text] Text   -- ^ Qualified constructors (@Foo.Bar@)
  | TcDotQual [Text]             -- ^ Qualified module, used for "let open ..." (@Foo.@)
  | TcTyvar Text                 -- ^ Type variable (@'foo@)
  | TcAccess Text                -- ^ Record access (@.foo@)
  | TcDotOp Text                 -- ^ Dot operators (@.+@)
  | TcHole Text                  -- ^ Hole (@_foo@)

  | TcInteger Integer -- ^ Integer literal
  | TcFloat Double    -- ^ Floating-point literal
  | TcString Text     -- ^ String literal

  -- "Virtual" tokens. It might be possible merge "end" and "in", but
  -- this allows for easier inspection
  | TcVBegin -- ^ Virtual @begin@ token, @$begin@.
  | TcVEnd   -- ^ Virtual @end@ token, @$end@.
  | TcVSep   -- ^ Virtual @;;@ token, @$sep@.
  | TcVIn    -- ^ Virtual @in@ token, @$in@.

  | TcWhitespace Text -- ^ One of more whitespace characters, only appearing in lexing streams
  | TcComment Text    -- ^ The body of a comment, including the `(*` and `*)`.

  | TcEOF -- ^ End of file
  deriving (Eq, Ord)

instance Show TokenClass where
  show TcArrow = "->"
  show TcGenerator = "<-"
  show TcEqual = "="
  show TcImplies = "=>"
  show TcPipe = "|"
  show TcStar = "*"
  show TcTilde = "~"
  show TcUnderscore = "_"

  show TcAnd = "and"
  show TcAs = "as"
  show TcBegin = "begin"
  show TcClass = "class"
  show TcDeriving = "deriving"
  show TcElse = "else"
  show TcEnd = "end"
  show TcExternal = "external"
  show TcFalse = "false"
  show TcForall = "forall"
  show TcFun = "fun"
  show TcFunction = "function"
  show TcIf = "if"
  show TcImport = "import"
  show TcIn = "in"
  show TcInclude = "include"
  show TcInstance = "instance"
  show TcLazy = "lazy"
  show TcLet = "let"
  show TcMatch = "match"
  show TcModule = "module"
  show TcOf = "of"
  show TcOpen = "open"
  show TcPrivate = "private"
  show TcRec = "rec"
  show TcThen = "then"
  show TcTrue = "true"
  show TcType = "type"
  show TcVal = "val"
  show TcWhen = "when"
  show TcWith = "with"

  show TcComma = ","
  show TcDot = "."
  show TcColon = ":"
  show TcBang = "!"
  show TcSemicolon = ";"
  show TcTopSep = ";;"
  show TcOParen = "("
  show TcCParen = ")"
  show TcAt = "@"
  show TcOBrace = "{"
  show TcCBrace = "}"
  show TcOSquare = "["
  show TcCSquare = "]"
  show TcOBanana = "(|"
  show TcCBanana = "|)"

  show (TcOp t) = unpack t
  show (TcIdentifier t) = unpack t
  show (TcOpIdent t) = "`" ++ unpack t ++ "`"
  show (TcConIdent t) = unpack t
  show (TcIdentifierQual ms t) = concatMap (\m -> unpack m ++ ['.']) (reverse ms) ++ unpack t
  show (TcConIdentQual ms t) = concatMap (\m -> unpack m ++ ['.']) (reverse ms) ++ unpack t
  show (TcOpIdentQual ms t) = "`" ++ concatMap (\m -> unpack m ++ ['.']) (reverse ms) ++ unpack t ++ "`"
  show (TcDotQual ms) = concatMap (\m -> unpack m ++ ['.']) ms
  show (TcDotOp m) = unpack m
  show (TcTyvar t) = '\'':unpack t
  show (TcAccess t) = '.':unpack t
  show (TcHole t) = '_':unpack t
  show (TcString t) = show (unpack t)
  show (TcInteger i) = show i
  show (TcFloat i) = show i

  show TcVBegin = "$begin"
  show TcVEnd = "$end"
  show TcVSep = "$sep"
  show TcVIn = "$in"

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

-- | Determine the friendly name of a virtual token
friendlyName :: TokenClass -> String
friendlyName TcVEnd = "end of block"
friendlyName TcVSep = "end of block"
friendlyName TcVIn = "end of block"
friendlyName TcVBegin = "start of block"
friendlyName TcWhitespace{} = "whitespace"
friendlyName TcComment{} = "comment"
friendlyName x = show x
