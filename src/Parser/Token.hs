module Parser.Token where

import Data.Text (unpack, Text)

data TokenClass
  = TcArrow -- ->
  | TcEqual -- =
  | TcForall -- forall
  | TcImplies -- =>
  | TcPipe -- |
  | TcStar -- *
  | TcTilde -- ~
  | TcUnderscore -- _

  | TcLet -- let
  | TcAnd -- and
  | TcFun -- fun
  | TcIf -- if
  | TcThen -- then
  | TcElse -- else
  | TcBegin -- begin
  | TcEnd -- end
  | TcIn -- in
  | TcExternal -- external
  | TcVal -- val
  | TcTrue -- true
  | TcFalse -- false
  | TcMatch -- match
  | TcFunction -- function
  | TcWith -- with
  | TcType -- type
  | TcOf -- of
  | TcModule -- module
  | TcOpen -- open
  | TcAs -- as

  | TcDot -- .
  | TcComma -- ,
  | TcColon -- :
  | TcSemicolon -- ;
  | TcTopSep -- ;;
  | TcOParen -- (
  | TcCParen -- )
  | TcOBrace -- {
  | TcCBrace -- }
  | TcOSquare -- [
  | TcCSquare -- ]

  | TcOp Text
  | TcIdentifier Text
  | TcOpIdent Text
  | TcConIdent Text
  | TcIdentifierQual [Text] Text
  | TcOpIdentQual [Text] Text
  | TcConIdentQual [Text] Text
  | TcDotQual [Text]
  | TcTyVar Text
  | TcAccess Text
  | TcHole Text
  | TcInteger Integer
  | TcFloat Double
  | TcString Text

  | TcEOF

instance Show TokenClass where
  show TcArrow = "->"
  show TcEqual = "="
  show TcForall = "forall"
  show TcImplies = "=>"
  show TcPipe = "|"
  show TcStar = "*"
  show TcTilde = "~"
  show TcUnderscore = "_"

  show TcLet = "let"
  show TcFun = "fun"
  show TcAnd = "and"
  show TcIf = "if"
  show TcThen = "then"
  show TcElse = "else"
  show TcBegin = "begin"
  show TcEnd = "end"
  show TcIn = "in"
  show TcExternal = "external"
  show TcVal = "val"
  show TcTrue = "true"
  show TcFalse = "false"
  show TcMatch = "match"
  show TcWith = "with"
  show TcFunction = "function"
  show TcType = "type"
  show TcOf = "of"
  show TcModule = "module"
  show TcOpen = "open"
  show TcAs = "as"

  show TcComma = ","
  show TcDot = "."
  show TcColon = ":"
  show TcSemicolon = ";"
  show TcTopSep = ";;"
  show TcOParen = "("
  show TcCParen = ")"
  show TcOBrace = "{"
  show TcCBrace = "}"
  show TcOSquare = "["
  show TcCSquare = "]"

  show (TcOp t) = unpack t
  show (TcIdentifier t) = unpack t
  show (TcOpIdent t) = "`" ++ unpack t ++ "`"
  show (TcConIdent t) = unpack t
  show (TcIdentifierQual ms t) = concatMap (\m -> unpack m ++ ['.']) (reverse ms) ++ unpack t
  show (TcConIdentQual ms t) = concatMap (\m -> unpack m ++ ['.']) (reverse ms) ++ unpack t
  show (TcOpIdentQual ms t) = "`" ++ concatMap (\m -> unpack m ++ ['.']) (reverse ms) ++ unpack t ++ "`"
  show (TcDotQual ms) = concatMap (\m -> unpack m ++ ['.']) ms
  show (TcTyVar t) = '\'':unpack t
  show (TcAccess t) = '.':unpack t
  show (TcHole t) = unpack t
  show (TcString t) = show (unpack t)
  show (TcInteger i) = show i
  show (TcFloat i) = show i

  show TcEOF = "<eof>"
