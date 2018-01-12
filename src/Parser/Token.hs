module Parser.Token where

import Data.Text (unpack, Text)

data TokenClass
  = TcArrow -- ->
  | TcEqual -- =
  | TcForall -- forall
  | TcImplies -- =>
  | TcPipe -- |
  | TcDoubleStar -- **
  | TcStar -- *
  | TcAdd -- +
  | TcConcat -- ^
  | TcLt -- <
  | TcLte -- <=
  | TcGt -- >
  | TcGte -- >=
  | TcEqEq -- ==
  | TcAndAnd -- &&
  | TcOrOr -- ||
  | TcDivide -- /
  | TcSubtract -- -
  | TcNotEqual -- <>
  | TcTilde -- ~

  | TcLet -- let
  | TcAnd -- and
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
  | TcWith -- with
  | TcType -- type
  | TcUnit -- unit
  | TcOf -- of

  | TcComma -- ,
  | TcSemicolon -- ;
  | TcTopSep -- ;;
  | TcOParen -- (
  | TcCParen -- )
  | TcOBrace -- {
  | TcCBrace -- }
  | TcOSquare -- [
  | TcCSquare -- ]

  | TcIdentifier Text
  | TcInteger Integer
  | TcString Text

  | TcEOF

instance Show TokenClass where
  show TcArrow = "->"
  show TcEqual = "="
  show TcForall = "forall"
  show TcImplies = "=>"
  show TcPipe = "|"
  show TcDoubleStar = "**"
  show TcStar = "*"
  show TcAdd = "+"
  show TcConcat = "^"
  show TcLt = "<"
  show TcLte = "<="
  show TcGt = ">"
  show TcGte = ">="
  show TcEqEq = "=="
  show TcAndAnd = "&&"
  show TcOrOr = "||"
  show TcDivide = "/"
  show TcSubtract = "-"
  show TcNotEqual = "<>"
  show TcTilde = "~"

  show TcLet = "let"
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
  show TcType = "type"
  show TcUnit = "unit"
  show TcOf = "of"

  show TcComma = ","
  show TcSemicolon = ";"
  show TcTopSep = ";;"
  show TcOParen = "("
  show TcCParen = ")"
  show TcOBrace = "{"
  show TcCBrace = "}"
  show TcOSquare = "["
  show TcCSquare = "]"

  show (TcIdentifier t) = unpack t
  show (TcString t) = show (unpack t)
  show (TcInteger i) = show i

  show TcEOF = "<eof>"
