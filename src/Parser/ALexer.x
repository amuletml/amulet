{
{-# OPTIONS_GHC -Wwarn -Wno-unused-imports #-}
module Parser.ALexer where

import Parser.Token
import qualified Data.Text as T
import Data.Char (chr)
import Debug.Trace
}

%wrapper "monadUserState"

$digit=0-9       -- Digits
$hex=[0-9A-Fa-f] -- Hexadecimal digits
$alpha=[a-zA-Z]  -- Alphabetic characters

tokens :-
  <0> $white+;

  <0> "->"     { constTok TcArrow }
  <0> "="      { constTok TcEqual }
  <0> "∀"      { constTok TcForall }
  <0> "forall" { constTok TcForall }
  <0> "=>"     { constTok TcImplies }
  <0> "|"      { constTok TcPipe }
  <0> "**"     { constTok TcDoubleStar }
  <0> "*"      { constTok TcStar }
  <0> "+"      { constTok TcAdd }
  <0> "^"      { constTok TcConcat }
  <0> "<"      { constTok TcLt }
  <0> "<="     { constTok TcLte }
  <0> ">"      { constTok TcGt }
  <0> ">="     { constTok TcGte }
  <0> "=="     { constTok TcEqEq }
  <0> "&&"     { constTok TcAndAnd }
  <0> "||"     { constTok TcOrOr }
  <0> "/"      { constTok TcDivide }
  <0> "-"      { constTok TcSubtract }
  <0> "<>"     { constTok TcNotEqual }
  <0> "~"      { constTok TcTilde }

  <0> "let"    { constTok TcLet }
  <0> "and"    { constTok TcAnd }
  <0> "if"     { constTok TcIf }
  <0> "then"   { constTok TcThen }
  <0> "else"   { constTok TcElse }
  <0> "begin"  { constTok TcBegin }
  <0> "end"    { constTok TcEnd }
  <0> "in"     { constTok TcIn }
  <0> "external" { constTok TcExternal }
  <0> "val"    { constTok TcVal }
  <0> "true"   { constTok TcTrue }
  <0> "false"  { constTok TcFalse }
  <0> "match"  { constTok TcMatch }
  <0> "with"   { constTok TcWith }
  <0> "type"   { constTok TcType }
  <0> "unit"   { constTok TcUnit }
  <0> "of"     { constTok TcOf }

  <0> ","      { constTok TcComma }
  <0> ";"      { constTok TcSemicolon }
  <0> "("      { constTok TcOParen }
  <0> ")"      { constTok TcCParen }
  <0> "{"      { constTok TcOBrace }
  <0> "}"      { constTok TcCBrace }
  <0> "["      { constTok TcOSquare }
  <0> "]"      { constTok TcCSquare }

  <0> $digit+                          { onString $ TcInteger . read }
  <0> $alpha [$alpha $digit '_' '\'']* { onString $ TcIdentifier . T.pack }
  <0> \"                               { begin string }

  <string> \" { endString }

  <string> \\ a { appendChar '\a' }
  <string> \\ b { appendChar '\b' }
  <string> \\ f { appendChar '\f' }
  <string> \\ n { appendChar '\n' }
  <string> \\ r { appendChar '\r' }
  <string> \\ v { appendChar '\v' }
  <string> \\ t { appendChar '\t' }

  <string> \\ \\ { appendChar '\\' }
  <string> \\ \" { appendChar '\"' }

  <string> \\ x $hex+ { onStringM $ appendChar . chr . read . ('0':) . tail }

  <string> [^\\\"] { onStringM $ appendChar . head }

{
data AlexUserState = AlexUserState { stringBuffer :: String }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState { stringBuffer = "" }

appendChar :: Char -> AlexAction TokenClass
appendChar c _ _ = do
  s <- alexGetUserState
  alexSetUserState $ s { stringBuffer = c:stringBuffer s }
  alexMonadScan -- Don't emit a token, just continue

endString :: AlexAction TokenClass
endString _ _ = do
  s <- alexGetUserState
  alexSetUserState $ s { stringBuffer = "" }
  return $ TcString (T.pack (reverse (stringBuffer s)))

constTok :: TokenClass -> AlexAction TokenClass
constTok t _ _ = return t

onString :: (String -> a) -> AlexAction a
onString f (_, _, _, str) len = return (f (take len str))

onStringM :: (String -> AlexAction a) -> AlexAction a
onStringM f p@(_, _, _, str) len = (f (take len str)) p len

alexEOF :: Alex TokenClass
alexEOF = return TcEOF

lex :: T.Text -> Either String [TokenClass]
lex text = runAlex (T.unpack text) (loop []) where
  loop buf = do
    tok <- alexMonadScan
    case tok of
      TcEOF -> return (reverse buf)
      tok -> loop $! (tok : buf)
}
