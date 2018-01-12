{
{-# OPTIONS_GHC -Wwarn -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
module Parser.ALexer
  ( lexerScan
  ) where

import qualified Data.ByteString as Bs
import qualified Data.ByteString.Builder as B
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as Bsc
import Data.Span
import Data.Spanned
import Data.Text.Encoding

import Data.Char (chr, digitToInt)
import Data.Semigroup
import qualified Data.Text as T

import Text.Printf

import Parser.Token
import Parser.AWrapper
}

$digit = [0-9]       -- Digits
$hex   = [0-9A-Fa-f] -- Hexadecimal digits
$upper = [A-Z]       -- Uppercase characters
$lower = [a-z]       -- Lowercase characters

$ident = [$digit $upper $lower '_' '\''] -- Valid identifier characters

tokens :-
  <0> $white+;

  <0> "(*"       { beginComment }
  <comment> "(*" { beginComment }
  <comment> "*)" { endComment }
  <comment> .    ;

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
  <0> \_       { constTok TcUnderscore }

  <0> "let"    { constTok TcLet }
  <0> "fun"    { constTok TcFun }
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
  <0> ";" ";"  { constTok TcTopSep }
  <0> ";"      { constTok TcSemicolon }
  <0> "("      { constTok TcOParen }
  <0> ")"      { constTok TcCParen }
  <0> "{"      { constTok TcOBrace }
  <0> "}"      { constTok TcCBrace }
  <0> "["      { constTok TcOSquare }
  <0> "]"      { constTok TcCSquare }

  <0> $digit+                          { onString $ TcInteger . parseNum 10 }
  <0> $lower $ident*                   { lexTok TcIdentifier }
  <0> $upper $ident*                   { lexTok TcConIdent }
  <0> '_' $ident+                      { lexTok TcHole }
  <0> \' $lower $ident*                { lexTok TcTyVar }
  <0> \"                               { beginString }

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

  <string> \\ x $hex+ { onStringM $ appendChar . chr . parseNum 16 . Bsc.drop 2 }

  <string> [^\\\"] { onStringM append }

{
parseNum :: Num a => a -> ByteString -> a
parseNum radix = Bsc.foldl' (\accum digit -> accum * radix + fromIntegral (digitToInt digit)) 0

appendChar :: Char -> Action Token
appendChar c _ _ = do
  s <- getState
  setState $ s { stringBuffer = stringBuffer s <> B.charUtf8 c }
  lexerScan -- Don't emit a token, just continue

append :: ByteString -> Action Token
append c _ _ = do
  s <- getState
  setState $ s { stringBuffer = stringBuffer s <> B.lazyByteString c }
  lexerScan -- Don't emit a token, just continue

beginString, endString :: Action Token
beginString _ _ = do
  setStartCode string
  lexerScan
endString (LI p _ _) _ = do
  s <- getState
  setState $ s { stringBuffer = "" }
  setStartCode 0
  return . flip Token p .  TcString . decodeUtf8 . Bs.concat . L.toChunks . B.toLazyByteString . stringBuffer $ s

beginComment, endComment :: Action Token
beginComment _ _ = do
  s <- getState
  -- Increment the comment depth and set the mode to comments
  setState $ s { commentDepth = commentDepth s + 1 }
  setStartCode comment
  lexerScan
endComment _ _ = do
  s <- getState
  -- Decrement the comment depth and, if required, set the mode to normal
  setState $ s { commentDepth = commentDepth s - 1 }
  if commentDepth s == 1 then setStartCode 0 else pure ()
  lexerScan

constTok :: TokenClass -> Action Token
constTok t (LI p _ _) _ = return $! Token t p

onString :: (ByteString -> TokenClass) -> Action Token
onString f (LI p str _) len = return $! Token (f (L.take len str)) p

lexTok :: (T.Text -> TokenClass) -> Action Token
lexTok k (LI p str _) len = return (Token (k str') p) where
    str' = decodeUtf8 . Bs.concat . L.toChunks . L.take len $ str

onStringM :: (ByteString -> Action a) -> Action a
onStringM f p@(LI _ str _) len = (f (L.take len str)) p len

lexInput :: String -> ByteString -> ParseResult [Token]
lexInput fp text = runParser fp text (loop []) where
  loop buf = do
    tok <- lexerScan
    case tok of
      Token TcEOF _ -> return (reverse buf)
      tok -> loop $! (tok : buf)

lexerScan :: Parser Token
lexerScan = do
  inp <- getInput
  sc <- getStartCode
  case alexScan inp sc of
    AlexEOF -> do
      code <- getStartCode
      case code of
        0 -> Token TcEOF <$> getPos
        n | n == string -> fail "expected closing '\"', got end of input"
        n | n == comment -> fail "expected closing '*)', got end of input"
        _ -> fail "unexpected end of input"
    AlexError (LI p str _) ->
      let t = decodeUtf8 . Bs.concat . L.toChunks $ str
          ch = T.head t
      in failPos (printf "unexpected character '%c'" ch) p
    AlexSkip  inp' _ -> do
      setInput inp'
      lexerScan
    AlexToken inp' len action -> do
      setInput inp'
      action inp (fromIntegral len)
}
