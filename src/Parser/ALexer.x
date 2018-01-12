{
{-# OPTIONS_GHC -Wwarn -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
module Parser.ALexer
  ( Token(..)
  , AlexPosn(..)
  , Alex(..)
  , alexMonadScan'
  , alexError'
  ) where

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString as Bs
import Data.ByteString.Lazy (ByteString)
import Data.Text.Encoding

import qualified Data.Text.Read as TR
import qualified Data.Text as T
import Data.Semigroup
import Data.Either
import Data.Char (chr)

import Text.Printf

import Parser.Token
}

%wrapper "monadUserState-bytestring"

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

  <0> $digit+                          { lexTok $ TcInteger . either undefined fst . TR.decimal }
  <0> $alpha [$alpha $digit '_' '\'']* { lexTok TcIdentifier }
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

  <string> \\ x $hex+ { onStringM $ undefined }

  <string> [^\\\"] { onStringM append }

{
data AlexUserState = AlexUserState { stringBuffer :: B.Builder, filePath :: String }

data Token = Token !TokenClass !AlexPosn deriving Show

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState { stringBuffer = mempty, filePath = mempty }

appendChar :: Char -> AlexAction Token
appendChar c _ _ = do
  s <- alexGetUserState
  alexSetUserState $ s { stringBuffer = stringBuffer s <> B.charUtf8 c }
  alexMonadScan' -- Don't emit a token, just continue

append :: ByteString -> AlexAction Token
append c _ _ = do
  s <- alexGetUserState
  alexSetUserState $ s { stringBuffer = stringBuffer s <> B.lazyByteString c }
  alexMonadScan' -- Don't emit a token, just continue

endString :: AlexAction Token
endString _ _ = do
  s <- alexGetUserState
  alexSetUserState $ s { stringBuffer = "" }
  (p, _, _, i) <- alexGetInput
  return . flip Token p .  TcString . decodeUtf8 . Bs.concat . ByteString.toChunks . B.toLazyByteString . stringBuffer $ s

constTok :: TokenClass -> AlexAction Token
constTok t _ _ = do
  (p,_,_,_) <- alexGetInput
  return $! Token t p

onString :: (ByteString -> a) -> AlexAction a
onString f (_, _, str, _) len = return (f (ByteString.take len str))

lexTok :: (T.Text -> TokenClass) -> AlexAction Token
lexTok k (p, _, str, _) len = return (Token (k str') p) where
  str' = decodeUtf8 . Bs.concat . ByteString.toChunks . ByteString.take len $ str

onStringM :: (ByteString -> AlexAction a) -> AlexAction a
onStringM f p@(_, _, str, _) len = (f (ByteString.take len str)) p len

alexEOF :: Alex Token
alexEOF = do
  (p,_,_,_) <- alexGetInput
  return $! Token TcEOF p

lex :: String -> ByteString -> Either String [Token]
lex fp text = runAlex text (setfp *> loop []) where
  setfp = do
    s <- alexGetUserState
    alexSetUserState $ s { filePath = fp }
  loop buf = do
    tok <- alexMonadScan'
    case tok of
      Token TcEOF _ -> return (reverse buf)
      tok -> loop $! (tok : buf)

alexGetUserState :: Alex AlexUserState
alexGetUserState = Alex (\s -> pure (s, alex_ust s))

alexSetUserState :: AlexUserState -> Alex ()
alexSetUserState us = Alex (\s -> pure (s { alex_ust = us }, ()))

alexError' :: AlexPosn -> ByteString -> Alex a
alexError' (AlexPn off l c) bs = do
  fp <- filePath <$> alexGetUserState
  let t = decodeUtf8 . Bs.concat . ByteString.toChunks $ bs
      ch = T.head t
  alexError (printf "%s: lexical error at line %d, column %d: unexpected character '%c'" fp l c ch)

alexMonadScan' :: Alex Token
alexMonadScan' = do
  inp <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp sc of
    AlexEOF -> alexEOF
    AlexError (p, c, bs, s) ->
      alexError' p bs
    AlexSkip  inp' len -> do
      alexSetInput inp'
      alexMonadScan'
    AlexToken inp' len action -> do
      alexSetInput inp'
      action (ignorePendingBytes inp) (fromIntegral len)
}
