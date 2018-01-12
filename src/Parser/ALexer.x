{
{-# OPTIONS_GHC -Wwarn -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
module Parser.ALexer
  ( Token(..)
  , Alex
  , alexMonadScan', alexError
  , lexInput, runLexer
  ) where

import qualified Data.ByteString as Bs
import qualified Data.ByteString.Builder as B
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as Bsc
import Data.Span
import Data.Spanned
import Data.Text.Encoding

import Data.Char (chr, digitToInt)
import Data.Semigroup
import qualified Data.Text as T

import Text.Parsec.Pos
import Text.Printf

import Parser.Token
}

%wrapper "monadUserState-bytestring"

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
  <0> \' $lower $ident* { lexTok TcTyVar }
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

  <string> \\ x $hex+ { onStringM $ appendChar . chr . parseNum 16 . Bsc.drop 2 }

  <string> [^\\\"] { onStringM append }

{
data AlexUserState = AlexUserState { stringBuffer :: B.Builder
                                   , filePath :: String
                                   , commentDepth :: Int }

data Token = Token !TokenClass !SourcePos deriving Show

instance Spanned Token where
  annotation (Token _ s) = mkSpan1 s

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState { stringBuffer = mempty
                                  , filePath = mempty
                                  , commentDepth = 0 }

sourcePos :: AlexUserState -> AlexPosn -> SourcePos
sourcePos (AlexUserState { filePath = f }) (AlexPn _ l c) = newPos f l c

parseNum :: Num a => a -> ByteString -> a
parseNum radix = Bsc.foldl' (\accum digit -> accum * radix + fromIntegral (digitToInt digit)) 0

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
endString (p,_,_,_) _ = do
  s <- alexGetUserState
  alexSetUserState $ s { stringBuffer = "" }
  alexSetStartCode 0
  return . flip Token (sourcePos s p) .  TcString . decodeUtf8 . Bs.concat . ByteString.toChunks . B.toLazyByteString . stringBuffer $ s

beginComment, endComment :: AlexAction Token
beginComment _ _ = do
  s <- alexGetUserState
  -- Increment the comment depth and set the mode to comments
  alexSetUserState $ s { commentDepth = commentDepth s + 1 }
  alexSetStartCode comment
  alexMonadScan'
endComment _ _ = do
  s <- alexGetUserState
  -- Decrement the comment depth and, if required, set the mode to normal
  alexSetUserState $ s { commentDepth = commentDepth s - 1 }
  if (commentDepth s) == 1 then alexSetStartCode 0 else pure ()
  alexMonadScan'

constTok :: TokenClass -> AlexAction Token
constTok t (p,_,_,_) _ = do
  s <- alexGetUserState
  return $! Token t (sourcePos s p)

onString :: (ByteString -> TokenClass) -> AlexAction Token
onString f (p, _, str, _) len = do
  s <- alexGetUserState
  return $! Token (f (ByteString.take len str)) (sourcePos s p)

lexTok :: (T.Text -> TokenClass) -> AlexAction Token
lexTok k (p, _, str, _) len = do
  s <- alexGetUserState
  return (Token (k str') (sourcePos s p)) where
    str' = decodeUtf8 . Bs.concat . ByteString.toChunks . ByteString.take len $ str

onStringM :: (ByteString -> AlexAction a) -> AlexAction a
onStringM f p@(_, _, str, _) len = (f (ByteString.take len str)) p len

alexEOF :: Alex Token
alexEOF = do
  (p,_,_,_) <- alexGetInput
  s <- alexGetUserState
  return $! Token TcEOF (sourcePos s p)

lexInput :: String -> ByteString -> Either String [Token]
lexInput fp text = runLexer fp text (loop []) where
  loop buf = do
    tok <- alexMonadScan'
    case tok of
      Token TcEOF _ -> return (reverse buf)
      tok -> loop $! (tok : buf)

runLexer :: String -> ByteString -> Alex a -> Either String a
runLexer fp text m = runAlex text (setfp *> m) where
  setfp = do
    s <- alexGetUserState
    alexSetUserState $ s { filePath = fp }

alexGetUserState :: Alex AlexUserState
alexGetUserState = Alex (\s -> pure (s, alex_ust s))

alexSetUserState :: AlexUserState -> Alex ()
alexSetUserState us = Alex (\s -> pure (s { alex_ust = us }, ()))

alexCharError :: AlexPosn -> ByteString -> Alex a
alexCharError p bs =
  let t = decodeUtf8 . Bs.concat . ByteString.toChunks $ bs
      ch = T.head t
  in alexGenericError p (printf "unexpected character '%c'" ch)

alexGenericError :: AlexPosn -> String -> Alex a
alexGenericError (AlexPn _ l c) msg = do
  fp <- filePath <$> alexGetUserState
  alexError (printf "%s: lexical error at line %d, column %d: %s" fp l c msg)

alexMonadScan' :: Alex Token
alexMonadScan' = do
  inp <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp sc of
    AlexEOF -> do
      code <- alexGetStartCode
      (p, _, _, _) <- alexGetInput
      case code of
        0 -> alexEOF
        n | n == string -> alexGenericError p "expected closing '\"', got end of input"
        n | n == comment -> alexGenericError p "expected closing '*)', got end of input"
        _ -> alexGenericError p "unexpected end of input"
    AlexError (p, _, bs, _) ->
      alexCharError p bs
    AlexSkip  inp' _ -> do
      alexSetInput inp'
      alexMonadScan'
    AlexToken inp' len action -> do
      alexSetInput inp'
      action (ignorePendingBytes inp) (fromIntegral len)
}
