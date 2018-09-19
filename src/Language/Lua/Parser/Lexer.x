{
{-# OPTIONS_GHC -Wwarn -Wno-unused-imports -Wno-monomorphism-restriction #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Lua.Parser.Lexer (lexerScan) where

import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy as L
import qualified Data.Text.Read as R
import qualified Data.Text as T

import Data.Char (chr, digitToInt)
import Data.Span
import Data.Semigroup
import Data.Position
import Data.Spanned
import Data.Span

import Language.Lua.Parser.Error
import Language.Lua.Parser.Token
import Language.Lua.Parser.Wrapper
}

%encoding "latin1"

$digit = [0-9]               -- Digits
$hex   = [0-9A-Fa-f]         -- Hexadecimal digits
$upper = [A-Z] -- Uppercase characters
$lower = [a-z] -- Lowercase characters

$identHead = [$upper $lower '_'] -- Valid starting identifier characters
$identTail = [$identHead $digit] -- Valid remaining identifier characters

tokens :-
  <0> $white+    { trivialTok TcWhitespace }
  <0> "--" .*    { trivialTok TcComment }

  -- Keywords
  <0> "and"      { constTok TcAnd }
  <0> "break"    { constTok TcBreak }
  <0> "do"       { constTok TcDo }
  <0> "else"     { constTok TcElse }
  <0> "elseif"   { constTok TcElseIf }
  <0> "end"      { constTok TcEnd }
  <0> "false"    { constTok TcFalse }
  <0> "for"      { constTok TcFor }
  <0> "function" { constTok TcFunction }
  <0> "if"       { constTok TcIf }
  <0> "in"       { constTok TcIn }
  <0> "local"    { constTok TcLocal }
  <0> "nil"      { constTok TcNil }
  <0> "not"      { constTok TcNot }
  <0> "or"       { constTok TcOr }
  <0> "repeat"   { constTok TcRepeat }
  <0> "return"   { constTok TcReturn }
  <0> "then"     { constTok TcThen }
  <0> "true"     { constTok TcTrue }
  <0> "until"    { constTok TcUntil }
  <0> "while"    { constTok TcWhile }

  -- Symbols
  <0> ":"        { constTok TcColon }
  <0> ","        { constTok TcComma }
  <0> "."        { constTok TcDot }
  <0> "..."      { constTok TcDots }
  <0> "="        { constTok TcEquals }
  <0> ";"        { constTok TcSemicolon }

  <0> "("        { constTok TcOParen }
  <0> ")"        { constTok TcCParen }
  <0> "{"        { constTok TcOBrace }
  <0> "}"        { constTok TcCBrace }
  <0> "["        { constTok TcOSquare }
  <0> "]"        { constTok TcCSquare }

  -- Operators
  <0> "+"        { constTok TcAdd }
  <0> "-"        { constTok TcSub }
  <0> "*"        { constTok TcMul }
  <0> "/"        { constTok TcDiv }
  <0> "^"        { constTok TcPow }
  <0> "%"        { constTok TcMod }

  <0> ".."       { constTok TcConcat }

  <0> "=="       { constTok TCEq }
  <0> "~="       { constTok TCNe }
  <0> "<"        { constTok TcLt }
  <0> ">"        { constTok TcGt }
  <0> "<="       { constTok TcLe }
  <0> ">="       { constTok TcGe }

  <0> "#"        { constTok TcLen }

  -- Numbers
  <0> $digit+                          { onString $ TcInteger . parseNum 10 }
  <0> 0 x $hex+                        { onString $ TcInteger . parseNum 16 . L.drop 2 }

  <0> $digit+ \. $digit+               { onString $ TcFloat . parseDouble }
  <0> $digit+ \. $digit+ [Ee] $digit+  { onString $ TcFloat . parseDouble }
  <0> $digit+ \. $digit+ [Ee] [\+\-] $digit+ { onString $ TcFloat . parseDouble }

  -- Identifiers
  <0> $identHead $identTail*           { lexTok $ TcIdentifier }
  <0> "%" $identHead $identTail*       { lexTok $ TcQuoteE . T.tail }
  <0> "@" $identHead $identTail*       { lexTok $ TcQuoteS . T.tail }
  <0> "$" $identHead $identTail*       { lexTok $ TcQuoteV . T.tail }

  -- Strings
  <0> \"                               { beginString '\"' }
  <0> \'                               { beginString '\'' }

  <string> \"                          { endString '\"' }
  <string> \'                          { endString '\'' }

  <string> \\ a                        { appendChar '\a' }
  <string> \\ b                        { appendChar '\b' }
  <string> \\ f                        { appendChar '\f' }
  <string> \\ n                        { appendChar '\n' }
  <string> \\ r                        { appendChar '\r' }
  <string> \\ v                        { appendChar '\v' }
  <string> \\ t                        { appendChar '\t' }

  <string> \\ \\                       { appendChar '\\' }
  <string> \\ \"                       { appendChar '\"' }
  <string> \\ \'                       { appendChar '\'' }

  <string> \\ x $hex+                  { onStringM $ appendChar . chr . parseNum 16 . L.drop 2 }

  <string> [^\\\"\']+                  { onStringM append }

{

fToken :: SourcePos -> SourcePos -> TokenClass -> Token
fToken s e t = Token t s e

parseNum :: Num a => a -> L.Text -> a
parseNum radix = L.foldl' (\accum digit -> accum * radix + fromIntegral (digitToInt digit)) 0

parseDouble :: L.Text -> Double
parseDouble t = case (R.double . L.toStrict) t of
                  Right (x, t) | T.null t -> x
                  _ -> error "Cannot parse float"

appendChar :: Char -> Action Token
appendChar c _ _ _ = do
  s <- getState
  setState $ s { stringBuffer = stringBuffer s <> B.singleton c }
  lexerScan -- Don't emit a token, just continue

append :: L.Text -> Action Token
append c _ _ _ = do
  s <- getState
  setState $ s { stringBuffer = stringBuffer s <> B.fromLazyText c }
  lexerScan -- Don't emit a token, just continue

beginString, endString :: Char -> Action Token
beginString c (LI p _ _ _) _ _ = do
  mapState $ \s -> s { tokenStart = p, stringChar = c }
  setStartCode string
  lexerScan
endString c _ _ ep = do
  s <- getState
  if stringChar s == c
  then do
    -- Characters match, terminate
    setState $ s { stringBuffer = "" }
    setStartCode 0
    pure . fToken (tokenStart s) ep .  TcString . L.toStrict . B.toLazyText . stringBuffer $ s
  else do
    -- Don't emit a token, just continue
    setState $ s { stringBuffer = stringBuffer s <> B.singleton c }
    lexerScan

trivialTok :: (T.Text -> TokenClass) -> Action Token
trivialTok f (LI sp str _ _) len ep = do
  s <- getState
  if trivials s
  then pure $! Token (f (L.toStrict (L.take len str))) sp ep
  else lexerScan

constTok :: TokenClass -> Action Token
constTok t (LI sp _ _ _) _ ep = pure $! Token t sp ep

onString :: (L.Text -> TokenClass) -> Action Token
onString f (LI sp str _ _) len ep = pure $! Token (f (L.take len str)) sp ep

lexTok :: (T.Text -> TokenClass) -> Action Token
lexTok k (LI sp str _ _) len ep = pure (Token (k str') sp ep) where
    str' = L.toStrict . L.take len $ str

onStringM :: (L.Text -> Action a) -> Action a
onStringM f p@(LI _ str _ _) len = (f (L.take len str)) p len

-- | Consume a token from the input text without adding virtual
-- "context" tokens.
lexerScan :: Parser Token
lexerScan = do
  inp <- getInput
  sc <- getStartCode
  case alexScan inp sc of
    AlexEOF -> do
      start <- tokenStart <$> getState
      code <- getStartCode
      case code of
        0 -> (\p -> Token TcEOF p p) <$> getPos
        n | n == string -> failWith (UnclosedString (liPos inp) start)
        _ -> failWith (UnexpectedEnd (liPos inp))
    AlexError (LI p str _ _) ->
      let ch = L.head str
      in failWith (UnexpectedCharacter p ch)
    AlexSkip  inp' _ -> do
      setInput inp'
      lexerScan
    AlexToken inp' _ action -> do
      setInput inp'
      -- Determine when this token ends. For the sake of the argument, we
      -- assume all tokens exist on a single line.
      let ep = case liPos inp' of
                 SourcePos { spCol = 1 } -> liPos inp
                 SourcePos f l c -> SourcePos f l (c - 1)
      action inp (liIdx inp' - liIdx inp) ep

}
