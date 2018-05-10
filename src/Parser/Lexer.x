{
{-# OPTIONS_GHC -Wwarn -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
module Parser.Lexer
  ( lexerScan, lexerContextScan
  ) where

import qualified Data.ByteString as Bs
import qualified Data.ByteString.Builder as B
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as Bsc
import qualified Data.Text.Read as R
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Char (chr, digitToInt)
import Data.Span
import Data.Semigroup
import Data.Position
import Data.Spanned
import Data.Span

import Parser.Error
import Parser.Token
import Parser.Context
import Parser.Wrapper

}

$digit = [0-9]       -- Digits
$hex   = [0-9A-Fa-f] -- Hexadecimal digits
$upper = [A-Z]       -- Uppercase characters
$lower = [a-z]       -- Lowercase characters

$ident = [$digit $upper $lower '_' '\''] -- Valid identifier characters

-- Valid symbol characters. We're slightly more fluent with what we allow
-- in non-head positions
$symbol_head = [\: \! \# \$ \% \& \* \+ \. \/ \< \= \> \? \@ \\ \^ \| \- \~]
$symbol_tail = [$symbol_head \[ \]]

tokens :-
  <0> $white+;

  <0> "(*"       { beginComment }
  <comment> "(*" { beginComment }
  <comment> "*)" { endComment }
  <comment> [.\n] ;

  <0> "->"     { constTok TcArrow }
  <0> "="      { constTok TcEqual }
  <0> "∀"      { constTok TcForall }
  <0> "forall" { constTok TcForall }
  <0> "=>"     { constTok TcImplies }
  <0> "|"      { constTok TcPipe }
  <0> "*"      { constTok TcStar }
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
  <0> "function" { constTok TcFunction }
  <0> "type"   { constTok TcType }
  <0> "of"     { constTok TcOf }
  <0> "module" { constTok TcModule }
  <0> "open"   { constTok TcOpen }

  <0> ","      { constTok TcComma }
  <0> "."      { constTok TcDot }
  <0> ":"      { constTok TcColon }
  <0> ";" ";"  { constTok TcTopSep }
  <0> ";"      { constTok TcSemicolon }
  <0> "("      { constTok TcOParen }
  <0> ")"      { constTok TcCParen }
  <0> "{"      { constTok TcOBrace }
  <0> "}"      { constTok TcCBrace }
  <0> "["      { constTok TcOSquare }
  <0> "]"      { constTok TcCSquare }

  <0> $digit+                          { onString $ TcInteger . parseNum 10 }
  <0> $digit+ \. $digit+               { onString $ TcFloat . parseDouble }
  <0> $digit+ \. $digit+ [Ee] $digit+  { onString $ TcFloat . parseDouble }
  <0> $digit+ \. $digit+ [Ee] [\+\-] $digit+ { onString $ TcFloat . parseDouble }

  <0> $upper $ident* \.                { beginModule }
  <0> $lower $ident*                   { lexTok $ TcIdentifier }
  <0> $upper $ident*                   { lexTok $ TcConIdent }
  <modP> $upper $ident* \.             { pushModule }
  <modP> $lower $ident*                { endModule TcIdentifierQual }
  <modP> $upper $ident*                { endModule TcConIdentQual }
  <modP> ()                            { endModuleNull TcDotQual }

  <0> $symbol_head $symbol_tail*       { lexTok $ TcOp }
  <0> \` $lower $ident* \`             { lexTok $ TcOpIdent . T.init . T.tail }
  <0> \` $upper $ident* \.             { beginModuleOp }
  <modOp> $upper $ident* \.            { pushModule }
  <modOp> $lower $ident* \`            { endModuleOp TcOpIdentQual }

  <0> \_ $ident+                       { lexTok TcHole }
  <0> \. $lower $ident*                { lexTok (TcAccess . T.tail) }
  <0> \' $lower $ident*                { lexTok (TcTyVar . T.tail) }

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

parseDouble :: ByteString -> Double
parseDouble t = case (R.double . decodeUtf8 . Bs.concat . L.toChunks) t of
                  Right (x, t) | T.null t -> x
                  _ -> error "Cannot parse float"

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
beginString (LI p _ _) _ = do
  mapState $ \s -> s { tokenStart = p }
  setStartCode string
  lexerScan
endString _ _ = do
  s <- getState
  setState $ s { stringBuffer = "" }
  setStartCode 0
  return . flip Token (tokenStart s) .  TcString . decodeUtf8 . Bs.concat . L.toChunks . B.toLazyByteString . stringBuffer $ s

beginComment, endComment :: Action Token
beginComment (LI p _ _) _ = do
  s <- getState
  -- Increment the comment depth and set the mode to comments
  setState $ s { commentDepth = commentDepth s + 1
               , tokenStart = if commentDepth s == 0 then p else tokenStart s }
  setStartCode comment
  lexerScan
endComment _ _ = do
  s <- getState
  -- Decrement the comment depth and, if required, set the mode to normal
  setState $ s { commentDepth = commentDepth s - 1 }
  if commentDepth s == 1 then setStartCode 0 else pure ()
  lexerScan

beginModule, beginModuleOp, pushModule :: Action Token
beginModule (LI p str _) len = do
  s <- getState
  setState $ s { modulePrefix = [ decodeUtf8 . Bs.concat . L.toChunks . L.take (pred len) $ str]
               , tokenStart = p }
  setStartCode modP
  lexerScan
beginModuleOp (LI p str _) len = do
  s <- getState
  setState $ s { modulePrefix = [ T.tail $ decodeUtf8 . Bs.concat . L.toChunks . L.take (pred len) $ str]
               , tokenStart = p }
  setStartCode modOp
  lexerScan
pushModule (LI _ str _) len = do
  s <- getState
  setState $ s { modulePrefix = (decodeUtf8 . Bs.concat . L.toChunks . L.take (pred len) $ str) : modulePrefix s }
  lexerScan

endModule, endModuleOp :: ([T.Text] -> T.Text -> TokenClass) -> Action Token
endModule t (LI _ str _) len = do
  s <- getState
  setState $ s { modulePrefix = [] }
  setStartCode 0
  return . flip Token (tokenStart s) .  t (modulePrefix s) . decodeUtf8 . Bs.concat . L.toChunks . L.take len $ str
endModuleOp f = endModule (\xs x -> f xs (T.init x))

endModuleNull :: ([T.Text] -> TokenClass) -> Action Token
endModuleNull t (LI _ _ _) _ = do
  s <- getState
  setState $ s { modulePrefix = [] }
  setStartCode 0
  return (Token (t (modulePrefix s)) (tokenStart s))

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

lexerContextScan :: Parser Token
lexerContextScan = do
  s <- getState
  go (pending s) (context s)
    where
      go :: PendingState -> [Context] -> Parser Token
      go Done cs  = do
        tok <- lexerScan
        uncurry go =<< (handleContext tok cs)
      go (Working tok) cs = uncurry go =<< (handleContext tok cs)
      go (Result tok toks) cs = do
        mapState (\s -> s { pending = toks, context = cs })
        pure tok

lexerScan :: Parser Token
lexerScan = do
  inp <- getInput
  sc <- getStartCode
  case alexScan inp sc of
    AlexEOF -> do
      start <- tokenStart <$> getState
      code <- getStartCode
      case code of
        0 -> Token TcEOF <$> getPos
        n | n == string -> failWith (UnclosedString (liPos inp) start)
        n | n == comment -> failWith (UnclosedComment (liPos inp) start)
        _ -> failWith (UnexpectedEnd (liPos inp))
    AlexError (LI p str _) ->
      let t = decodeUtf8 . Bs.concat . L.toChunks $ str
          ch = T.head t
      in failWith (UnexpectedCharacter p ch)
    AlexSkip  inp' _ -> do
      setInput inp'
      lexerScan
    AlexToken inp' len action -> do
      setInput inp'
      action inp (fromIntegral len)

}
