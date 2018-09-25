{
{-# OPTIONS_GHC -Wwarn -Wno-unused-imports -Wno-monomorphism-restriction #-}
{-# LANGUAGE OverloadedStrings #-}
module Parser.Lexer
  ( lexerScan, lexerContextScan
  ) where

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

import Parser.Error
import Parser.Token
import Parser.Context
import Parser.Wrapper

}

%encoding "latin1"

-- See Parser.Unicode
$unicodeUpper = \xf0
$unicodeLower = \xf1
$unicodeSymbol = \xf2
$unicodeGeneric = \xf3
$unicodeDigit = \xf4
$unicodeWhitespace = \xf5
$unicodeOtherGraphic = \xf6
$unicodeOther = \xf7

$digit = [0-9]               -- Digits
$hex   = [0-9A-Fa-f]         -- Hexadecimal digits
$upper = [$unicodeUpper A-Z] -- Uppercase characters
$lower = [$unicodeLower a-z] -- Lowercase characters

$ident = [$unicodeDigit $unicodeGeneric $digit $upper $lower '_' '\''] -- Valid identifier characters

-- Valid symbol characters. We're slightly more fluent with what we allow
-- in non-head positions
$symbolHead = [\: \! \# \$ \% \& \* \+ \. \/ \< \= \> \? \@ \\ \^ \| \- \~ $unicodeSymbol]
$symbolTail = [$symbolHead \[ \]]

tokens :-
  <0> $white+    { whitespace }

  -- Comments
  <0> "(*"           { beginComment }
  <comment> "(*"     { beginComment }
  <comment> "*)"     { endComment }
  -- We have a greedy "comment" body regex which consumes everything
  -- until a potential starting/ending character. This hopefully reduces
  -- the number of times we call the action.
  <comment> [^\*\(]+ { addComment }
  -- This rule is our fall back which will handle any remaining
  -- characters
  <comment> [.\n]    { addComment }

  -- Builtin keywords and symbols
  <0> "->"     { constTok TcArrow }
  <0> "="      { constTok TcEqual }
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
  <0> "lazy"   { constTok TcLazy }
  <0> "as"     { constTok TcAs }

  <0> ","      { constTok TcComma }
  <0> "."      { constTok TcDot }
  <0> ":"      { constTok TcColon }
  <0> ";" ";"  { constTok TcTopSep }
  <0> ";"      { constTok TcSemicolon }
  <0> "?("     { constTok TcQParen }
  <0> "("      { constTok TcOParen }
  <0> ")"      { constTok TcCParen }
  <0> "@{"     { constTok TcAtBrace }
  <0> "?"      { constTok TcQuestion }
  <0> "{"      { constTok TcOBrace }
  <0> "}"      { constTok TcCBrace }
  H0> "["      { constTok TcOSquare }
  <0> "]"      { constTok TcCSquare }

  -- Numbers
  <0> $digit+                          { onString $ TcInteger . parseNum 10 }
  <0> 0 x $hex+                        { onString $ TcInteger . parseNum 16 . L.drop 2 }
  <0> 0 b [01]+                        { onString $ TcInteger . parseNum 2 . L.drop 2 }

  <0> $digit+ \. $digit+               { onString $ TcFloat . parseDouble }
  <0> $digit+ \. $digit+ [Ee] $digit+  { onString $ TcFloat . parseDouble }
  <0> $digit+ \. $digit+ [Ee] [\+\-] $digit+ { onString $ TcFloat . parseDouble }

  -- Identifiers
  <0> $lower $ident*                   { lexTok $ TcIdentifier }
  <0> "?" $lower $ident*               { lexTok $ TcQIdentifier . T.tail }
  <0> $upper $ident*                   { lexTok $ TcConIdent }

  -- Module identifiers
  <0> $upper $ident* \.                { beginModule }
  <modP> $upper $ident* \.             { pushModule }
  <modP> $lower $ident*                { endModule TcIdentifierQual }
  <modP> $upper $ident*                { endModule TcConIdentQual }
  <modP> ()                            { endModuleNull TcDotQual }

  -- Operators
  <0> $symbolHead $symbolTail*         { lexOperator }
  <0> \` $lower $ident* \`             { lexTok $ TcOpIdent . T.init . T.tail }

  -- Module operators
  <0> \` $upper $ident* \.             { beginModuleOp }
  <modOp> $upper $ident* \.            { pushModule }
  <modOp> $lower $ident* \`            { endModuleOp TcOpIdentQual }

  -- Other operators
  <0> \_ $ident+                       { lexTok (TcHole . T.tail) }
  <0> \. $lower $ident*                { lexTok (TcAccess . T.tail) }
  <0> \' $lower $ident*                { lexTok (TcTyvar . T.tail) }

  -- Strings
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

  <string> \\ x $hex+ { onStringM $ appendChar . chr . parseNum 16 . L.drop 2 }
  <string> \\ [.\n]   { invalidEscape }

  <string> [^\\\"]    { onStringM append }

  <string> \n         { unclosedString }

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

invalidEscape :: Action Token
invalidEscape (LI sp _ _ _) _ ep = do
  tellErrors [InvalidEscapeCode (mkSpanUnsafe sp ep)]
  lexerScan

unclosedString :: Action Token
unclosedString _ _ ep = do
  s <- getState
  tellErrors [UnclosedString ep (tokenStart s) Newline]
  pure . fToken (tokenStart s) ep .  TcString . L.toStrict . B.toLazyText . stringBuffer $ s

beginString, endString :: Action Token
beginString (LI p _ _ _) _ _ = do
  mapState $ \s -> s { tokenStart = p }
  setStartCode string
  lexerScan
endString _ _ ep = do
  s <- getState
  setState $ s { stringBuffer = "" }
  setStartCode 0
  pure . fToken (tokenStart s) ep .  TcString . L.toStrict . B.toLazyText . stringBuffer $ s


whitespace :: Action Token
whitespace (LI sp str _ _) len ep = do
  s <- getState
  if trivials s
  then pure $! Token (TcWhitespace (L.toStrict (L.take len str))) sp ep
  else lexerScan

beginComment, endComment, addComment :: Action Token
beginComment (LI p str _ _) len _ = do
  s <- getState
  -- Increment the comment depth and set the mode to comments
  setState $ s { commentDepth = commentDepth s + 1
               , tokenStart = if commentDepth s == 0 then p else tokenStart s
               , stringBuffer = if trivials s
                                then stringBuffer s <> B.fromLazyText (L.take len str)
                                else mempty
               }
  setStartCode comment
  lexerScan
endComment (LI _ str _ _) len ep = do
  s <- getState

  if commentDepth s > 1
  then do
    -- Pop up a comment level
    setState $ s { commentDepth = commentDepth s - 1 }
    lexerScan
  else do
    -- Return to normal parsing and, if desired, emit a comment token
    setState $ s { commentDepth = 0, stringBuffer = mempty, sMode = 0 }
    if trivials s
    then pure $! Token (TcComment . L.toStrict . B.toLazyText $ stringBuffer s <> B.fromLazyText (L.take len str))
                       (tokenStart s) ep
    else lexerScan
addComment (LI _ str _ _) len _ = do
  s <- getState
  if trivials s
  then setState $ s { stringBuffer = stringBuffer s <> B.fromLazyText (L.take len str) }
  else pure ()
  lexerScan

beginModule, beginModuleOp, pushModule :: Action Token
beginModule (LI p str _ _) len _ = do
  s <- getState
  setState $ s { modulePrefix = [ L.toStrict . L.take (pred len) $ str]
               , tokenStart = p }
  setStartCode modP
  lexerScan
beginModuleOp (LI p str _ _) len _ = do
  s <- getState
  setState $ s { modulePrefix = [ L.toStrict . L.tail . L.take (pred len) $ str]
               , tokenStart = p }
  setStartCode modOp
  lexerScan
pushModule (LI _ str _ _) len _ = do
  s <- getState
  setState $ s { modulePrefix = (L.toStrict . L.take (pred len) $ str) : modulePrefix s }
  lexerScan

endModule, endModuleOp :: ([T.Text] -> T.Text -> TokenClass) -> Action Token
endModule t (LI _ str _ _) len ep = do
  s <- getState
  setState $ s { modulePrefix = [] }
  setStartCode 0
  pure . fToken (tokenStart s) ep .  t (modulePrefix s) . L.toStrict . L.take len $ str
endModuleOp f = endModule (\xs x -> f xs (T.init x))

endModuleNull :: ([T.Text] -> TokenClass) -> Action Token
endModuleNull t (LI _ _ _ _) _ ep = do
  s <- getState
  setState $ s { modulePrefix = [] }
  setStartCode 0
  pure (Token (t (modulePrefix s)) (tokenStart s) ep)

lexOperator :: Action Token
lexOperator (LI sp str _ _) len ep =
  let str' = L.toStrict . L.take len $ str
      -- We perform some special handling of a couple of fancy operators
      tok | str' == "∀" = TcForall
          | str' == "→" = TcArrow
          | otherwise = TcOp str'
  in pure (Token tok sp ep)

constTok :: TokenClass -> Action Token
constTok t (LI sp _ _ _) _ ep = pure $! Token t sp ep

onString :: (L.Text -> TokenClass) -> Action Token
onString f (LI sp str _ _) len ep = pure $! Token (f (L.take len str)) sp ep

lexTok :: (T.Text -> TokenClass) -> Action Token
lexTok k (LI sp str _ _) len ep = pure (Token (k str') sp ep) where
    str' = L.toStrict . L.take len $ str

onStringM :: (L.Text -> Action a) -> Action a
onStringM f p@(LI _ str _ _) len = (f (L.take len str)) p len

-- | Consume a virtual context token or a token from the current input
-- text.
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
        n | n == string -> failWith (UnclosedString (liPos inp) start Eof)
        n | n == comment -> failWith (UnclosedComment (liPos inp) start)
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
