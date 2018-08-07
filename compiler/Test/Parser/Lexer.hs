module Test.Parser.Lexer (tests) where

import Test.Tasty
import Test.Util

import qualified Data.Text.Lazy as L
import qualified Data.Text as T
import Data.Position

import Parser.Wrapper
import Parser.Lexer

import qualified Text.Pretty.Note as N
import Text.Pretty.Semantic

result :: String -> T.Text -> T.Text
result file contents =
  case runLexer file (L.fromStrict contents) lexerContextScan of
    (Just toks, []) -> displayPlain $ writeToks 1 True toks
    (Just toks, es) -> displayPlain $ writeToks 1 True toks <##>
                       string "(*" <##> indent 2 (prettyErrs es) <##> string "*)"
    (Nothing, es) -> displayPlain $ prettyErrs es

  where writeToks _ _ [] = empty
        writeToks l f t@(Token tc p _:ts)
          | spLine p > l = empty <##> writeToks (l + 1) True t
          | f
          = string (replicate (spCol p - 1) ' ' ++ show tc) <> writeToks l False ts
          | otherwise
          = space <> string (show tc) <> writeToks l False ts

        prettyErrs = vsep . map (N.format (N.fileSpans [(file, contents)]))

resultTrivial :: String -> T.Text -> T.Text
resultTrivial file contents =
  let (Just toks, _) = runLexerTrivial file (L.fromStrict contents) lexerScan
  in T.pack (concatMap (\(Token tc f t) -> "\"" <> show tc <> "\" " <> sp f <> "-" <> sp t <> "\n") toks)
  where sp (SourcePos _ l c) = show l ++ ":" ++ show c

tests :: IO TestTree
tests = testGroup "Lexer" <$> sequenceA
  [ testGroup "Normal" <$> goldenDir result "tests/lexer/" ".ml"
  , testGroup "Trivials" <$> goldenDir resultTrivial "tests/lexer/trivial/" ".ml"
  ]
