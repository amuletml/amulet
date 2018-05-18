module Test.Parser.Lexer (tests) where

import Test.Tasty
import Test.Util

import qualified Data.Text.Lazy as L
import qualified Data.Text as T
import Data.Position
import Data.Spanned

import Parser.Wrapper (Token(..), runLexer)
import Parser.Lexer

import Text.Pretty.Semantic

result :: String -> T.Text -> String
result file contents =
  case runLexer file (L.fromStrict contents) lexerContextScan of
    (Just toks, []) -> disp $ writeToks 1 True toks
    (Just toks, es) -> disp $ writeToks 1 True toks <##>
                       string "(*" <##> indent 2 (prettyErrs es) <##> string "*)" <##> empty
    (Nothing, es) -> disp $ prettyErrs es <##> empty

  where writeToks _ _ [] = linebreak
        writeToks l f t@(Token tc p:ts)
          | spLine p > l = empty <##> writeToks (l + 1) True t
          | f
          = string (replicate (spCol p - 1) ' ' ++ show tc) <> writeToks l False ts
          | otherwise
          = space <> string (show tc) <> writeToks l False ts

        prettyErrs = vsep . map (\e -> pretty (annotation e) <> colon <+> pretty e)
        disp = display . renderPretty 0.8 120

tests :: IO TestTree
tests = testGroup "Test.Parser.Lexer" <$> goldenDir result "tests/lexer/" ".ml"
