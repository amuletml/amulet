module Test.Parser.Lexer (tests) where

import Test.Tasty
import Test.Util

import qualified Data.Text.Lazy as L
import qualified Data.Text as T
import Data.Position
import Data.Spanned

import Parser.Wrapper (ParseResult(..), Token(..), runLexer)
import Parser.Lexer

import Pretty

result :: String -> T.Text -> String
result file contents =
  case runLexer file (L.fromStrict contents) lexerContextScan of
    PFailed es -> show $ vsep (map (\e -> pretty (annotation e) <> colon <+> pretty e) es) <##> empty
    POK _ toks -> tail $ writeToks 0 True toks

  where writeToks _ _ [] = "\n"
        writeToks l f t@(Token tc p:ts)
          | spLine p > l = '\n' : writeToks (l + 1) True t
          | f
          = replicate (spCol p - 1) ' ' ++ show tc ++ writeToks l False ts
          | otherwise
          = " " ++ show tc ++ writeToks l False ts

tests :: IO TestTree
tests = testGroup "Test.Parser.Lexer" <$> goldenDir result "tests/lexer/" ".ml"
