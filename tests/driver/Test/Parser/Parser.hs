module Test.Parser.Parser (tests) where

import Test.Util

import qualified Data.Text.Lazy as L
import qualified Data.Text as T

import Parser.Wrapper (runParser)
import Parser

import qualified Text.Pretty.Note as N
import Text.Pretty.Semantic

result :: String -> T.Text -> T.Text
result file contents =
  case runParser name (L.fromStrict contents) parseTops of
    (Just res, []) -> displayPlain $ Right <$> pretty res
    (Just res, es) -> displayPlain $ (Right <$> pretty res) <##>
                       string "(*" <##> indent 2 (prettyErrs es) <##> string "*)"
    (Nothing, es) -> displayPlain $ prettyErrs es

  where
    name = T.pack file
    prettyErrs = vsep . map (N.format (N.fileSpans [(name, contents)] N.defaultHighlight))

tests :: IO TestTree
tests = testGroup "Test.Parser.Parser" <$> goldenDir result "tests/parser/" ".ml"
