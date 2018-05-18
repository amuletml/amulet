module Test.Parser.Parser (tests) where

import Test.Tasty
import Test.Util

import qualified Data.Text.Lazy as L
import qualified Data.Text as T

import Parser.Wrapper (runParser)
import Parser

import Syntax.Pretty()

import qualified Text.Pretty.Note as N
import Text.Pretty.Semantic

result :: String -> T.Text -> T.Text
result file contents =
  case runParser file (L.fromStrict contents) parseInput of
    (Just res, []) -> displayPlain $ (Right <$> pretty res) <##> empty
    (Just res, es) -> displayPlain $ (Right <$> pretty res) <##>
                       string "(*" <##> indent 2 (prettyErrs es) <##> string "*)" <##> mempty
    (Nothing, es) -> displayPlain $ prettyErrs es <##> empty

  where prettyErrs = vsep . map (N.format (N.fileSpans [(file, contents)]))

tests :: IO TestTree
tests = testGroup "Test.Parser.Parser" <$> goldenDir result "tests/parser/" ".ml"
