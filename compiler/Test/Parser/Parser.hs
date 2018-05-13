module Test.Parser.Parser (tests) where

import Test.Tasty
import Test.Util

import qualified Data.Text.Lazy as L
import qualified Data.Text as T
import Data.Spanned

import Parser.Wrapper (ParseResult(..), runParser)
import Parser

import Syntax.Pretty()
import Pretty

result :: String -> T.Text -> String
result file contents =
  case runParser file (L.fromStrict contents) parseInput of
    PFailed es -> show $ vsep (map (\e -> pretty (annotation e) <> colon <+> pretty e) es) <##> empty
    POK _ res -> display $ renderPretty 0.8 120 $ pretty res <##> empty

tests :: IO TestTree
tests = testGroup "Test.Parser.Parser" <$> goldenDir result "tests/parser/" ".ml"
