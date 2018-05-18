module Test.Parser.Parser (tests) where

import Test.Tasty
import Test.Util

import qualified Data.Text.Lazy as L
import qualified Data.Text as T
import Data.Spanned

import Parser.Wrapper (runParser)
import Parser

import Syntax.Pretty()
import Text.Pretty.Semantic

result :: String -> T.Text -> String
result file contents =
  case runParser file (L.fromStrict contents) parseInput of
    (Just res, []) -> disp $ pretty res <##> empty
    (Just res, es) -> disp $ pretty res <##>
                       string "(*" <##> indent 2 (prettyErrs es) <##> string "*)" <##> mempty
    (Nothing, es) -> disp $ prettyErrs es <##> empty

  where prettyErrs = vsep . map (\e -> pretty (annotation e) <> colon <+> pretty e)
        disp = display . renderPretty 0.8 120


tests :: IO TestTree
tests = testGroup "Test.Parser.Parser" <$> goldenDir result "tests/parser/" ".ml"
