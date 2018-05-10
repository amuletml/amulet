module Test.Parser.Parser (tests) where

import Test.Tasty
import Test.Util

import qualified Data.ByteString.Builder as B
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import Data.Spanned

import Parser.Wrapper (ParseResult(..), runParser)
import Parser

import Syntax.Pretty()
import Pretty

result :: String -> T.Text -> String
result file contents =
  case runParser file (B.toLazyByteString $ T.encodeUtf8Builder contents) parseInput of
    PFailed es -> show $ vsep (map (\e -> pretty (annotation e) <> colon <+> pretty e) es) <##> empty
    POK _ res -> display $ renderPretty 0.8 120 $ pretty res <##> empty

tests :: TestTree
tests = testGroup "Test.Parser.Parser" (map (goldenFile result "tests/parser/") files)

files :: [String]
files =
  [ "pass_records.ml"
  ]
