module Test.Lua.Parser (tests) where

import Test.Tasty
import Test.Util

import qualified Data.Text.Lazy as L
import qualified Data.Text as T

import Language.Lua.Parser

import qualified Text.Pretty.Note as N
import Text.Pretty.Semantic

result :: String -> T.Text -> T.Text
result file contents =
  case parseStmts (SourcePos file 1 1) (L.fromStrict contents) of
    Right res-> displayPlain $ Right <$> vsep (map pretty res)
    Left e -> displayPlain $ prettyErr e

  where prettyErr = N.format (N.fileSpans [(file, contents)] N.defaultHighlight)

tests :: IO TestTree
tests = testGroup "Test.Lua.Parser" <$> goldenDir result "tests/lua_parse/" ".lua"
