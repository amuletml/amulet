{-# LANGUAGE TemplateHaskell #-}
module Test.Lua.Parser (tests) where

import Test.Lua.Gen
import Test.Tasty
import Test.Util

import qualified Data.Text.Lazy as L
import qualified Data.Text as T

import Language.Lua.Parser

import qualified Text.Pretty.Note as N
import Text.Pretty.Semantic

import Hedgehog

result :: String -> T.Text -> T.Text
result file contents =
  case parseStmts (SourcePos file 1 1) (L.fromStrict contents) of
    Right res-> displayPlain $ Right <$> vsep (map pretty res)
    Left e -> displayPlain $ prettyErr e

  where prettyErr = N.format (N.fileSpans [(file, contents)] N.defaultHighlight)

prop_roundtripStmts :: Property
prop_roundtripStmts = withTests 1000 . property $ do
  stmts <- forAllWith (show . pretty) genStmts
  tripping stmts (display . renderPretty 0.4 100 . pretty) (parseStmts (SourcePos "in" 1 1) . L.fromStrict)

prop_roundtripExpr :: Property
prop_roundtripExpr = withTests 1000 . property $ do
  stmts <- forAllWith (show . pretty) genExpr
  tripping stmts (display . renderPretty 0.4 100 . pretty) (parseExpr (SourcePos "in" 1 1) . L.fromStrict)


tests :: IO TestTree
tests = do
  golden <- goldenDir result "tests/lua_parse/" ".lua"
  pure $ testGroup "Test.Lua.Parser"
    [ testGroup "Golden" golden
    , hedgehog $ $$(discover)
    ]
