module Test.Lsp (tests) where

import Test.Tasty

import Test.Lsp.Diagnostics
import Test.Lsp.SymbolList
import Test.Lsp.TypeOverlay

tests :: IO TestTree
tests = pure $ testGroup "amulet-lsp"
  [ diagnosticsTests
  , symbolListTests
  , typeOverlayTests
  ]
