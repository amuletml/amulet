{-# LANGUAGE DuplicateRecordFields, OverloadedStrings #-}
module Test.Lsp.SymbolList (symbolListTests) where

import Data.Text ()

import Test.Tasty.Lsp
import Test.Tasty

symbolListTests :: TestTree
symbolListTests = testGroup "Symbol list"
  [ lspSession "Is provided" $ do
      ident <- openDoc "main.ml" "amulet"
      Left symbs <- getDocumentSymbols ident
      assertIn $ symbs @?= mainSymbols

  , lspSession "Is not provided on parse errors" $ do
      ident <- openDoc "main.ml" "amulet"
      changeDoc ident [ TextDocumentContentChangeEvent
                        (Just (range 0 0 0 3))
                        Nothing "lets" ]

      Left symbs <- getDocumentSymbols ident
      assertIn $ symbs @?= []

  , lspSession "Is provided on resolution errors" $ do
      ident <- openDoc "main.ml" "amulet"
      changeDoc ident [ TextDocumentContentChangeEvent
                        (Just (range 0 8 0 9))
                        Nothing "y" ]

      Left symbs <- getDocumentSymbols ident
      assertIn $ symbs @?= mainSymbols
  ]

  where
    mainSymbols =
      [ DocumentSymbol
        { _name = "x"
        , _detail = Nothing
        , _kind = SkVariable
        , _deprecated = Nothing
        , _range = range 0 4 0 9
        , _selectionRange = range 0 4 0 5
        , _children = Nothing
        }
      , DocumentSymbol
        { _name = "y"
        , _detail = Nothing
        , _kind = SkVariable
        , _deprecated = Nothing
        , _range = range 2 4 2 9
        , _selectionRange = range 2 4 2 5
        , _children = Nothing
        } ]
