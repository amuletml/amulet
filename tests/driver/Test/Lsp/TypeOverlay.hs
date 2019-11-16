{-# LANGUAGE DuplicateRecordFields, OverloadedStrings #-}
module Test.Lsp.TypeOverlay (typeOverlayTests) where

import Data.Aeson.Types
import Data.Text ()

import Test.Tasty.Lsp
import Test.Tasty

typeOverlayTests :: TestTree
typeOverlayTests = testGroup "Type overlay"
  [ lspSession "Is shown as a code lens" $ do
      ident <- openDoc "main.ml" "amulet"
      lenses <- getCodeLenses ident
      assertIn $ lenses @?= [ CodeLens
                              { _range = range 2 4 2 5
                              , _command = Nothing
                              , _xdata = Just (object [("name", String "y"), ("id", Number 2), ("file", Number 0)])
                              }
                            , CodeLens
                              { _range = range 0 4 0 5
                              , _command = Nothing
                              , _xdata = Just (object [("name", String "x"), ("id", Number 1), ("file", Number 0)])
                              } ]

  , lspSession "Code lenses are resolved" $ do
      ident <- openDoc "main.ml" "amulet"
      _ <- getCodeLenses ident
      resolved <- resolveCodeLens CodeLens
                  { _range = range 0 4 0 5
                  , _command = Nothing
                  , _xdata = Just (object [("name", "x"), ("id", Number 1), ("file", Number 0)])
                  }
      assertIn $ resolved @?= [ CodeLens
                                { _range = range 0 4 0 5
                                , _command = Just (Command "x : int" "" Nothing)
                                , _xdata = Nothing
                                } ]
  , lspSession "Malformed code lenses resolve to nothing" $ do
      ident <- openDoc "main.ml" "amulet"
      _ <- getCodeLenses ident
      resolved <- resolveCodeLens CodeLens
                  { _range = range 0 4 0 5
                  , _command = Nothing
                  , _xdata = Just (object [("name", String "x"), ("id", Number 1), ("file", Number 5)])
                  }
      assertIn $ resolved @?= [ ]


  , lspSession "Is not shown on errors" $ do
      ident <- openDoc "main.ml" "amulet"
      changeDoc ident [ TextDocumentContentChangeEvent
                        (Just (range 0 8 0 9))
                        Nothing "y" ]
      lenses <- getCodeLenses ident
      assertIn $ lenses @?= []
  ]
