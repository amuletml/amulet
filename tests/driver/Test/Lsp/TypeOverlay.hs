{-# LANGUAGE DuplicateRecordFields, OverloadedStrings #-}
module Test.Lsp.TypeOverlay (typeOverlayTests) where

import Data.Text ()

import Test.Tasty.Lsp
import Test.Tasty

typeOverlayTests :: TestTree
typeOverlayTests = testGroup "Type overlay"
  [ lspSession "Is shown as a code lens" $ do
      ident <- openDoc "main.ml" "amulet"
      lenses <- getCodeLenses ident
      assertIn $ lenses @?= [ CodeLens
                              { _range = range 2 4  2 5
                              , _command = Just (Command "y : int" "" Nothing)
                              , _xdata = Nothing
                              }
                            , CodeLens
                              { _range = range 0 4 0 5
                              , _command = Just (Command "x : int" "" Nothing)
                              , _xdata = Nothing
                              } ]

  , lspSession "Is not shown on errors" $ do
      ident <- openDoc "main.ml" "amulet"
      changeDoc ident [ TextDocumentContentChangeEvent
                        (Just (range 0 8 0 9))
                        Nothing "y" ]
      lenses <- getCodeLenses ident
      assertIn $ lenses @?= []
  ]
