{-# LANGUAGE DuplicateRecordFields, OverloadedStrings #-}
module Test.Lsp.Diagnostics (diagnosticsTests) where

import Data.Text ()

import Test.Tasty.Lsp
import Test.Tasty

diagnosticsTests :: TestTree
diagnosticsTests = testGroup "Diagnostics"
  [ lspSession "Well-formed files have no diagnostics" $ do
      _ <- openDoc "main.ml" "amulet"
      diags <- waitForDiagnostics
      assertIn $ diags @?= []

  , lspSession "Parse errors provide diagnostics" $ do
      ident <- openDoc "main.ml" "amulet"
      _ <- waitForDiagnostics
      changeDoc ident [ TextDocumentContentChangeEvent
                        (Just (range 0 0 0 3))
                        Nothing "lets" ]
      diags <- waitForDiagnostics
      assertIn $ diags @?= [ Diagnostic
                             { _range = range 0 0 0 4
                             , _severity = Just DsError
                             , _code = Nothing
                             , _source = Just "amc.parser"
                             , _message = "Unexpected lets, expected type"
                             , _relatedInformation = Nothing } ]

  , lspSession "Resolve errors provide diagnostics" $ do
      ident <- openDoc "main.ml" "amulet"
      _ <- waitForDiagnostics
      changeDoc ident [ TextDocumentContentChangeEvent
                        (Just (range 0 8 0 9))
                        Nothing "y" ]
      diags <- waitForDiagnostics
      assertIn $ diags @?= [ Diagnostic
                             { _range = range 0 8 0 9
                             , _severity = Just DsError
                             , _code = Just (NumberValue 1001)
                             , _source = Just "amc.resolve"
                             , _message = "Variable not in scope: `y`"
                             , _relatedInformation = Nothing } ]
  ]
