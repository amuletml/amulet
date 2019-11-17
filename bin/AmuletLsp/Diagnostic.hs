{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, FlexibleContexts #-}

{-| Provides helper methods for working with diagnostics, and converting
  Amulet's error messages into diagnostics. -}
module AmuletLsp.Diagnostic (diagnosticOf) where

import Data.Spanned
import Data.Span

import Language.Haskell.LSP.Types

import Control.Monad.Infer as T (TypeError(..))
import Syntax.Resolve.Error as R
import Syntax.Verify.Error
import Parser.Error

import AmuletLsp.Features

import Text.Pretty.Semantic hiding (line)
import Text.Pretty.Note

-- | Some type which can be converted to a diagnostic.
class DiagnosticLike a where
  diagnosticOf :: a -> Diagnostic

instance DiagnosticLike ParseError where
  diagnosticOf err = mkDiagnostic "amulet.parser" (annotation err) (pretty err) err

instance DiagnosticLike R.ResolveError where
  diagnosticOf resErr = go resErr where
    mk msg = mkDiagnostic "amulet.resolve" (annotation resErr) msg resErr

    go (R.ArisingFrom e _) = go e
    go e@(NonLinearPattern  _ ps) =
      let d = mk (pretty e)
          info p = DiagnosticRelatedInformation (locationOf (annotation p)) "Variable declared here"
      in d { _relatedInformation = Just (List (map info ps)) }
    go e = mk (pretty e)

instance DiagnosticLike TypeError where
  diagnosticOf err = mkDiagnostic "amulet.tc" (annotation err) (pretty err) err

instance DiagnosticLike VerifyError where
  diagnosticOf err = mkDiagnostic "amulet.resolve" (annotation err) (pretty err) err

-- | Construct a diagnostic of some error.
mkDiagnostic :: Note a b
             => DiagnosticSource -> Span -> Doc -> a
             -> Diagnostic
mkDiagnostic source pos msg note =
  Diagnostic
  { _range = rangeOf pos
  , _severity = Just (severityOf (diagnosticKind note))
  , _code = NumberValue . fromIntegral <$> noteId note
  , _message = renderBasic msg
  , _relatedInformation = Nothing
  , _source = Just source
  }

severityOf :: NoteKind -> DiagnosticSeverity
severityOf NoteMessage    = DsInfo
severityOf WarningMessage = DsWarning
severityOf ErrorMessage   = DsError
