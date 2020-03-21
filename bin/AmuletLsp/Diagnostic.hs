{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, FlexibleContexts #-}

{-| Provides helper methods for working with diagnostics, and converting
  Amulet's error messages into diagnostics. -}
module AmuletLsp.Diagnostic (diagnosticOf) where

import Control.Lens hiding (List)

import Data.Spanned
import Data.Span

import Language.Haskell.LSP.Types.Lens
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
  diagnosticOf err = mkDiagnostic "amulet.parser" (spanOf err) (pretty err) err

instance DiagnosticLike R.ResolveError where
  diagnosticOf resErr = go resErr where
    mk msg = mkDiagnostic "amulet.resolve" (spanOf resErr) msg resErr

    go (R.ArisingFrom e _) = go e
    go e@(NonLinearPattern  _ ps) =
      let d = mk (pretty e)
          info p = DiagnosticRelatedInformation (locationOf (spanOf p)) "Variable declared here"
      in d { _relatedInformation = Just (List (map info ps)) }
    go e = mk (pretty e)

instance DiagnosticLike TypeError where
  diagnosticOf err = mkDiagnostic "amulet.tc" (spanOf err) (pretty err) err

instance DiagnosticLike VerifyError where
  diagnosticOf err = go err where
    mk msg = mkDiagnostic "amulet.verify" (spanOf err) msg err

    go e@DefinedUnused{} =
      let d = mk (pretty e)
      in d & tags ?~ List [ DtUnnecessary ]
    go e = mk (pretty e)

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
  , _tags = Nothing
  }

severityOf :: NoteKind -> DiagnosticSeverity
severityOf NoteMessage    = DsInfo
severityOf WarningMessage = DsWarning
severityOf ErrorMessage   = DsError
