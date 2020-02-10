{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, FlexibleContexts, TypeFamilies #-}
module AmuletLsp.Features where

import Control.Monad.Infer (TypeError(..))
import Control.Lens hiding (List)

import qualified Data.Text as T
import Data.Foldable
import Data.Position
import Data.Spanned
import Data.Maybe
import Data.Span

import Frontend.Errors

import Language.Haskell.LSP.Types.Lens hiding (error)
import Language.Haskell.LSP.Types

import Prelude hiding (id)

import Text.Pretty.Semantic hiding (line)

-- | Get all code actions within a range.
--
-- For now, this just provides a function to fill in any type hole.
getCodeActions :: VersionedTextDocumentIdentifier -> Range -> ErrorBundle -> [CAResult]
getCodeActions file filterRange = foldl' getAction [] . (^.typeErrors) where
  -- TODO: Investigate benefits/disadvantages of using a command instead of a
  -- raw "replace" action.

  getAction ac err
    | errPos <- rangeOf (annotation err)
    , filterRange ^. end >= errPos ^. start
    , filterRange ^. start <= errPos ^. end
    = getActionOf errPos ac err
    | otherwise = ac

  getActionOf range ac (ArisingFrom e _) = getActionOf range ac e
  getActionOf range ac (FoundHole _ _ exprs@(_:_)) = map (CACodeAction . mkAction range . pretty) exprs ++ ac
  getActionOf _ ac _ = ac

  mkAction range expr =
    let simple = renderBasic expr
    in CodeAction
    { _title = "Replace hole with '"
            <> (if T.length simple > 20 then T.take 17 simple <> "..." else simple)
            <> "'"
    , _kind = Just CodeActionQuickFix
    , _diagnostics = Nothing
    , _edit = Just (WorkspaceEdit
                    { _changes = Nothing
                    , _documentChanges = Just . List $
                      [ TextDocumentEdit
                        { _textDocument = file
                        , _edits = List [ TextEdit range (renderBasic (hang (range ^. start . character) expr)) ]
                        } ] })
    , _command = Nothing
    }

-- | Convert a span into a range.
rangeOf :: Span -> Range
rangeOf s = Range (startPosOf (spanStart s)) (endPosOf (spanEnd s)) where
  endPosOf (SourcePos _ line col) = Position (line - 1) col
  startPosOf (SourcePos _ line col) = Position (line - 1) (col - 1)

-- | Convert a span into a location.
locationOf :: Span -> Location
locationOf s = Location (Uri (fileName s)) (rangeOf s)

-- | Take the first line of a range.
firstLine :: Range -> Range
firstLine r@(Range start@(Position l c) end)
  | start ^. line == end ^. line = r
  | otherwise = Range start (Position l (c + 1))

renderBasic :: Doc -> T.Text
renderBasic = display . uncommentDoc . renderPretty 0.4 100
