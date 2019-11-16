{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, FlexibleContexts, TypeFamilies #-}
module AmuletLsp.Features where

import Control.Monad.Infer (TypeError(..))
import Control.Lens hiding (List)
import Control.Applicative

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

import qualified Syntax.Resolve.Error as R
import Syntax

import Text.Pretty.Semantic hiding (line)
import Text.Pretty.Note

getOutline :: [Toplevel Parsed] -> [DocumentSymbol]
getOutline = concatMap getTop where
  getName (Name n) = n
  getName InModule{} = "?"

  getApp :: Var Parsed -> [TyConArg Parsed] -> Maybe T.Text
  getApp v args = Just . renderBasic . hsep $ pretty (TyCon v :: Type Parsed) : map pretty args

  mk :: Spanned a
     => Var Parsed -> SymbolKind -> a
     -> Maybe T.Text -> Maybe [DocumentSymbol]
     -> DocumentSymbol
  mk name kind node = mkWith name kind (annotation node) node

  mkWith :: Spanned a
     => Var Parsed -> SymbolKind -> Span -> a
     -> Maybe T.Text -> Maybe [DocumentSymbol]
     -> DocumentSymbol
  mkWith name kind range node detail children =
    DocumentSymbol
    { _name = getName name
    , _detail = detail
    , _kind = kind
    , _deprecated = Nothing
    , _range = rangeOf range
    , _selectionRange = firstLine (rangeOf (annotation node))
    , _children = List <$> children
    }

  getTop :: Toplevel Parsed -> [DocumentSymbol]
  getTop (LetStmt _ _ bindings) = concatMap getBinding bindings
  getTop t@(ForeignVal _ v _ _ _) = [ mk v SkFunction t Nothing Nothing ]

  getTop t@(TypeDecl _ v args ctors _) =
    [ mk v SkStruct t (getApp v args) (map getCtor <$> ctors) ]
  getTop t@(TySymDecl _ v args _ _) = [ mk v SkClass t (getApp v args) Nothing ]
  getTop t@(TypeFunDecl _ v args _ _ _) = [ mk v SkClass t (getApp v args) Nothing ]

  getTop t@(Module _ v (ModStruct ts _)) =
    [ mk v SkModule t Nothing (Just (concatMap getTop ts)) ]
  getTop t@(Module _ v _) = [ mk v SkModule t Nothing Nothing ]
  getTop Open{} = [] -- Skip open and include unconditionally for now.
  getTop Include{} = []

  getTop t@(Class v _ _ args _ ms _) =
    [ mk v SkClass t (getApp v args) (Just (concatMap getClassItem ms)) ]
  getTop Instance{} = []
  getTop DeriveInstance{} = []

  getBinding :: Binding Parsed -> [DocumentSymbol]
  getBinding b@(Binding v _ _ _) = [ mk v SkFunction b Nothing Nothing ]
  getBinding b@(Matching p _ _) = getPattern (annotation b) [] p
  getBinding TypedMatching{} = []

  getClassItem :: ClassItem Parsed -> [DocumentSymbol]
  getClassItem c@(MethodSig v _ _) = [ mk v SkMethod c Nothing Nothing ]
  getClassItem DefaultMethod{} = []
  getClassItem c@(AssocType v args _ _) = [ mk v SkClass c (getApp v args) Nothing ]

  -- For now, constructors don't provide any additional detail. Their definition
  -- /is/ their type signature, so not clear if it's worth exposing it or not.
  getCtor c@(UnitCon _ v _) = mk v SkConstructor c Nothing Nothing
  getCtor c@(ArgCon _ v _ _) = mk v SkConstructor c Nothing Nothing
  getCtor c@(GadtCon _ v _ _) = mk v SkConstructor c Nothing Nothing

  getPattern :: Span -> [DocumentSymbol] -> Pattern Parsed -> [DocumentSymbol]
  getPattern _   ds Wildcard{}           = ds
  getPattern def ds p@(Capture v _)      = mkWith v SkVariable def p Nothing Nothing:ds
  getPattern def ds (Destructure _ p _)  = foldl' (getPattern def) ds p
  getPattern def ds n@(PAs p v _)        = getPattern def (mkWith v SkVariable def n Nothing Nothing:ds) p
  getPattern def ds (PType p _ _)        = getPattern def ds p
  getPattern def ds (PTuple ps _)        = foldl' (getPattern def) ds ps
  getPattern def ds (PRecord ps _)       = foldl' (\x -> getPattern def x . snd) ds ps
  getPattern def ds (PList ps _)         = foldl' (getPattern def) ds ps
  getPattern def ds (PGadtCon _ _ _ p _) = foldl' (getPattern def) ds p
  getPattern _   ds PLiteral{}           = ds

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

-- | Construct a diagnostic of some error.
diagnosticOf :: Note a Style
             => Maybe DiagnosticSource -> (a -> Doc) -> a
             -> Diagnostic
diagnosticOf source disp m =
  Diagnostic
  { _range = rangeOf (annotation m)
  , _severity = Just (severityOf (diagnosticKind m))
  , _code = NumberValue . fromIntegral <$> noteId m
  , _message = renderBasic . disp $ m
  , _relatedInformation = Nothing
  , _source = source
  }

prettyResolve :: R.ResolveError -> Doc
prettyResolve (R.ArisingFrom e _) = prettyResolve e
prettyResolve e = pretty e

severityOf :: NoteKind -> DiagnosticSeverity
severityOf NoteMessage    = DsInfo
severityOf WarningMessage = DsWarning
severityOf ErrorMessage   = DsError

rangeOf :: Span -> Range
rangeOf s = Range (startPosOf (spanStart s)) (endPosOf (spanEnd s)) where
  endPosOf (SourcePos _ line col) = Position (line - 1) col
  startPosOf (SourcePos _ line col) = Position (line - 1) (col - 1)

-- | Take the first line of a range.
firstLine :: Range -> Range
firstLine r@(Range start@(Position l c) end)
  | start ^. line == end ^. line = r
  | otherwise = Range start (Position l (c + 1))

renderBasic :: Doc -> T.Text
renderBasic = display . uncommentDoc . renderPretty 0.4 100
