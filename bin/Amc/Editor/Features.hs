{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, FlexibleContexts, TypeFamilies #-}
module Amc.Editor.Features where

import           Control.Applicative
import           Control.Lens hiding (List)
import           Control.Monad.Infer (TypeError(..))
import           Data.Foldable
import           Data.Maybe
import           Data.Position
import           Data.Span
import           Data.Spanned
import qualified Data.Text as T
import           Frontend.Errors
import           Language.Haskell.LSP.Types
import           Language.Haskell.LSP.Types.Lens hiding (error)
import           Prelude hiding (id)
import           Syntax
import           Text.Pretty.Note
import           Text.Pretty.Semantic hiding (line)

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
  mk name kind node detail children =
    DocumentSymbol
    { _name = getName name
    , _detail = detail
    , _kind = kind
    , _deprecated = Nothing
    , _range = rangeOf (annotation node)
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


  getBinding b@(Binding v _ _ _) = [ mk v SkFunction b Nothing Nothing ]
  getBinding Matching{} = [] -- TODO: Fill this one out.
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

-- | Get code lenses for this program.
--
-- Currently, this provides type signatures for any top-level binding within the
-- program.
getCodeLenses :: [Toplevel Typed] -> [CodeLens]
getCodeLenses = getTops [] where
  -- TODO: Split this into "get lenses" (which uses the resolved tree) and
  -- "resolve lenses" (which uses the type environment). We'll need to ensure
  -- the two match up.

  getTop ac (LetStmt _ _ b) = foldl getBinding ac b
  getTop ac (Module _ _ (ModStruct ms _)) = getTops ac ms
  getTop ac (Open (ModStruct ms _)) = getTops ac ms
  getTop ac (Include (ModStruct ms _)) = getTops ac ms
  getTop ac _ = ac
  getTops = foldl' getTop

  getBinding :: [CodeLens] -> Binding Typed -> [CodeLens]
  getBinding ac (Binding v _ _ (p, ty)) =
    CodeLens (rangeOf p)
      (Just (Command (renderBasic $ pretty v <+> colon <+> displayType ty) "" Nothing))
      Nothing
    : ac
  getBinding ac _ = ac

-- | Construct a diagnostic of some error.
diagnosticOf :: (Note a Style, Pretty a) => Maybe DiagnosticSource -> a -> Diagnostic
diagnosticOf source m =
  Diagnostic
  { _range = rangeOf (annotation m)
  , _severity = Just (severityOf (diagnosticKind m))
  , _code = NumberValue . fromIntegral <$> noteId m
  , _message = renderBasic . pretty $ m
  , _relatedInformation = Nothing
  , _source = source
  }

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
