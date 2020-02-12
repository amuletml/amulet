{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings #-}
module Syntax.Resolve.Error
  ( ResolveError(..)
  , VarKind(..)
  ) where

import qualified Data.List.NonEmpty as E
import qualified Data.Text as T
import Data.Position
import Data.Spanned
import Data.Reason
import Data.Span

import Syntax.Resolve.Import
import Syntax

import qualified CompileTarget as CT

import Text.Pretty.Semantic
import Text.Pretty.Note

data VarKind
  = VarVar
  | VarCtor
  | VarType
  | VarTyvar
  | VarModule
  deriving (Show, Eq)

-- | An error in the resolution process. Note that one error may be
-- thrown multiple times.
data ResolveError
  -- ^ This object was not in scope.
  --
  -- This optionally contains a location where a recursive modifier should be
  -- added.
  = NotInScope VarKind (Var Parsed) (Maybe Span)

  | Ambiguous (Var Parsed) [Var Resolved] -- ^ This reference could refer to more than one variable
  | NonLinearPattern (Var Resolved) [Pattern Resolved] -- ^ This pattern declares one variable multiple times
  | NonLinearRecord (Expr Parsed) T.Text -- ^ This record declares an entry multiple times

  | EmptyBegin -- ^ This @begin@ block has no expressions
  | IllegalMethod -- ^ An illegal method within an @instance@
  | LastStmtNotExpr -- ^ Invalid statement in tail position
  | LetOpenStruct -- ^ Invalid module struct in a let open.

  | UnresolvedImport T.Text SearchedIn -- ^ Cannot resolve this module.
  | ImportLoop (E.NonEmpty (SourceName, Span)) -- ^ Cyclic dependencies when loading modules.
  -- | This file errored when importing. This is only used when compiling
  -- single files (such as in an editor).
  | ImportError Span FilePath
  | NoMatchingImport CT.Target -- ^ No imports available for this compile target.
  | ManyMatchingImports CT.Target [TargetImport Parsed] Span -- ^ Multiple matching imports for this compile target.

  | TFClauseWrongHead (Type Parsed) (Var Parsed)
  | TFClauseWrongArity Int Int

  -- | A wrapper for other errors which adds some additional context,
  -- such as a source position.
  | ArisingFrom ResolveError SomeReason
  deriving (Show)

searchedIn :: SearchedIn -> Doc
searchedIn (Relative _) = mempty
searchedIn (LibraryPath []) =
  mempty <#> "The library path appears to be empty. Is Amulet correctly configured?"
searchedIn (LibraryPath xs) =
  mempty <#> "Searched in:" <#> indent 2 (vsep . map (bullet . string) $ xs)

instance Pretty VarKind where
  pretty VarVar = "Variable"
  pretty VarCtor = "Constructor"
  pretty VarType = "Type"
  pretty VarTyvar = "Type variable"
  pretty VarModule = "Module"

instance Pretty ResolveError where
  pretty (NotInScope k e _) = pretty k <+> "not in scope:" <+> verbatim e

  pretty (Ambiguous v _) = "Ambiguous reference to variable:" <+> verbatim v
  pretty (NonLinearPattern v _) = "Non-linear pattern (multiple definitions of" <+> verbatim v <+> ")"
  pretty (NonLinearRecord _ t) = "Duplicate field" <+> stypeSkol (text t) <+> "in record" <#> empty

  pretty EmptyBegin = "Empty begin expression"
  pretty IllegalMethod = "Illegal pattern in instance method declaration"
  pretty LastStmtNotExpr = "The last statement in a" <+> keyword "begin" <+> "block should be an expression"
  pretty LetOpenStruct = "Cannot declare a module within a" <+> keyword "let open" <+> "expression"

  pretty (UnresolvedImport name search) = "Cannot resolve" <+> dquotes (text name) <> searchedIn search
  pretty (ImportLoop _) = "Modules form an import cycle"
  pretty (ImportError _ file) = "Error importing" <+> dquotes (string file)
  pretty (NoMatchingImport target) = "No suitable import for compile target" <+> dquotes (text (CT.name target))
  pretty (ManyMatchingImports target _ _) = "Multiple possible imports for compile target" <+> dquotes (text (CT.name target))

  pretty (TFClauseWrongHead t tau) =
    vsep [ "The lhs of a type function equation must be headed by the type function constructor"
         , "Expected" <+> pretty tau <+> "but got" <+> pretty t
         ]
  pretty (TFClauseWrongArity t tau) =
    vsep [ "The lhs of a type function equation must have the same arity as the type function itself"
         , "Expected" <+> int tau <+> "but got" <+> int t
         ]

  pretty (ArisingFrom er ex) =
    pretty er <#> empty <#> nest 4 (string "Arising from use of" <+> blameOf ex)

instance Spanned ResolveError where
  spanOf (ArisingFrom _ x) = spanOf x
  spanOf (NonLinearRecord e _) = spanOf e
  spanOf (ImportError e _) = spanOf e
  spanOf (ManyMatchingImports _ _ a) = a
  spanOf x = error (show x)

instance Note ResolveError Style where
  diagnosticKind _ = ErrorMessage

  formatNote f x = body mempty x where
    body :: NoteDoc Style -> ResolveError -> NoteDoc Style
    body _ (ArisingFrom er a) = body (f [spanOf a]) er
    body _ (NonLinearPattern _ ps) = def <#> f (map spanOf ps)
    body _ (NonLinearRecord e _) = def <#> f [spanOf e]
    body d (UnresolvedImport name search)
        = wrap ("Cannot resolve" <+> dquotes (text name))
      <#> d
      <#> wrap (searchedIn search)
    body _ (ImportLoop loop) = def <#> foldl1 (<#>) (E.map imported loop)
    body _ (ManyMatchingImports _ xs _) = def <#> f (map spanOf xs)
    body d (NotInScope _ _ (Just pos))
        = def <#> d
      <#> wrap ("Do you need a" <+> keyword "rec" <+> "modifier here?")
      <#> f [pos]
    body d _ = def <#> d

    wrap d = indent 2 (Right <$> d)
    def = wrap (pretty x)

    imported (name, pos)
      = f [ pos ]
        <#> indent 2 (note <+> dquotes (text name) <+> "imported from" <+> dquotes (text (fileName pos)))

  noteId NotInScope{}         = Just 1001
  noteId Ambiguous{}          = Just 1002
  noteId NonLinearPattern{}   = Just 1003
  noteId NonLinearRecord{}    = Just 1004
  noteId EmptyBegin{}         = Nothing
  noteId IllegalMethod{}      = Just 1007
  noteId LastStmtNotExpr{}    = Just 1008
  noteId LetOpenStruct{}      = Just 1009
  noteId UnresolvedImport{}   = Just 1010
  noteId ImportLoop{}         = Just 1011
  noteId ImportError{}        = Nothing
  noteId NoMatchingImport{}   = Just 1014
  noteId ManyMatchingImports{}  = Just 1015
  noteId TFClauseWrongHead{}  = Just 1012
  noteId TFClauseWrongArity{} = Just 1013

  noteId (ArisingFrom e _)    = noteId e
