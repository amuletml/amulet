{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings #-}
module Syntax.Resolve.Error
  ( ResolveError(..)
  , VarKind(..)
  ) where

import Control.Applicative ((<|>))

import qualified Data.Text as T
import Data.Spanned
import Data.Reason
import Data.Maybe

import Syntax.Pretty

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
  = NotInScope VarKind (Var Parsed)   [Var Parsed] -- ^ This object was not in scope

  | Ambiguous (Var Parsed) [Var Resolved] -- ^ This reference could refer to more than one variable
  | NonLinearPattern (Var Resolved) [Pattern Resolved] -- ^ This pattern declares one variable multiple times
  | NonLinearRecord (Expr Parsed) T.Text -- ^ This record declares an entry multiple times

  | EmptyMatch -- ^ This @match@ has no patterns
  | EmptyBegin -- ^ This @begin@ block has no expressions
  | IllegalMethod -- ^ An illegal method within an @instance@

  -- | A wrapper for other errors which adds some additional context,
  -- such as a source position.
  | ArisingFrom ResolveError SomeReason
  deriving (Show)

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

  pretty EmptyMatch = "Empty match expression"
  pretty EmptyBegin = "Empty begin expression"
  pretty IllegalMethod = "Illegal pattern in instance method declaration"

  pretty (ArisingFrom er ex) = pretty er <#> empty <#> nest 4 (string "Arising from use of" <+> blameOf ex </> pretty ex)

instance Spanned ResolveError where
  annotation (ArisingFrom _ x) = annotation x
  annotation (NonLinearRecord e _) = annotation e
  annotation _ = undefined

instance Note ResolveError Style where
  diagnosticKind _ = ErrorMessage

  formatNote f x = indent 2 (Right <$> pretty x) <#> fromJust (body x) where
    body (ArisingFrom er a) = body er <|> Just (f [annotation a])
    body (NonLinearPattern _ ps) = Just (f (map annotation ps))
    body (NonLinearRecord e _) = Just (f [annotation e])
    body _ = Nothing
