{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings #-}
module Syntax.Verify.Error
  ( BindingSite(..)
  , VerifyError(..)
  ) where

import Data.Spanned
import Data.Reason
import Data.Span
import Data.List

import Text.Pretty.Semantic
import Text.Pretty.Note

import Syntax.Verify.Pattern
import Syntax

import Language.Lua.Parser

-- | Bound variable within a term or expression
data BindingSite
  = BindingSite { boundVar :: Var Typed
                , boundWhere :: Span
                , boundType :: Type Typed
                }
  deriving Show

instance Ord BindingSite where
  BindingSite v _ _ `compare` BindingSite v' _ _ = v `compare` v'

instance Eq BindingSite where
  BindingSite v _ _ == BindingSite v' _ _ = v == v'

data VerifyError
  -- | Recursive binding groups which cannot be evaluated, as they depend
  -- on themselves.
  = NonRecursiveRhs { why :: SomeReason
                    , var :: Var Typed
                    , unguarded :: [Var Typed]
                    }
  -- | Unused local variables
  | DefinedUnused BindingSite
  -- | Malformed foreign declarations
  | ParseErrorInForeign { stmt :: Toplevel Typed
                        , err :: ParseError }

  -- | Misleading laziness on let expressions
  | LazyLet (Expr Typed) (Type Typed)
  -- | A pattern which is shadowed by another
  | RedundantArm (Arm Typed)
  -- | This case is missing several patterns
  | MissingPattern (Expr Typed) [ValueAbs Typed]

instance Spanned VerifyError where
  annotation (NonRecursiveRhs e _ _) = annotation e
  annotation (DefinedUnused b) = boundWhere b
  annotation (ParseErrorInForeign _ e) = annotation e
  annotation (LazyLet e _) = annotation e
  annotation (RedundantArm a) = annotation a
  annotation (MissingPattern e _) = annotation e

instance Pretty VerifyError where
  pretty (NonRecursiveRhs re ex xs) =
    vsep [ "Invalid recursive right-hand side for variable" <+> skeyword (pretty ex)
         , if null xs
              then empty
              else note <+> "because evaluation of the variable" <> plural
                        <+> hsep (punctuate comma (map pretty xs)) <+> "is not delayed"
         , nest 4 ("Arising from use of" <+> blameOf re)
         ]
    where plural | length xs == 1 = empty | otherwise = char 's'
  pretty (DefinedUnused (BindingSite v _ _)) =
    string "Bound locally but not used:" <+> squotes (pretty v)
  pretty (ParseErrorInForeign var err) =
    vsep [ "Invalid syntax in definition of foreign value" <+> pretty var
         , pretty err ]
  pretty (LazyLet _ _) =
    vsep [ "Automatic thunking of" <+> keyword "let" <> "s does not cover bindings"
         ]

  pretty (RedundantArm _) =
    vsep [ "Redundant pattern in expression"
         , note <+> "This case is covered by all previous patterns and so can be removed"
         ]
  pretty (MissingPattern _ ps) =
    vsep [ "Non-exhaustive patterns in expression"
         , note <+> "The following patterns are not covered"
         , empty
         , indent 2 . hsep . intersperse pipe . map pretty $ ps ]

instance Note VerifyError Style where
  diagnosticKind NonRecursiveRhs{} = ErrorMessage
  diagnosticKind ParseErrorInForeign{} = WarningMessage
  diagnosticKind DefinedUnused{} = WarningMessage
  diagnosticKind LazyLet{} = WarningMessage
  diagnosticKind RedundantArm{} = WarningMessage
  diagnosticKind MissingPattern{} = WarningMessage

  formatNote f (ParseErrorInForeign (ForeignVal _ var s _ (span, _)) err) =
    let SourcePos name _ _ = spanStart (annotation err)
        spans = [( name, s )]
     in vsep [ indent 2 "Syntax error in definition of" <+> (Right <$> skeyword (pretty var))
             , f [span]
             , empty
             , format (fileSpans spans highlightLua) err
             ]

  formatNote f (LazyLet (Let bs ex _) _) =
    vsep
      [ indent 2 "Automatic thunking of" <+> (Right <$> keyword "let") <> "s does not cover bindings"
      , empty
      , indent 2 $ bullet "Note: the expression"
      , f [annotation ex]
      , indent 2 "will be evaluated lazily, but" <+> (if length bs == 1 then "this" else "these")
          <+> "binding" <> if length bs == 1 then "" else "s"
      , f (fmap annotation bs)
      , indent 2 "are" <+> (Right <$> highlight "strict.")
      , indent 2 $ bullet "Note: if this is what you want, use" <+> (Right <$> keyword "lazy") <+> "explicitly"
      , indent 6 "to silence this warning."
      ]
  formatNote _ LazyLet{} = error "impossible"
  formatNote f (RedundantArm a) =
    vsep [ indent 2 "Redundant pattern in expression"
         , f [annotation a]
         , indent 2 $ note <+> "This case is covered by all previous patterns and so can be removed"
         ]
  formatNote f (MissingPattern a ps) =
    vsep [ indent 2 "Non-exhaustive patterns in expression"
         , f [annotation a]
         , indent 2 $ note <+> "The following patterns are not covered"
         , indent 6 . fmap Right . hsep . intersperse pipe . map pretty $ ps ]

  formatNote f x = indent 2 (Right <$> pretty x) <#> f [annotation x]
