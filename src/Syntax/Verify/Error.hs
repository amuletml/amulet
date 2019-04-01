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
  -- | Foreign types which generate values out of nothing.
  | NonParametricForeign { stmt :: Toplevel Typed
                         , typeOf :: Type Typed
                         , var :: Var Typed }

  -- | An expression in a begin block which does not evaluate to a unit.
  | NonUnitBegin (Expr Typed) (Type Typed)
  -- | Misleading laziness on let expressions
  | LazyLet (Expr Typed) (Type Typed)
  -- | A pattern which is shadowed by another
  | RedundantArm (Arm Typed)
  -- | This case is missing several patterns
  | MissingPattern (Expr Typed) [ValueAbs Typed]
  -- | Polymorphic values aren't shared
  | PolyValue SomeReason (Var Typed) (Type Typed)

instance Spanned VerifyError where
  annotation (NonRecursiveRhs e _ _) = annotation e
  annotation (DefinedUnused b) = boundWhere b
  annotation (ParseErrorInForeign _ e) = annotation e
  annotation (NonParametricForeign s _ _) = annotation s
  annotation (NonUnitBegin e _) = annotation e
  annotation (LazyLet e _) = annotation e
  annotation (RedundantArm a) = annotation a
  annotation (MissingPattern e _) = annotation e
  annotation (PolyValue e _ _) = annotation e

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
  pretty (NonUnitBegin ex ty) =
    vsep [ "This statement discards a value of type"
         , indent 2 (displayType ty)
         , empty
         , bullet "Note: use a" <+> keyword "let" <+> "to silence this warning, as in"
         , indent 2 $
             keyword "let" <+> soperator (char '_') <+> equals <+> pretty ex
         ]
  pretty (LazyLet _ _) =
    vsep [ "Automatic thunking of" <+> keyword "let" <> "s does not cover bindings"
         ]
  pretty (NonParametricForeign _ ty var) =
    vsep [ "Foreign value has implied non-parametric type"
         , bullet "Note: the compiler could assume all functions returning otherwise"
         , indent 8 "unused type variables are non-terminating for optimisation"
         , empty
         , bullet "Note: in this type, no terms of type" <+> stypeVar (pretty var) <+> "are inputs"
         , indent 6 $ displayType ty
         , indent 2 "and so no value of that type could be returned."
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
  pretty (PolyValue v _ _)  = "Polymorphic value" <+> pretty v <+> "behaves like a function"

instance Note VerifyError Style where
  diagnosticKind NonRecursiveRhs{} = ErrorMessage
  diagnosticKind ParseErrorInForeign{} = WarningMessage
  diagnosticKind NonParametricForeign{} = WarningMessage
  diagnosticKind DefinedUnused{} = WarningMessage
  diagnosticKind NonUnitBegin{} = WarningMessage
  diagnosticKind LazyLet{} = WarningMessage
  diagnosticKind RedundantArm{} = WarningMessage
  diagnosticKind MissingPattern{} = WarningMessage
  diagnosticKind PolyValue{} = WarningMessage

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

  formatNote f (PolyValue a var ty) =
    vsep [ indent 2 $ "Polymorphic values are not shared"

         , f [annotation a]

         , indent 2 $ hsep
            [ note, "The variable"
            , Right <$> stypeSkol (pretty var)
            , parens ("of type" <+> fmap Right (displayType ty))
            , "behaves as if"
            ]
         , indent 4 "it were a function, since it was generalised,"
         , indent 4 "and is polymorphic."

         , mempty

         , indent 2 . fmap Right . bullet $ "Suggestion: if this is not intentional, use a type annotation"
         , indent 4 "constraint the type of"
            <+> fmap Right (stypeSkol (pretty var))
            <> char '.'
         ] 

  formatNote f x = indent 2 (Right <$> pretty x) <#> f [annotation x]
