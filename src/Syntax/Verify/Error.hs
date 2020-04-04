{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings #-}
module Syntax.Verify.Error
  ( BindingSite(..)
  , VerifyError(..)
  , WhyRedundant(..)
  ) where

import qualified Data.Text as T
import Data.Position
import Data.Spanned
import Data.Reason
import Data.Span
import Data.List

import Text.Pretty.Semantic
import Text.Pretty.Note

import Syntax.Verify.Pattern
import Syntax

import qualified CompileTarget as CT

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

data WhyRedundant
  = Shadowed -- ^ This arm is shadowed by a previous one.
  | BecauseMatch -- ^ This whole match expression is redundant.
  | BecauseArm -- ^ This specific arm is redundant.
  deriving Show

instance Pretty WhyRedundant where
  pretty Shadowed = "This case is covered by all previous patterns and so can be removed"
  pretty BecauseMatch = "This can be replaced by an empty match."
  pretty BecauseArm = "This case can never occur."

data VerifyError
  -- | Recursive binding groups which cannot be evaluated, as they depend
  -- on themselves.
  = MalformedRecursiveRhs
    { why :: SomeReason
    , var :: Var Typed
    , unguarded :: [Var Typed]
    }
  -- | Unused local variables
  | DefinedUnused BindingSite
  -- | Malformed foreign declarations
  | ParseErrorInForeign
    { stmt :: Toplevel Typed
    , err :: CT.ParseError
    , errTarg :: CT.Target }
  -- | Unknown intrinsic name
  | UnknownIntrinsic
    { stmt      :: Toplevel Typed
    , intrinsic :: T.Text }
  -- | Intrinsic name doesn't match expected type
  | MismatchedIntrinsic
    { stmt      :: Toplevel Typed
    , intrinsic :: T.Text
    , expTy     :: Type Typed
    , actualTy  :: Type Typed }


  -- | Misleading laziness on let expressions
  | LazyLet (Expr Typed) (Type Typed)
  -- | A pattern which is shadowed by another
  | RedundantArm (Arm Typed) WhyRedundant
  -- | This case is missing several patterns
  | MissingPattern Span [ValueAbs Typed]

  -- | This match expression can be rewritten as a @let@.
  | MatchToLet Span (Pattern Typed)
  -- | This function expression can be rewritten as a @fun@.
  | MatchToFun Span (Pattern Typed)

  -- | Top-level @ref α@ binding
  | ToplevelRefBinding BindingSite
  deriving Show


instance Spanned VerifyError where
  spanOf (MalformedRecursiveRhs e _ _) = spanOf e
  spanOf (DefinedUnused b) = boundWhere b
  spanOf (ParseErrorInForeign t _ _) = spanOf t
  spanOf (UnknownIntrinsic s _) = spanOf s
  spanOf (MismatchedIntrinsic s _ _ _) = spanOf s
  spanOf (LazyLet e _) = spanOf e
  spanOf (RedundantArm a _) = spanOf a
  spanOf (MissingPattern e _) = e
  spanOf (MatchToLet e _) = e
  spanOf (MatchToFun e _) = e
  spanOf (ToplevelRefBinding (BindingSite _ s _)) = spanOf s

instance Pretty VerifyError where
  pretty (MalformedRecursiveRhs re ex xs) =
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
  pretty (ParseErrorInForeign _ err _) =
    vsep [ "Invalid syntax in definition of foreign value"
         , pretty err ]
  pretty (UnknownIntrinsic _ t) = "Unknown intrinsic" <+> squotes (text t)
  pretty (MismatchedIntrinsic _ t e a) =
    vsep [ "Mismatched type for intrinsic" <+> squotes (text t) <> ". Definition has type"
         , indent 2 (pretty a)
         , "but should have type"
         , indent 2 (pretty e)
         ]
  pretty (LazyLet _ _) =
    vsep [ "Automatic thunking of" <+> keyword "let" <> "s does not cover bindings"
         ]

  pretty (RedundantArm _ b) =
    vsep [ "Redundant pattern in expression"
         , note <+> pretty b
         ]
  pretty (MissingPattern _ ps) =
    vsep [ "Non-exhaustive patterns in expression"
         , note <+> "The following patterns are not covered"
         , empty
         , indent 2 . hsep . intersperse pipe . map pretty $ ps ]

  pretty (MatchToLet _ p) =
    vsep [ keyword "match" <+> "with a single arm can be rewritten using" <+> keyword "let" <> "."
         , note <+> "Replace with" <+> keyword "let" <+> pretty p <+> equals <+> "…"
         ]

  pretty (MatchToFun _ p) =
    vsep [ keyword "function" <+> "with a single arm can be rewritten using" <+> keyword "fun" <> "."
         , note <+> "Replace with" <+> keyword "fun" <+> pretty p <+> arrow <+> "…"
         ]

  pretty (ToplevelRefBinding (BindingSite _ _ t)) =
    vsep [ "This top-level binding defines a reference"
            <+> parens ("of type" <+> pretty t)
         , note <+> "This is bad style."
         ]


instance Note VerifyError Style where
  diagnosticKind MalformedRecursiveRhs{} = ErrorMessage
  diagnosticKind ParseErrorInForeign{} = WarningMessage
  diagnosticKind UnknownIntrinsic{}    = WarningMessage
  diagnosticKind MismatchedIntrinsic{} = ErrorMessage
  diagnosticKind DefinedUnused{}       = WarningMessage
  diagnosticKind LazyLet{}             = WarningMessage
  diagnosticKind RedundantArm{}        = WarningMessage
  diagnosticKind MissingPattern{}      = WarningMessage
  diagnosticKind MatchToLet{}          = NoteMessage
  diagnosticKind MatchToFun{}          = NoteMessage
  diagnosticKind ToplevelRefBinding{}  = WarningMessage

  formatNote f (ParseErrorInForeign (ForeignVal _ var s _ span) err targ) =
    let SourcePos name _ _ = spanStart (spanOf err)
        spans = [( name, s )]
     in vsep [ indent 2 "Syntax error in definition of" <+> (Right <$> skeyword (pretty var))
             , f [span]
             , empty
             , format (fileSpans spans (CT.highlight targ)) err
             ]

  formatNote f (LazyLet (Let _ bs ex _) _) =
    vsep
      [ indent 2 "Automatic thunking of" <+> (Right <$> keyword "let") <> "s does not cover bindings"
      , empty
      , indent 2 $ bullet "Note: the expression"
      , f [spanOf ex]
      , indent 2 "will be evaluated lazily, but" <+> (if length bs == 1 then "this" else "these")
          <+> "binding" <> if length bs == 1 then "" else "s"
      , f (fmap spanOf bs)
      , indent 2 (if length bs == 1 then "is" else "are") <+> (Right <$> highlight "strict.")
      , indent 2 $ bullet "Note: if this is what you want, use" <+> (Right <$> keyword "lazy") <+> "explicitly"
      , indent 6 "to silence this warning."
      ]
  formatNote _ LazyLet{} = error "impossible"
  formatNote f (RedundantArm a pat) =
    vsep [ indent 2 "Redundant pattern in expression"
         , f [spanOf a]
         , indent 2 $ note <+> (Right <$> pretty pat)
         ]
  formatNote f (MissingPattern a ps) =
    vsep [ indent 2 "Non-exhaustive patterns in expression"
         , f [a]
         , indent 2 $ note <+> "The following patterns are not covered"
         , indent 6 . fmap Right . hsep . intersperse pipe . map pretty $ ps ]

  formatNote f (MatchToLet a pat) =
    vsep [ indent 2 $ Right <$> keyword "match" <+> "with a single arm can be rewritten using" <+> keyword "let" <> "."
         , f [a]
         , indent 2 $ Right <$> note <+> "Replace with" <+> keyword "let" <+> pretty pat <+> equals <+> "…"
         ]

  formatNote f (MatchToFun a pat) =
    vsep [ indent 2 $ Right <$> keyword "function" <+> "with a single arm can be rewritten using" <+> keyword "fun" <> "."
         , f [a]
         , indent 2 $ Right <$> note <+> "Replace with" <+> keyword "fun" <+> pretty pat <+> arrow <+> "…"
         ]

  formatNote f x = indent 2 (Right <$> pretty x) <#> f [spanOf x]

  noteId MalformedRecursiveRhs{} = Just 3001
  noteId DefinedUnused{}       = Just 3002
  noteId ParseErrorInForeign{} = Just 3003
  noteId UnknownIntrinsic{}    = Just 3010
  noteId MismatchedIntrinsic{} = Just 3011
  noteId LazyLet{}             = Just 3004
  noteId RedundantArm{}        = Just 3005
  noteId MissingPattern{}      = Just 3006
  noteId MatchToLet{}          = Just 3007
  noteId MatchToFun{}          = Just 3008
  noteId ToplevelRefBinding{}  = Just 3009
