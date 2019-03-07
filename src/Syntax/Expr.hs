{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, OverloadedStrings
  , StandaloneDeriving, TemplateHaskell, TypeFamilies
  , UndecidableInstances #-}

-- | The core types to represent expressions within Amulet's syntax.
module Syntax.Expr where

import Control.Lens hiding (Lazy, (:>))

import Data.Text (Text)
import Data.Bifunctor
import Data.Typeable
import Data.Spanned
import Data.Data
import Data.List

import Text.Pretty.Semantic

import Syntax.Type
import Syntax.Var

-- | A literal value
data Lit
  = LiFloat Double
  | LiInt Integer
  | LiStr Text
  | LiBool Bool
  | LiUnit
  deriving (Eq, Show, Ord, Data, Typeable)

data Binding p
  -- | @let implicit f x = ...@
  = Binding { _bindVariable :: Var p
            , _bindBody :: Expr p
            , _bindVerify :: Bool
            , _bindAnn :: Ann p
            }
  -- | @let (a, b) = ...@
  | Matching { _bindPattern :: Pattern p
             , _bindBody :: Expr p
             , _bindAnn :: Ann p
             }

  -- | @let (a, b) = ...@
  | TypedMatching { _bindPattern :: Pattern p -- fucking ghc, p ~ Typed
                  , _bindBody :: Expr p -- fucking ghc, p ~ Typed
                  , _bindAnn :: Ann p -- fucking ghc, p ~ Typed
                  , _bindBindings :: [(Var Typed, Type Typed)]
                  }

deriving instance (Eq (Var p), Eq (Ann p)) => Eq (Binding p)
deriving instance (Show (Var p), Show (Ann p)) => Show (Binding p)
deriving instance (Ord (Var p), Ord (Ann p)) => Ord (Binding p)
deriving instance (Data p, Typeable p, Data (Var p), Data (Ann p)) => Data (Binding p)

data Parameter p
  = PatParam { _paramPat :: Pattern p }
  | EvParam { _paramPat :: Pattern p }

deriving instance (Eq (Var p), Eq (Ann p)) => Eq (Parameter p)
deriving instance (Show (Var p), Show (Ann p)) => Show (Parameter p)
deriving instance (Ord (Var p), Ord (Ann p)) => Ord (Parameter p)
deriving instance (Data p, Typeable p, Data (Var p), Data (Ann p)) => Data (Parameter p)

data Expr p
  = VarRef (Var p) (Ann p)
  | Let [Binding p] (Expr p) (Ann p)
  | If (Expr p) (Expr p) (Expr p) (Ann p)
  | App (Expr p) (Expr p) (Ann p)
  | Fun (Parameter p) (Expr p) (Ann p)
  | Begin [Expr p] (Ann p)
  | Literal Lit (Ann p)
  | Match (Expr p) [Arm p] (Ann p)
  | Function [Arm p] (Ann p)
  | BinOp (Expr p) (Expr p) (Expr p) (Ann p)
  | Hole (Var p) (Ann p)
  | Ascription (Expr p) (Type p) (Ann p)

  -- Records
  | Record [Field p] (Ann p) -- { foo = bar, baz = quux }
  | RecordExt (Expr p) [Field p] (Ann p) -- { foo with baz = quux }
  | Access (Expr p) Text (Ann p) -- foo.bar

  -- Sections
  | LeftSection (Expr p) (Expr p) (Ann p) -- (+ foo)
  | RightSection (Expr p) (Expr p) (Ann p) -- (foo +)
  | BothSection (Expr p) (Ann p) -- (+)
  | AccessSection Text (Ann p)
  | Parens (Expr p) (Ann p) -- (xyz), just useful for resetting precedence

  -- Tuple (see note [1])
  | Tuple [Expr p] (Ann p)
  | TupleSection [Maybe (Expr p)] (Ann p)

  -- Module
  | OpenIn (Var p) (Expr p) (Ann p)

  -- Laziness
  | Lazy (Expr p) (Ann p)

  -- Visible type application
  | Vta (Expr p) (Type p) (Ann p)

  -- Lists
  | ListExp [Expr p] (Ann p)
  | ListComp (Expr p) [CompStmt p] (Ann p)

  | ExprWrapper (Wrapper p) (Expr p) (Ann p)

deriving instance (Eq (Var p), Eq (Ann p)) => Eq (Expr p)
deriving instance (Show (Var p), Show (Ann p)) => Show (Expr p)
deriving instance (Ord (Var p), Ord (Ann p)) => Ord (Expr p)
deriving instance (Data p, Typeable p, Data (Var p), Data (Ann p)) => Data (Expr p)

data CompStmt p
  = CompGen (Pattern p) (Expr p) (Ann p)
  | CompLet [Binding p] (Ann p)
  | CompGuard (Expr p)

deriving instance (Eq (Var p), Eq (Ann p)) => Eq (CompStmt p)
deriving instance (Show (Var p), Show (Ann p)) => Show (CompStmt p)
deriving instance (Ord (Var p), Ord (Ann p)) => Ord (CompStmt p)
deriving instance (Data p, Typeable p, Data (Var p), Data (Ann p)) => Data (CompStmt p)

data Arm p
  = Arm { armPat :: Pattern p
        , armGuard :: Maybe (Expr p)
        , armExp :: Expr p
        }

deriving instance (Eq (Var p), Eq (Ann p)) => Eq (Arm p)
deriving instance (Show (Var p), Show (Ann p)) => Show (Arm p)
deriving instance (Ord (Var p), Ord (Ann p)) => Ord (Arm p)
deriving instance (Data p, Typeable p, Data (Var p), Data (Ann p)) => Data (Arm p)

data Field p =
  Field { _fName :: Text
        , _fExpr :: Expr p
        , _fAnn :: Ann p
        }

deriving instance (Eq (Var p), Eq (Ann p)) => Eq (Field p)
deriving instance (Show (Var p), Show (Ann p)) => Show (Field p)
deriving instance (Ord (Var p), Ord (Ann p)) => Ord (Field p)
deriving instance (Data p, Typeable p, Data (Var p), Data (Ann p)) => Data (Field p)

data Wrapper p
  = TypeApp (Type p)
  | Cast (Coercion p)
  | TypeLam (Skolem p) (Type p)
  | (:>) (Wrapper p) (Wrapper p)
  | TypeAsc (Type p) -- ^ Invisible (to pretty-printer) ascription
  | WrapVar (Var p) -- ^ Unsolved wrapper variable
  | ExprApp (Expr p)
  | WrapFn (WrapCont p)
  | IdWrap

data WrapCont p = MkWrapCont { runWrapper :: Expr p -> Expr p, desc :: String }

deriving instance Typeable p => Typeable (WrapCont p)
instance Typeable p => Data (WrapCont p) where
  gunfold _ _ = error "gunfold WrapCont"
  toConstr _ = error "toConstr WrapCont"
  dataTypeOf _ = error "dataTypeOf WrapCont"

instance Eq (WrapCont p) where _ == _ = False
instance Ord (WrapCont p) where _ `compare` _ = GT

instance Show (WrapCont p) where
  show = show . desc

infixr 5 :>

deriving instance (Eq (Var p), Eq (Ann p)) => Eq (Wrapper p)
deriving instance (Show (Var p), Show (Ann p)) => Show (Wrapper p)
deriving instance (Ord (Var p), Ord (Ann p)) => Ord (Wrapper p)
deriving instance (Data p, Typeable p, Data (Var p), Data (Ann p)) => Data (Wrapper p)
instance (Data (Var p), Data (Ann p), Data p) => Spanned (Wrapper p)

data Pattern p
  = Wildcard (Ann p)
  | Capture (Var p) (Ann p)
  | Destructure (Var p) (Maybe (Pattern p)) (Ann p)
  | PAs (Pattern p) (Var p) (Ann p)
  | PType (Pattern p) (Type p) (Ann p)
  | PRecord [(Text, Pattern p)] (Ann p)
  | PTuple [Pattern p] (Ann p)
  | PList [Pattern p] (Ann p)
  | PLiteral Lit (Ann p)
  | PWrapper (Wrapper p, Type p) (Pattern p) (Ann p)
  | PSkolem (Pattern p) [Var p] (Ann p)

deriving instance (Eq (Var p), Eq (Ann p)) => Eq (Pattern p)
deriving instance (Show (Var p), Show (Ann p)) => Show (Pattern p)
deriving instance (Ord (Var p), Ord (Ann p)) => Ord (Pattern p)
deriving instance (Data p, Typeable p, Data (Var p), Data (Ann p)) => Data (Pattern p)

makePrisms ''Arm
makePrisms ''Expr
makePrisms ''Lit
makeLenses ''Parameter
makePrisms ''Pattern

makeLenses ''Binding
makeLenses ''Field

instance Spanned (Ann p) => Spanned (Binding p) where
  annotation = annotation . _bindAnn

instance Spanned (Ann p) => Spanned (Expr p) where
  annotation (VarRef _ a) = annotation a
  annotation (Let _ _ a) = annotation a
  annotation (If _ _ _ a) = annotation a
  annotation (App _ _ a) = annotation a
  annotation (Fun _ _ a) = annotation a
  annotation (Begin _ a) = annotation a
  annotation (Literal _ a) = annotation a
  annotation (Match _ _ a) = annotation a
  annotation (Function _ a) = annotation a
  annotation (BinOp _ _ _ a) = annotation a
  annotation (Hole _ a) = annotation a
  annotation (Ascription _ _ a) = annotation a

  annotation (Record _ a) = annotation a
  annotation (RecordExt _ _ a) = annotation a
  annotation (Access _ _ a) = annotation a

  annotation (LeftSection _ _ a) = annotation a
  annotation (RightSection _ _ a) = annotation a
  annotation (BothSection _ a) = annotation a
  annotation (AccessSection _ a) = annotation a
  annotation (Parens _ a) = annotation a

  annotation (Tuple _ a) = annotation a
  annotation (TupleSection _ a) = annotation a

  annotation (OpenIn _ _ a) = annotation a
  annotation (Lazy _ a) = annotation a
  annotation (Vta _ _ a) = annotation a
  annotation (ListExp _ a) = annotation a
  annotation (ListComp _ _ a) = annotation a

  annotation (ExprWrapper _ _ a) = annotation a

instance Spanned (Ann p) => Spanned (Pattern p) where
  annotation (Wildcard a) = annotation a
  annotation (Capture _ a) = annotation a
  annotation (Destructure _ _ a) = annotation a
  annotation (PAs _ _ a) = annotation a
  annotation (PType _ _ a) = annotation a
  annotation (PRecord _ a) = annotation a
  annotation (PTuple _ a) = annotation a
  annotation (PLiteral _ a) = annotation a
  annotation (PWrapper _ _ a) = annotation a
  annotation (PSkolem _ _ a) = annotation a
  annotation (PList _ a) = annotation a

instance Spanned (Ann p) => Spanned (Arm p) where
  annotation (Arm p _ e) = annotation p <> annotation e

instance Spanned (Ann p) => Spanned (Parameter p) where
  annotation = annotation . view paramPat

parenFun :: Pretty (Var p) => Expr p -> Doc
parenFun f = case f of
  Fun{} -> parens (pretty f)
  Let{} -> parens (pretty f)
  Match{} -> parens (pretty f)
  _ -> pretty f

parenArg :: Pretty (Var p) => Expr p -> Doc
parenArg f = case f of
  ExprWrapper w ex _ -> case w of
    IdWrap -> parenArg ex
    _ -> parens (pretty f)
  App{} -> parens (pretty f)
  _ -> parenFun f

instance (Pretty (Var p)) => Pretty (Expr p) where
  pretty (VarRef v _) = pretty v
  pretty (Let [] _ _) = keyword "let" <+> braces mempty
  pretty (Let (x:xs) e _) =
    let prettyBind x = keyword "and" <+> pretty x
     in keyword "let" <+> pretty x
            <#> case xs of
              [] -> keyword "in" <+> pretty e
              _ -> vsep (map prettyBind xs) <#> keyword "in" <+> pretty e
  pretty (If c t e _) = keyword "if" <+> pretty c
                    <#> indent 2 (vsep [ keyword "then" <+> pretty t
                                       , keyword "else" <+> pretty e
                                       ])
  pretty (App f x _) = parenFun f <+> parenArg x
  pretty (Fun v e _) = keyword "fun" <+> pretty v <+> arrow <+> pretty e
  pretty (Begin e _) =
    vsep [ keyword "begin", indent 2 (vsep (punctuate semi (map pretty e))), keyword "end" ]
  pretty (Literal l _) = pretty l
  pretty (BinOp l o r _) = parens (pretty l <+> pretty o <+> pretty r)
  pretty (Match t bs _) = vsep ((keyword "match" <+> pretty t <+> keyword "with"):map pretty bs)
  pretty (Function bs _) = vsep (keyword "function":map pretty bs)
  pretty (Hole v _) = "_" <> pretty v -- A typed hole
  pretty (Ascription e t _) = parens $ pretty e <+> colon <+> pretty t
  pretty (Record [] _) = braces empty
  pretty (Record rows _) = record (map (\(Field n v _) -> text n <+> equals <+> pretty v) rows)
  pretty (RecordExt var rows _) =
    enclose (char '{' <> space)
      (space <> char '}') $
      pretty var <+> keyword "with" <+> hsep (punctuate comma (prettyRows' equals rows))
  pretty (Access e f _) = parenArg e <> dot <> text f

  pretty (LeftSection op vl _) = parens $ pretty op <+> pretty vl
  pretty (RightSection op vl _) = parens $ pretty vl <+> pretty op
  pretty (BothSection op _) = parens $ pretty op
  pretty (AccessSection k _) = parens $ dot <> text k
  pretty (Parens e _) = parens $ pretty e

  pretty (Tuple es _) = parens (hsep (punctuate comma (map pretty es)))
  pretty (TupleSection es _) = parens (hsep (punctuate comma (map (maybe (string "") pretty) es)))

  pretty (OpenIn v e _) = pretty v <> string "." <> parens (pretty e)
  pretty (Lazy e _) = keyword "lazy" <+> parenArg e
  pretty (Vta e t _) = parenFun e <+> keyword "as" <+> pretty t
  pretty (ListExp es _) = brackets (hsep (punctuate comma (map pretty es)))
  pretty (ListComp e qs _) =
    brackets (pretty e <+> pipe <+> hsep (punctuate comma (map pretty qs)))

  pretty (ExprWrapper wrap ex an) = go wrap ex where
    go (TypeLam v t) ex =
      keyword "fun" <+> braces (pretty (TySkol v) <+> colon <+> pretty t) <> dot <+> pretty ex
    go (Cast c) ex = parens (pretty ex <+> soperator (string "|>") <+> pretty c)
    go (TypeApp t) ex = pretty ex <+> braces (pretty t)
    go (ExprApp t) ex = pretty (App ex t undefined)
    go (TypeAsc _) ex = pretty ex
    go (wr :> wi) ex = go wr (ExprWrapper wi ex an)
    go (WrapVar v) ex = pretty ex <+> soperator (char '_') <> pretty v
    go (WrapFn f) ex = pretty (runWrapper f ex)
    go IdWrap ex = pretty ex

instance Pretty (Var p) => Pretty (CompStmt p) where
  pretty (CompGen p e _) = pretty p <+> soperator (string "<-") <+> pretty e
  pretty (CompLet [] _) = keyword "let" <> braces mempty
  pretty (CompLet (x:xs) _) =
    let prettyBind x = keyword "and" <+> pretty x
     in keyword "let" <+> pretty x
            <+> case xs of
              [] -> empty
              _ -> hsep (map prettyBind xs)
  pretty (CompGuard e) = pretty e

instance Pretty (Var p) => Pretty (Arm p) where
  pretty (Arm p g b) = pipe <+> nest 4 (pretty p <+> prettyGuard g <> arrow </> pretty b) where
    prettyGuard Nothing = mempty
    prettyGuard (Just g) = keyword "when" <+> pretty g <+> mempty

instance (Pretty (Var p)) => Pretty (Pattern p) where
  pretty Wildcard{} = skeyword (char '_')
  pretty (Capture x _) = pretty x
  pretty (Destructure x Nothing   _) = stypeCon (pretty x)
  pretty (Destructure x (Just xs) _) = parens $ stypeCon (pretty x) <+> pretty xs
  pretty (PAs p v _) = pretty p <+> keyword "as" <+> pretty v
  pretty (PType p x _) = parens $ pretty p <+> colon <+> pretty x
  pretty (PRecord rows _) = record (prettyRows equals rows)
  pretty (PTuple ps _) = parens (hsep (punctuate comma (map pretty ps)))
  pretty (PList ps _) = brackets (hsep (punctuate comma (map pretty ps)))
  pretty (PLiteral l _) = pretty l
  pretty (PWrapper _ p _) = pretty p
  pretty (PSkolem p _ _) = pretty p

instance Pretty Lit where
  pretty (LiStr s) = sstring (dquotes (text s))
  pretty (LiInt s) = sliteral (integer s)
  pretty (LiFloat s) = sliteral (double s)
  pretty (LiBool True) = sliteral (string "true")
  pretty (LiBool False) = sliteral (string "false")
  pretty LiUnit = sliteral (parens empty)

instance Pretty (Var p) => Pretty (Binding p) where
  pretty (Binding n v _ _) = hsep (pretty n:map pretty args) <> sig <+> nest 2 (equals </> pretty rest') where
    (args, rest) = takeLambdas v
    (sig, rest') = case rest of
      Ascription e t _ -> (space <> colon <+> pretty t, e)
      _ -> (empty, rest)

    takeLambdas (Fun p x _) = first (p:) . takeLambdas $ x
    takeLambdas x = ([], x)
  pretty (Matching p e _) = pretty p <+> nest 2 (equals </> pretty e)
  pretty (TypedMatching p e _ _) = pretty p <+> equals <+> pretty e


instance Pretty (Var p) => Pretty (Parameter p) where
  pretty = pretty . view paramPat

prettyRows' :: Pretty (Var p) => Doc -> [Field p] -> [Doc]
prettyRows' sep = map (\(Field n v _) -> text n <+> sep <+> pretty v) . sortOn (view fName)

{- Note [1]: Tuple types vs tuple patterns/values

    Tuple types only describe *pairs*, but patterns/values can have any
    number of elements. We do this like Idris, in which (a, b, c) = (a,
    (b, c)) -}
