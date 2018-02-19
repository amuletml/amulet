{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies, DataKinds #-}
{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
module Syntax where

import Prelude hiding ((<$>))
import Text.PrettyPrint.Leijen

import qualified Data.Text as T
import Data.Text (Text)
import Data.Spanned
import Data.Span

import Data.List.NonEmpty(NonEmpty ((:|)))
import Data.Semigroup (sconcat)
import Data.Foldable
import Data.Typeable
import Data.Triple
import Data.Maybe
import Data.Data

import Control.Lens

newtype Parsed = Parsed Parsed deriving Data
newtype Resolved = Resolved Resolved deriving Data
newtype Typed = Typed Typed deriving Data

data family Var a

data instance Var Parsed
  = Name Text
  deriving (Eq, Show, Ord, Data)

data instance Var Resolved
  = TgName Text {-# UNPACK #-} !Int
  | TgInternal Text
  deriving (Show, Data)

instance Eq (Var Resolved) where
  (TgName _ a) == (TgName _ b) = a == b
  (TgInternal a) == (TgInternal b) = a == b
  _ == _ = False

instance Ord (Var Resolved) where
  (TgName _ a) `compare` (TgName _ b) = a `compare` b
  (TgInternal a) `compare` (TgInternal b) = a `compare` b

  (TgName _ _) `compare` (TgInternal _) = GT
  (TgInternal _) `compare` (TgName _ _) = LT

data instance Var Typed
  = TvName (Var Resolved)
  deriving (Show, Data, Eq, Ord)

type family Ann a :: * where
  Ann Parsed = Span
  Ann Resolved = Span
  Ann Typed = (Span, Type Typed)

data Expr p
  = VarRef (Var p) (Ann p)
  | Let [(Var p, Expr p, Ann p)] (Expr p) (Ann p)
  | If (Expr p) (Expr p) (Expr p) (Ann p)
  | App (Expr p) (Expr p) (Ann p)
  | Fun (Pattern p) (Expr p) (Ann p)
  | Begin [Expr p] (Ann p)
  | Literal Lit (Ann p)
  | Match (Expr p) [(Pattern p, Expr p)] (Ann p)
  | BinOp (Expr p) (Expr p) (Expr p) (Ann p)
  | Hole (Var p) (Ann p)
  | Ascription (Expr p) (Type p) (Ann p)

  -- Records
  | Record [(Text, Expr p)] (Ann p) -- { foo = bar, baz = quux }
  | RecordExt (Expr p) [(Text, Expr p)] (Ann p) -- { foo with baz = quux }
  | Access (Expr p) Text (Ann p) -- foo.bar

  -- Sections
  | LeftSection (Expr p) (Expr p) (Ann p) -- (+ foo)
  | RightSection (Expr p) (Expr p) (Ann p) -- (foo +)
  | BothSection (Expr p) (Ann p) -- (+)
  | AccessSection Text (Ann p)

  -- Tuple (see note [1])
  | Tuple [Expr p] (Ann p)

  -- Explicit type application
  | TypeApp (Expr p) (Type p) (Ann p)


deriving instance (Eq (Var p), Eq (Ann p)) => Eq (Expr p)
deriving instance (Show (Var p), Show (Ann p)) => Show (Expr p)
deriving instance (Ord (Var p), Ord (Ann p)) => Ord (Expr p)
deriving instance (Data p, Typeable p, Data (Var p), Data (Ann p)) => Data (Expr p)
instance (Data (Var p), Data (Ann p), Data p) => Spanned (Expr p)

data Pattern p
  = Wildcard (Ann p)
  | Capture (Var p) (Ann p)
  | Destructure (Var p) (Maybe (Pattern p)) (Ann p)
  | PType (Pattern p) (Type p) (Ann p)
  | PRecord [(Text, Pattern p)] (Ann p)
  | PTuple [Pattern p] (Ann p)

deriving instance (Eq (Var p), Eq (Ann p)) => Eq (Pattern p)
deriving instance (Show (Var p), Show (Ann p)) => Show (Pattern p)
deriving instance (Ord (Var p), Ord (Ann p)) => Ord (Pattern p)
deriving instance (Data p, Typeable p, Data (Var p), Data (Ann p)) => Data (Pattern p)
instance (Data (Var p), Data (Ann p), Data p) => Spanned (Pattern p)

data Lit
  = LiInt Integer
  | LiStr Text
  | LiBool Bool
  | LiUnit
  deriving (Eq, Show, Ord, Data, Typeable)

data Type p
  = TyCon (Var p)
  | TyVar (Var p)
  | TyForall [Var p] (Type p)
  | TyArr (Type p) (Type p)
  | TyApp (Type p) (Type p)
  | TyRows (Type p) [(Text, Type p)]  -- { α | foo : int, bar : string }
  | TyExactRows [(Text, Type p)] -- { foo : int, bar : string }
  | TyTuple (Type p) (Type p) -- (see note [1])

  | TySkol (Skolem p)

data Skolem p
  = Skolem (Var p) -- the constant itself
           (Var p) -- what variable this skolemises
           (Type p) -- the type this was generated for

deriving instance (Show (Var p), Show (Ann p)) => Show (Skolem p)
deriving instance (Data p, Typeable p, Data (Var p), Data (Ann p)) => Data (Skolem p)

instance Eq (Var p) => Eq (Skolem p) where
  Skolem v _ _  == Skolem v' _ _ = v == v'

instance Ord (Var p) => Ord (Skolem p) where
  Skolem v _ _ `compare` Skolem v' _ _ = v `compare` v'

deriving instance (Eq (Var p), Eq (Ann p)) => Eq (Type p)
deriving instance (Show (Var p), Show (Ann p)) => Show (Type p)
deriving instance (Ord (Var p), Ord (Ann p)) => Ord (Type p)
deriving instance (Data p, Typeable p, Data (Var p), Data (Ann p)) => Data (Type p)

data Kind p
  = KiStar
  | KiArr (Kind p) (Kind p)
  | KiVar (Var p)
  | KiForall [Var p] (Kind p)

deriving instance (Eq (Var p), Eq (Ann p)) => Eq (Kind p)
deriving instance (Show (Var p), Show (Ann p)) => Show (Kind p)
deriving instance (Ord (Var p), Ord (Ann p)) => Ord (Kind p)
deriving instance (Data p, Typeable p, Data (Var p), Data (Ann p)) => Data (Kind p)

data Toplevel p
  = LetStmt [(Var p, Expr p, Ann p)]
  | ForeignVal (Var p) Text (Type p) (Ann p)
  | TypeDecl (Var p) [Var p] [Constructor p]

instance (Spanned (Constructor p), Ann p ~ Span) => Spanned (Toplevel p) where
  annotation (LetStmt ((_, _, x):vs)) = sconcat (x :| map thd3 vs)
  annotation (TypeDecl _ _ (x:xs)) = sconcat (annotation x :| map annotation xs)
  annotation (ForeignVal _ _ _ x) = x
  annotation _ = internal

deriving instance (Eq (Var p), Eq (Ann p)) => Eq (Toplevel p)
deriving instance (Show (Var p), Show (Ann p)) => Show (Toplevel p)
deriving instance (Ord (Var p), Ord (Ann p)) => Ord (Toplevel p)
deriving instance (Data p, Typeable p, Data (Var p), Data (Ann p)) => Data (Toplevel p)

data Constructor p
  = UnitCon (Var p) (Ann p)
  | ArgCon (Var p) (Type p) (Ann p)

deriving instance (Eq (Var p), Eq (Ann p)) => Eq (Constructor p)
deriving instance (Show (Var p), Show (Ann p)) => Show (Constructor p)
deriving instance (Ord (Var p), Ord (Ann p)) => Ord (Constructor p)
deriving instance (Data p, Typeable p, Data (Var p), Data (Ann p)) => Data (Constructor p)
instance (Data (Var p), Data (Ann p), Data p) => Spanned (Constructor p)

--- Pretty-printing {{{

instance (Pretty (Var p)) => Pretty (Expr p) where
  pretty (VarRef v _) = pretty v
  pretty (Let [] _ _) = error "absurd: never parsed"
  pretty (Let ((n, v, _):xs) e _) =
    let prettyBind (n, v, _) = string "and" <+> pretty n <+> nest 2 (equals </> pretty v)
     in align $ string "let" <+> pretty n <+> nest 2 (equals </> pretty v)
            <$> case xs of
              [] -> string "in" <+> pretty e
              _ -> vsep (map prettyBind xs) <$> string "in" <+> pretty e
  pretty (If c t e _) = string "if" <+> pretty c
                    <$> indent 2 (vsep [ string "then" <+> pretty t
                                       , string "else" <+> pretty e
                                       ])
  pretty (App c (e@App{}) _) = pretty c <+> parens (pretty e)
  pretty (App c (e@Fun{}) _) = pretty c <+> parens (pretty e)
  pretty (App f x _) = pretty f <+> pretty x
  pretty (Fun v e _) = string "fun" <+> pretty v <+> nest 2 (string "->" </> pretty e)
  pretty (Begin e _) =
    vsep [ string "begin", indent 2 (vsep (punctuate semi (map pretty e))), string "end" ]
  pretty (Literal l _) = pretty l
  pretty (BinOp l o r _) = parens (pretty l <+> pretty o <+> pretty r)
  pretty (Match t bs _) = vsep ((string "match" <+> pretty t <+> string "with"):prettyMatches bs)
  pretty (Hole v _) = pretty v -- A typed hole
  pretty (Ascription e t _) = parens $ pretty e <+> colon <+> pretty t
  pretty (Record rows _) = record (map (\(n, v) -> text (T.unpack n) <+> equals <+> pretty v) rows)
  pretty (RecordExt var rows _) = braces $ pretty var <> text "with" <> hsep (punctuate comma (prettyRows rows))
  pretty (Access x@VarRef{} f _) = pretty x <> dot <> text (T.unpack f)
  pretty (Access e f _) = parens (pretty e) <> dot <> text (T.unpack f)

  pretty (LeftSection op vl _) = parens $ pretty op <+> pretty vl
  pretty (RightSection op vl _) = parens $ pretty vl <+> pretty op
  pretty (BothSection op _) = parens $ pretty op
  pretty (AccessSection k _) = parens $ dot <> text (T.unpack k)

  pretty (Tuple es _) = tupled (map pretty es)
  pretty (TypeApp f x _) = pretty f <+> text "@" <> pretty x

prettyMatches :: (Pretty (Pattern p), Pretty (Expr p)) => [(Pattern p, Expr p)] -> [Doc]
prettyMatches = map (\(a, b) -> char '|' <+> pretty a <+> string "->" <+> pretty b)

prettyRows :: Pretty x => [(Text, x)] -> [Doc]
prettyRows = map (\(n, v) -> text (T.unpack n) <+> equals <+> pretty v)

instance (Pretty (Var p)) => Pretty (Pattern p) where
  pretty Wildcard{} = char '_'
  pretty (Capture x _) = pretty x
  pretty (Destructure x Nothing   _) = pretty x
  pretty (Destructure x (Just xs) _) = parens $ pretty x <+> pretty xs
  pretty (PType p x _) = parens $ pretty p <+> colon <+> pretty x
  pretty (PRecord rows _) = record (prettyRows rows)
  pretty (PTuple ps _) = tupled (map pretty ps)

instance Pretty Lit where
  pretty (LiStr s) = dquotes (text (T.unpack s))
  pretty (LiInt s) = integer s
  pretty (LiBool True) = text "true"
  pretty (LiBool False) = text "false"
  pretty LiUnit = text "unit"

instance (Pretty (Var p)) => Pretty (Type p) where
  pretty (TyCon v) = pretty v
  pretty (TyVar v) = squote <> pretty v
  pretty (TySkol (Skolem v _ _)) = dquote <> pretty v
  pretty (TyForall vs v)
    = text "forall" <+> hsep (map ((squote <>) . pretty) vs) <> dot <+> pretty v

  pretty (TyArr x e)
    | TyArr{} <- x = parens (pretty x) <+> text "->" <+> pretty e
    | TyForall{} <- x = parens (pretty x) <+> text "->" <+> pretty e
    | TyTuple{} <- x = parens (pretty x) <+> text "->" <+> pretty e
    | otherwise = pretty x <+> text "->" <+> pretty x

  pretty (TyRows p rows) = braces $ pretty p <+> char '|' <+> hsep (punctuate comma (prettyRows rows)) 
  pretty (TyExactRows rows) = record (prettyRows rows)

  pretty (TyApp e x@TyApp{}) = pretty e <+> parens (pretty x)
  pretty (TyApp x e) = pretty x <+> pretty e
  pretty (TyTuple a b)
    | TyTuple{} <- a
    = parens (pretty a) <+> char '*' <+> pretty b
    | otherwise
    = pretty a <+> char '*' <+> pretty b

instance Pretty (Var p) => Pretty (Kind p) where
  pretty KiStar = text "Type"
  pretty (KiArr a b)
    | KiArr{} <- a = parens (pretty a) <+> text "->" <+> pretty b
    | otherwise = pretty a <+> text "->" <+> pretty b

  pretty (KiVar v) = squote <> pretty v
  pretty (KiForall vs v)
    = text "forall" <+> hsep (map ((squote <>) . pretty) vs) <> dot <+> pretty v

instance (Pretty (Var p)) => Pretty (Toplevel p) where
  pretty (LetStmt []) = error "absurd!"
  pretty (LetStmt ((n, v, _):xs)) =
    let prettyBind (n, v, _) = string "and" <+> pretty n <+> nest 2 (equals </> pretty v)
     in align $ string "let" <+> pretty n <+> nest 2 (equals </> pretty v)
            <$> vsep (map prettyBind xs)
  pretty (ForeignVal v d ty _) = text "foreign val" <+> pretty v <+> colon <+> pretty ty <+> equals <+> dquotes (text (T.unpack d))
  pretty (TypeDecl ty args ctors) = text "type" <+> pretty ty
                                <+> hsep (map ((squote <>) . pretty) args)
                                <+> equals
                                <$> vsep (map ((char '|' <+>) . pretty) ctors)

instance (Pretty (Var p)) => Pretty [Toplevel p] where
  pretty = vcat . map pretty

instance (Pretty (Var p)) => Pretty (Constructor p) where
  pretty (UnitCon p _) = pretty p
  pretty (ArgCon p t _) = pretty p <+> text "of" <+> pretty t

instance Pretty (Var Parsed) where
  pretty (Name v) = text (T.unpack v)

instance Pretty (Var Resolved) where
  pretty (TgName v _) = text (T.unpack v)
  -- pretty (TgName v i) = pretty v <+> "#" <+> i
  pretty (TgInternal v) = text (T.unpack v)

instance Pretty (Var Typed) where
  pretty (TvName v) = pretty v
  -- pretty (TvName v t)
    -- | t == internalTyVar = pretty v
    -- | otherwise = parens $ v <+> opClr " : " <+> t
    --
instance Pretty (Span, Type Typed) where
  pretty (x, _) = pretty x

record :: [Doc] -> Doc
record = encloseSep lbrace rbrace comma

-- }}}

unTvName :: Var Typed -> Var Resolved
unTvName (TvName x) = x

getType :: Data (f Typed) => f Typed -> Type Typed
getType = snd . head . catMaybes . gmapQ get where
  get d = fmap (`asTypeOf` (undefined :: (Span, Type Typed))) (cast d)
  -- FIXME: Point-freeing this definition makes type inference broken.
  -- Thanks, GHC.

makePrisms ''Expr
makePrisms ''Type


{- Note [1]: Tuple types vs tuple patterns/values

    Tuple types only describe *pairs*, but patterns/values can have any
    number of elements. We do this like Idris, in which (a, b, c) = (a,
    (b, c)) -}

--- vim: fdm=marker
