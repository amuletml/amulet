{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies, DataKinds #-}
{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
module Syntax where

import Pretty

import Data.Text (Text)
import Data.Spanned
import Data.Span

import Data.List.NonEmpty(NonEmpty ((:|)))
import Data.Semigroup
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
  pprint (VarRef v _) = pprint v
  pprint (Let [] _ _) = error "absurd: never parsed"
  pprint (Let ((n, v, _):xs) e _) = do
    kwClr "let " <+> n <+> opClr " = " <+> v <+> newline
    for_ xs $ \(n, v, _) ->
      kwClr "and " <+> n <+> opClr " = " <+> v <+> newline
    pprint e
  pprint (If c t e _) = do
    kwClr "if " <+> c <+> newline
    block 2 $ do
      kwClr "then " <+> t <+> newline
      kwClr "else " <+> e
  pprint (App c (e@App{}) _) = c <+> " " <+> parens e
  pprint (App f x _) = f <+> " " <+> x
  pprint (Fun v e _) = kwClr "fun " <+> v <+> opClr " -> " <+> e
  pprint (Begin e _) = do
    kwClr "begin "
    body 2 e *> newline
    kwClr "end"
  pprint (Literal l _) = pprint l
  pprint (BinOp l o r _) = parens (pprint l <+> " " <+> pprint o <+> " " <+> pprint r)
  pprint (Match t bs _) = do
    kwClr "match " <+> t <+> " with"
    body 2 bs *> newline
  pprint (Hole v _) = pprint v -- A typed hole
  pprint (Ascription e t _) = parens $ e <+> opClr " : " <+> t
  pprint (Record rows _) = braces $ interleave ", " $ map (\(n, v) -> n <+> opClr " = " <+> v) rows
  pprint (RecordExt var rows _) = braces $ var <+> kwClr " with " <+> interleave ", " (map (\(n, v) -> n <+> opClr " = " <+> v) rows)
  pprint (Access x@VarRef{} f _) = x <+> opClr "." <+> f
  pprint (Access e f _) = parens e <+> opClr "." <+> f

  pprint (LeftSection op vl _) = parens $ opClr op <+> " " <+> vl
  pprint (RightSection op vl _) = parens $ vl <+> " " <+> opClr op
  pprint (BothSection op _) = parens $ opClr op
  pprint (AccessSection k _) = parens $ opClr "." <+> k

  pprint (Tuple es _) = parens $ interleave ", " es
  pprint (TypeApp f x _) = f <+> opClr " @" <+> x

instance (Pretty (Var p)) => Pretty (Pattern p, Expr p) where
  pprint (a, b) = opClr "| " <+> a <+> " -> " <+> b

instance (Pretty (Var p)) => Pretty (Expr p, Expr p) where
  pprint (a, b) = opClr "| " <+> a <+> " -> " <+> b

instance (Pretty (Var p)) => Pretty (Pattern p) where
  pprint Wildcard{} = kwClr "_"
  pprint (Capture x _) = pprint x
  pprint (Destructure x Nothing   _) = pprint x
  pprint (Destructure x (Just xs) _) = parens $ x <+> " " <+> xs
  pprint (PType p x _) = parens $ p <+> opClr " : " <+> x
  pprint (PRecord rows _) = braces $ interleave ", " $ map (\(x, y) -> x <+> opClr " = " <+> y) rows
  pprint (PTuple ps _) = parens $ interleave ", " ps

instance Pretty Lit where
  pprint (LiStr s) = str s
  pprint (LiInt s) = litClr s
  pprint (LiBool True) = litClr "true"
  pprint (LiBool False) = litClr "false"
  pprint LiUnit = litClr "unit"

instance (Pretty (Var p)) => Pretty (Type p) where
  pprint (TyCon v) = typeClr v
  pprint (TyVar v) = "'" <+> tvClr v
  pprint (TySkol (Skolem v _ _)) = kwClr v
  pprint (TyForall vs v)
    = kwClr "∀ " <+> interleave " " (map (\x -> "'" <+> tvClr x) vs) <+> opClr ". " <+> v

  pprint (TyArr x e)
    | TyArr{} <- x = parens x <+> opClr " -> " <+> e
    | TyForall{} <- x = parens x <+> opClr " -> " <+> e
    | TyTuple{} <- x = parens x <+> opClr " -> " <+> e
    | otherwise = x <+> opClr " -> " <+> e
  pprint (TyRows p rows) = braces $ p <+> opClr " | " <+> prettyRows rows where
    prettyRows = interleave ", " . map (\(x, t) -> x <+> opClr " : " <+> t)

  pprint (TyExactRows rows) = braces $ prettyRows rows where
    prettyRows = interleave ", " . map (\(x, t) -> x <+> opClr " : " <+> t)

  pprint (TyApp e x@TyApp{}) = e <+> " " <+> parens x
  pprint (TyApp x e) = x <+> opClr " " <+> e
  pprint (TyTuple a b)
    | TyTuple{} <- a
    = parens a <+> opClr " * " <+> b
    | otherwise
    = a <+> opClr " * " <+> b

instance Pretty (Var p) => Pretty (Kind p) where
  pprint KiStar = kwClr "Type"
  pprint (KiArr a b)
    | KiArr{} <- a = parens a <+> opClr " -> " <+> b
    | otherwise = a <+> opClr " -> " <+> b
  pprint (KiVar v) = opClr "'" <+> tvClr v
  pprint (KiForall vs v)
    = kwClr "∀ " <+> interleave " " (map (\x -> "'" <+> tvClr x) vs) <+> opClr ". " <+> v

instance (Pretty (Var p)) => Pretty (Toplevel p) where
  pprint (LetStmt vs) = opClr "let " <+> interleave (newline <+> opClr "and ") (map pVars vs) where
    pVars (v, e, _) = v <+> " = " <+> block 2 e
  pprint (ForeignVal v d ty _) = kwClr "foreign val " <+> v <+> opClr ": "
                           <+> ty <+> opClr " = " <+> str d
  pprint (TypeDecl ty args ctors) = do
    kwClr "type " <+> ty
    traverse_ (" '" <+>) args
    opClr " = "
    body 2 (map ("| "<+>) ctors)

instance (Pretty (Var p)) => Pretty [Toplevel p] where
  pprint = body 0 . map (<+> opClr " ;; ")

instance (Pretty (Var p)) => Pretty (Constructor p) where
  pprint (UnitCon p _) = pprint p
  pprint (ArgCon p t _) = pprint p <+> kwClr " of " <+> t

instance Pretty (Var Parsed) where
  pprint (Name v) = pprint v

instance Pretty (Var Resolved) where
  pprint (TgName v _) = pprint v
  -- pprint (TgName v i) = pprint v <+> "#" <+> i
  pprint (TgInternal v) = pprint v

instance Pretty (Var Typed) where
  pprint (TvName v) = pprint v
  -- pprint (TvName v t)
    -- | t == internalTyVar = pprint v
    -- | otherwise = parens $ v <+> opClr " : " <+> t
    --
instance Pretty (Span, Type Typed) where
  pprint (x, _) = pprint x

-- }}}

unTvName :: Var Typed -> Var Resolved
unTvName (TvName x) = x

getType :: Data (f Typed) => f Typed -> Type Typed
getType = snd . head . catMaybes . gmapQ get where
  get d = (`asTypeOf` (undefined :: (Span, Type Typed))) <$> cast d
  -- FIXME: Point-freeing this definition makes type inference broken.
  -- Thanks, GHC.

makePrisms ''Expr
makePrisms ''Type


{- Note [1]: Tuple types vs tuple patterns/values

    Tuple types only describe *pairs*, but patterns/values can have any
    number of elements. We do this like Idris, in which (a, b, c) = (a,
    (b, c)) -}

--- vim: fdm=marker
