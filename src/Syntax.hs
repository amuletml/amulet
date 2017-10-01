{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, DeriveFunctor, TypeFamilies, StandaloneDeriving, FlexibleContexts, UndecidableInstances #-}
module Syntax where

import Control.Comonad
import Pretty

import Data.Text (Text)

data ParsePhase = ParsePhase ParsePhase
data TypedPhase = TypedPhase TypedPhase

type family Var a :: *
type instance Var ParsePhase = BoundVar
type instance Var TypedPhase = BoundVar

data Expr p a
  = VarRef (Var p) a
  | Let [(Var p, Expr p a)] (Expr p a) a
  | If (Expr p a) (Expr p a) (Expr p a) a
  | App (Expr p a) (Expr p a) a
  | Fun (Pattern p) (Expr p a) a
  | Begin [Expr p a] a
  | Literal Lit a
  | Match (Expr p a) [(Pattern p, Expr p a)] a
  | BinOp (Expr p a) (Expr p a) (Expr p a) a
  deriving (Functor)

deriving instance (Eq (Var p), Eq a) => Eq (Expr p a)
deriving instance (Show (Var p), Show a) => Show (Expr p a)
deriving instance (Ord (Var p), Ord a) => Ord (Expr p a)

data Pattern p
  = Wildcard
  | Capture (Var p)
  | Destructure (Var p) [Pattern p]
  | PType (Pattern p) (Type p)

deriving instance Eq (Var p) => Eq (Pattern p)
deriving instance Show (Var p) => Show (Pattern p)
deriving instance Ord (Var p) => Ord (Pattern p)

data Lit
  = LiInt Integer
  | LiStr Text
  | LiBool Bool
  | LiUnit
  deriving (Eq, Show, Ord)

data Type p
  = TyCon (Var p)
  | TyVar (Var p)
  | TyForall [Var p] [Type p] (Type p) -- constraints
  | TyArr (Type p) (Type p)
  | TyApp (Type p) (Type p)

deriving instance Eq (Var p) => Eq (Type p)
deriving instance Show (Var p) => Show (Type p)
deriving instance Ord (Var p) => Ord (Type p)

data BoundVar
  = Name Text
  | Refresh BoundVar {-# UNPACK #-} !Int -- for that 1% memory use reduction
  deriving (Eq, Show, Ord)

data Toplevel p a
  = LetStmt [(Var p, Expr p a)]
  | ValStmt (Var p) (Type p)
  | ForeignVal (Var p) Text (Type p)
  | TypeDecl (Var p) [Var p] [(Var p, [Type p])]

deriving instance (Eq (Var p), Eq a) => Eq (Toplevel p a)
deriving instance (Show (Var p), Show a) => Show (Toplevel p a)
deriving instance (Ord (Var p), Ord a) => Ord (Toplevel p a)

data Kind
  = KiType
  | KiArr Kind Kind
  deriving (Eq, Show, Ord)

data Constraint a
  = ConUnify (Expr ParsePhase a) (Type TypedPhase) (Type TypedPhase)

instance (Pretty (Var p)) => Pretty (Expr p a) where
  pprint (VarRef v _) = pprint v
  pprint (Let [] _ _) = error "absurd: never parsed"
  pprint (Let ((n, v):xs) e _) = do
    kwClr "let " <+> n <+> opClr " = " <+> v <+> newline
    forM_ xs $ \(n, v) ->
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

instance (Pretty (Var p)) => Pretty (Pattern p, Expr p a) where
  pprint (a, b) = opClr "| " <+> a <+> " -> " <+> b

instance (Pretty (Var p)) => Pretty (Expr p a, Expr p a) where
  pprint (a, b) = opClr "| " <+> a <+> " -> " <+> b

instance Pretty Kind where
  pprint KiType = kwClr "Type"
  pprint (KiArr a b) = a <+> opClr " -> " <+> b

instance (Pretty (Var p)) => Pretty (Pattern p) where
  pprint Wildcard = kwClr "_"
  pprint (Capture x) = pprint x
  pprint (Destructure x []) = pprint x
  pprint (Destructure x xs) = parens $ x <+> " " <+> interleave " " xs
  pprint (PType p x) = parens $ p <+> opClr " : " <+> x

instance Pretty Lit where
  pprint (LiStr s) = strClr s
  pprint (LiInt s) = litClr s
  pprint (LiBool True) = litClr "true"
  pprint (LiBool False) = litClr "false"
  pprint LiUnit = litClr "unit"

instance (Pretty (Var p)) => Pretty (Type p) where
  pprint (TyCon v) = typeClr v
  pprint (TyVar v) = opClr "'" <+> tvClr v
  pprint (TyForall vs c v) = kwClr "∀ " <+> interleave " " vs <+> opClr ". " <+> parens (interleave "," c) <+> opClr " => " <+> v

  pprint (TyArr x@TyArr{} e) = parens x <+> opClr " -> " <+> e
  pprint (TyArr x e) = x <+> opClr " -> " <+> e

  pprint (TyApp e x@TyApp{}) = parens x <+> opClr " -> " <+> e
  pprint (TyApp x e) = x <+> opClr " " <+> e

instance Pretty BoundVar where
  pprint (Name v) = pprint v
  pprint (Refresh v _) = pprint v

instance Pretty (Constraint a) where
  pprint (ConUnify e a b) = e <+> opClr " <=> " <+> a <+> opClr " ~ " <+> b

instance Comonad (Expr v) where
  extract (VarRef _ p) = p
  extract (Let _ _ p) = p
  extract (If _ _ _ p) = p
  extract (App _ _ p) = p
  extract (Fun _ _ p) = p
  extract (Begin _ p) = p
  extract (Literal _ p) = p
  extract (Match _ _ p) = p
  extract (BinOp _ _ _ p) = p

  extend f e@(VarRef v _) = VarRef v (f e)
  extend f e@(Let b c _) = Let (map (fmap (extend f)) b) (extend f c) (f e)
  extend f e@(If c t b _) = If (extend f c) (extend f t) (extend f b) (f e)
  extend f e@(App l a _) = App (extend f l) (extend f a) (f e)
  extend f e@(Fun p b _) = Fun p (extend f b) (f e)
  extend f e@(Begin es _) = Begin (map (extend f) es) (f e)
  extend f e@(Literal l _) = Literal l (f e)
  extend f e@(Match t ps _) = Match (extend f t) (map (fmap (extend f)) ps) (f e)
  extend f e@(BinOp l o r _) = BinOp (extend f l) (extend f o) (extend f r) (f e)
