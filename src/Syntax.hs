{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, DeriveFunctor #-}
module Syntax where

import Control.Comonad
import Pretty

data Expr a
  = VarRef Var a
  | Let [(Var, Expr a)] (Expr a) a
  | If (Expr a) (Expr a) (Expr a) a
  | App (Expr a) (Expr a) a
  | Fun Pattern (Expr a) a
  | Begin [Expr a] a
  | Literal Lit a
  | Match (Expr a) [(Pattern, Expr a)] a
  | BinOp (Expr a) (Expr a) (Expr a) a
  deriving (Eq, Show, Ord, Functor)

data Pattern
  = Wildcard
  | Capture Var
  | Destructure Var [Pattern]
  | PType Pattern Type
  deriving (Eq, Show, Ord)

data Lit
  = LiInt Integer
  | LiStr String
  | LiBool Bool
  | LiUnit
  deriving (Eq, Show, Ord)

data Type
  = TyCon Var
  | TyVar String
  | TyForall [String] [Type] Type -- constraints
  | TyArr Type Type
  | TyApp Type Type
  deriving (Eq, Show, Ord)

data Var
  = Name String
  | Refresh Var String
  deriving (Eq, Show, Ord)

data Toplevel a
  = LetStmt [(Var, Expr a)]
  | ValStmt Var Type
  | ForeignVal Var String Type
  | TypeDecl Var [String] [(Var, [Type])]
  deriving (Eq, Show, Ord)

data Kind
  = KiType
  | KiArr Kind Kind
  deriving (Eq, Show, Ord)

data Constraint a
  = ConUnify (Expr a) Type Type
  deriving (Eq, Show, Ord, Functor)

instance Pretty (Expr a) where
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

instance Pretty (Pattern, Expr a) where
  pprint (a, b) = opClr "| " <+> a <+> " -> " <+> b

instance Pretty (Expr a, Expr a) where
  pprint (a, b) = opClr "| " <+> a <+> " -> " <+> b

instance Pretty Kind where
  pprint KiType = kwClr "Type"
  pprint (KiArr a b) = a <+> opClr " -> " <+> b

instance Pretty Pattern where
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

instance Pretty Type where
  pprint (TyCon v) = typeClr v
  pprint (TyVar v) = opClr "'" <+> tvClr v
  pprint (TyForall vs c v) = kwClr "∀ " <+> interleave " " vs <+> opClr ". " <+> parens (interleave "," c) <+> opClr " => " <+> v

  pprint (TyArr x@TyArr{} e) = parens x <+> opClr " -> " <+> e
  pprint (TyArr x e) = x <+> opClr " -> " <+> e

  pprint (TyApp e x@TyApp{}) = parens x <+> opClr " -> " <+> e
  pprint (TyApp x e) = x <+> opClr " " <+> e

instance Pretty Var where
  pprint (Name v) = pprint v
  pprint (Refresh v _) = pprint v

instance Pretty (Constraint a) where
  pprint (ConUnify e a b) = e <+> opClr " <=> " <+> a <+> opClr " ~ " <+> b

instance Comonad Expr where
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

