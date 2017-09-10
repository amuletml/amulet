{-# LANGUAGE FlexibleInstances #-}
module Syntax where

import Pretty

data Expr
  = VarRef Var
  | Let [(Var, Expr)] Expr
  | If Expr Expr Expr
  | App Expr Expr
  | Fun Var Expr
  | Begin [Expr]
  | Literal Lit
  | Match Expr [(Pattern, Expr)]
  deriving (Eq, Show, Ord)

data Pattern
  = Wildcard
  | Capture Var
  deriving (Eq, Show, Ord)

data Lit
  = LiInt Integer
  | LiStr String
  | LiBool Bool
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

data Toplevel
  = LetStmt [(Var, Expr)]
  | ValStmt Var Type
  | ForeignVal Var String Type
  deriving (Eq, Show, Ord)

data Constraint
  = ConEquality Type Type
  | ConInstance String Type
  deriving (Eq, Show, Ord)

instance Pretty Expr where
  pprint (VarRef v) = pprint v
  pprint (Let [] _) = error "absurd: never parsed"
  pprint (Let ((n, v):xs) e) = do
    kwClr "let " <+> n <+> opClr " = " <+> v <+> newline
    forM_ xs $ \(n, v) ->
      kwClr "and " <+> n <+> opClr " = " <+> v <+> newline
    pprint e
  pprint (If c t e) = do
    kwClr "if " <+> c <+> newline
    block 2 $ do
      kwClr "then " <+> t <+> newline
      kwClr "else " <+> e
  pprint (App c e@App{}) = c <+> " " <+> parens e
  pprint (App f x) = f <+> " " <+> x
  pprint (Fun v e) = kwClr "fun " <+> v <+> opClr " -> " <+> e
  pprint (Begin e) = do
    kwClr "begin " <+> newline
    block 2 $ interleave ("; " <+> newline) e
    kwClr "end"
  pprint (Literal l) = pprint l
  pprint (Match t bs) = do
    kwClr "match " <+> t <+> " with" <+> newline
    block 2 $ interleave ("; " <+> newline) bs

instance Pretty (Pattern, Expr) where
  pprint (a, b) = opClr "| " <+> a <+> " -> " <+> b

instance Pretty Pattern where
  pprint Wildcard = kwClr "_"
  pprint (Capture x) = pprint x

instance Pretty Lit where
  pprint (LiStr s) = strClr s
  pprint (LiInt s) = litClr s
  pprint (LiBool True) = litClr "true"
  pprint (LiBool False) = litClr "false"

instance Pretty Type where
  pprint (TyCon v) = typeClr v
  pprint (TyVar v) = tvClr v
  pprint (TyForall vs c v) = kwClr "∀ " <+> interleave " " vs <+> opClr ". " <+> parens (interleave "," c) <+> opClr " => " <+> v

  pprint (TyArr x@TyArr{} e) = parens x <+> opClr " -> " <+> e
  pprint (TyArr x e) = x <+> opClr " -> " <+> e

  pprint (TyApp e x@TyApp{}) = parens x <+> opClr " -> " <+> e
  pprint (TyApp x e) = x <+> opClr " -> " <+> e

instance Pretty Var where
  pprint (Name v) = pprint v
  pprint (Refresh v _) = pprint v

