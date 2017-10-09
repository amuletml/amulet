{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies, DataKinds #-}
{-# LANGUAGE LambdaCase #-}
module Syntax where

import Pretty

import Data.Text (Text, pack)
import Data.Span

import Control.Arrow ((***))

data Phase = ParsePhase | TypedPhase

type family Var (a :: Phase) :: * where
  Var 'ParsePhase = BoundVar
  Var 'TypedPhase = TypedVar

type family Ann (a :: Phase) :: * where
  Ann 'ParsePhase = Span
  Ann 'TypedPhase = Span

data Expr p
  = VarRef (Var p) (Ann p)
  | Let [(Var p, Expr p)] (Expr p) (Ann p)
  | If (Expr p) (Expr p) (Expr p) (Ann p)
  | App (Expr p) (Expr p) (Ann p)
  | Fun (Pattern p) (Expr p) (Ann p)
  | Begin [Expr p] (Ann p)
  | Literal Lit (Ann p)
  | Match (Expr p) [(Pattern p, Expr p)] (Ann p)
  | BinOp (Expr p) (Expr p) (Expr p) (Ann p)

deriving instance (Eq (Var p), Eq (Ann p)) => Eq (Expr p)
deriving instance (Show (Var p), Show (Ann p)) => Show (Expr p)
deriving instance (Ord (Var p), Ord (Ann p)) => Ord (Expr p)

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
  | TyStar -- * :: *

deriving instance Eq (Var p) => Eq (Type p)
deriving instance Show (Var p) => Show (Type p)
deriving instance Ord (Var p) => Ord (Type p)

data BoundVar
  = Name Text
  | Refresh BoundVar {-# UNPACK #-} !Int
  deriving (Eq, Show, Ord)

data TypedVar
  = TvName Text (Type 'TypedPhase)
  | TvRefresh TypedVar {-# UNPACK #-} !Int
  deriving (Eq, Show, Ord)

data Toplevel p
  = LetStmt [(Var p, Expr p)]
  | ValStmt (Var p) (Type p)
  | ForeignVal (Var p) Text (Type p)
  | TypeDecl (Var p) [Var p] [(Var p, [Type p])]

deriving instance (Eq (Var p), Eq (Ann p)) => Eq (Toplevel p)
deriving instance (Show (Var p), Show (Ann p)) => Show (Toplevel p)
deriving instance (Ord (Var p), Ord (Ann p)) => Ord (Toplevel p)

--- Pretty-printing {{{

instance (Pretty (Var p)) => Pretty (Expr p) where
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

instance (Pretty (Var p)) => Pretty (Pattern p, Expr p) where
  pprint (a, b) = opClr "| " <+> a <+> " -> " <+> b

instance (Pretty (Var p)) => Pretty (Expr p, Expr p) where
  pprint (a, b) = opClr "| " <+> a <+> " -> " <+> b

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
  pprint TyStar = kwClr "Type"

instance Pretty BoundVar where
  pprint (Name v) = pprint v
  pprint (Refresh v _) = pprint v

instance Pretty TypedVar where
  pprint (TvName v t)
    | t == internalTyVar = pprint v
    | t == TyStar = pprint v
    | otherwise = parens $ v <+> opClr " : " <+> t
  pprint (TvRefresh v _) = pprint v

--- }}}

extract :: Expr p -> Ann p
extract (VarRef _ p) = p
extract (Let _ _ p) = p
extract (If _ _ _ p) = p
extract (App _ _ p) = p
extract (Fun _ _ p) = p
extract (Begin _ p) = p
extract (Literal _ p) = p
extract (Match _ _ p) = p
extract (BinOp _ _ _ p) = p

--- Raising {{{

-- Raise an expression across phases
raiseE :: (Var p -> Var p') -- How to raise variables
      -> (Ann p -> Ann p') -- How to raise annotations
      -> Expr p -> Expr p'
raiseE vR aR =
  let eR = raiseE vR aR
   in \case
      VarRef k a -> VarRef (vR k) (aR a)
      Let bs b a -> Let (map (vR *** eR) bs) (eR b) (aR a)
      If a b c ann -> If (eR a) (eR b) (eR c) (aR ann)
      App a b ann -> App (eR a) (eR b) (aR ann)
      Fun p b a -> Fun (raiseP vR p) (eR b) (aR a)
      Begin bs an -> Begin (map eR bs) (aR an)
      Literal l a -> Literal l (aR a)
      Match e cs a -> Match (eR e) (map (raiseP vR *** eR) cs) (aR a)
      BinOp a b c an -> BinOp (eR a) (eR b) (eR c) (aR an)

raiseP :: (Var p -> Var p') -- How to raise variables
       -> Pattern p -> Pattern p'
raiseP _ Wildcard = Wildcard
raiseP r (Capture v) = Capture (r v)
raiseP r (Destructure v s) = Destructure (r v) (map (raiseP r) s)
raiseP r (PType p t) = PType (raiseP r p) (raiseT r t)

raiseT :: (Var p -> Var p') -- How to raise variables
       -> Type p -> Type p'
raiseT r (TyCon v) = TyCon (r v)
raiseT r (TyVar v) = TyVar (r v)
raiseT r (TyForall v c t) = TyForall (map r v)
                                     (map (raiseT r) c)
                                     (raiseT r t)
raiseT r (TyArr a b) = TyArr (raiseT r a) (raiseT r b)
raiseT r (TyApp a b) = TyApp (raiseT r a) (raiseT r b)
raiseT _ TyStar = TyStar

--- }}}

class InternalTV (p :: Phase) where
  internalTyVar :: Type p

instance InternalTV 'ParsePhase where
  internalTyVar = TyVar (Name (pack "«internal»"))

instance InternalTV 'TypedPhase where
  internalTyVar = TyVar (TvName (pack "«internal»") TyStar)

eraseVarTy :: Var 'TypedPhase -> Var 'ParsePhase
eraseVarTy (TvName x _) = Name x
eraseVarTy (TvRefresh k _) = eraseVarTy k

--- vim: fdm=marker
