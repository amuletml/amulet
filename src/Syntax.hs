{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies, DataKinds #-}
{-# LANGUAGE LambdaCase, MultiParamTypeClasses, FunctionalDependencies #-}
module Syntax where

import Pretty

import Data.Text (Text, pack)
import Data.Span

import Control.Arrow ((***))

data Parsed 
data Typed

data family Var a

data instance Var Parsed
  = Name Text
  | Refresh (Var Parsed) {-# UNPACK #-} !Int
  deriving (Eq, Show, Ord)

data instance Var Typed
  = TvName Text (Type Typed)
  | TvRefresh (Var Typed) {-# UNPACK #-} !Int
  deriving (Eq, Show, Ord)

type family Ann a :: * where
  Ann Parsed = Span
  Ann Typed = Span

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
  | Hole (Var p) (Ann p)

deriving instance (Eq (Var p), Eq (Ann p)) => Eq (Expr p)
deriving instance (Show (Var p), Show (Ann p)) => Show (Expr p)
deriving instance (Ord (Var p), Ord (Ann p)) => Ord (Expr p)

data Pattern p
  = Wildcard (Ann p)
  | Capture (Var p) (Ann p)
  | Destructure (Var p) [Pattern p] (Ann p)
  | PType (Pattern p) (Type p) (Ann p)

deriving instance (Eq (Var p), Eq (Ann p)) => Eq (Pattern p)
deriving instance (Show (Var p), Show (Ann p)) => Show (Pattern p)
deriving instance (Ord (Var p), Ord (Ann p)) => Ord (Pattern p)

data Lit
  = LiInt Integer
  | LiStr Text
  | LiBool Bool
  | LiUnit
  deriving (Eq, Show, Ord)

data Type p
  = TyCon (Var p) (Ann p)
  | TyVar (Var p) (Ann p)
  | TyForall [Var p] [Type p] (Type p) (Ann p) -- constraints
  | TyArr (Type p) (Type p) (Ann p)
  | TyApp (Type p) (Type p) (Ann p)
  | TyStar (Ann p) -- * :: *

deriving instance (Eq (Var p), Eq (Ann p)) => Eq (Type p)
deriving instance (Show (Var p), Show (Ann p)) => Show (Type p)
deriving instance (Ord (Var p), Ord (Ann p)) => Ord (Type p)

data Toplevel p
  = LetStmt [(Var p, Expr p)] (Ann p)
  | ValStmt (Var p) (Type p) (Ann p)
  | ForeignVal (Var p) Text (Type p) (Ann p)
  | TypeDecl (Var p) [Var p] [(Var p, [Type p])] (Ann p)

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
  pprint (Hole v _) = pprint v -- A typed hole

instance (Pretty (Var p)) => Pretty (Pattern p, Expr p) where
  pprint (a, b) = opClr "| " <+> a <+> " -> " <+> b

instance (Pretty (Var p)) => Pretty (Expr p, Expr p) where
  pprint (a, b) = opClr "| " <+> a <+> " -> " <+> b

instance (Pretty (Var p)) => Pretty (Pattern p) where
  pprint Wildcard{} = kwClr "_"
  pprint (Capture x _) = pprint x
  pprint (Destructure x [] _) = pprint x
  pprint (Destructure x xs _) = parens $ x <+> " " <+> interleave " " xs
  pprint (PType p x _) = parens $ p <+> opClr " : " <+> x

instance Pretty Lit where
  pprint (LiStr s) = strClr s
  pprint (LiInt s) = litClr s
  pprint (LiBool True) = litClr "true"
  pprint (LiBool False) = litClr "false"
  pprint LiUnit = litClr "unit"

instance (Pretty (Var p)) => Pretty (Type p) where
  pprint (TyCon v _) = typeClr v
  pprint (TyVar v _) = opClr "'" <+> v
  pprint (TyForall vs c v _) = kwClr "∀ " <+> interleave " " vs <+> opClr ". " <+> parens (interleave "," c) <+> opClr " => " <+> v

  pprint (TyArr x@TyArr{} e _) = parens x <+> opClr " -> " <+> e
  pprint (TyArr x e _) = x <+> opClr " -> " <+> e

  pprint (TyApp e x@TyApp{} _) = e <+> " " <+> parens x
  pprint (TyApp x e _) = x <+> opClr " " <+> e
  pprint TyStar{} = kwClr "Type"

instance Pretty (Var Parsed) where
  pprint (Name v) = pprint v
  pprint (Refresh v _) = pprint v

instance Pretty (Var Typed) where
  pprint (TvName v (TyStar _)) = pprint v
  pprint (TvName v t)
    | t == internalTyVar = pprint v
    | otherwise = parens $ v <+> opClr " : " <+> t
  pprint (TvRefresh v _) = pprint v

--- }}}

class Annotated f where
  annotation :: f p -> Ann p

instance Annotated Expr where
  annotation (VarRef _ p) = p
  annotation (Hole _ p) = p
  annotation (Let _ _ p) = p
  annotation (If _ _ _ p) = p
  annotation (App _ _ p) = p
  annotation (Fun _ _ p) = p
  annotation (Begin _ p) = p
  annotation (Literal _ p) = p
  annotation (Match _ _ p) = p
  annotation (BinOp _ _ _ p) = p

instance Annotated Type where
  annotation (TyCon _ p) = p
  annotation (TyVar _ p) = p
  annotation (TyForall _ _ _ p) = p
  annotation (TyArr _ _ p) = p
  annotation (TyApp _ _ p) = p
  annotation (TyStar p) = p

instance Annotated Pattern where
  annotation (Wildcard p) = p
  annotation (Capture _ p) = p
  annotation (Destructure _ _ p) = p
  annotation (PType _ _ p) = p

--- Raising {{{

-- Raise an expression across phases
raiseE :: (Var p -> Var p') -- How to raise variables
       -> (Ann p -> Ann p') -- How to raise annotations
       -> Expr p -> Expr p'
raiseE vR aR =
  let eR = raiseE vR aR
   in \case
      VarRef k a -> VarRef (vR k) (aR a)
      Hole k a -> Hole (vR k) (aR a)
      Let bs b a -> Let (map (vR *** eR) bs) (eR b) (aR a)
      If a b c ann -> If (eR a) (eR b) (eR c) (aR ann)
      App a b ann -> App (eR a) (eR b) (aR ann)
      Fun p b a -> Fun (raiseP vR aR p) (eR b) (aR a)
      Begin bs an -> Begin (map eR bs) (aR an)
      Literal l a -> Literal l (aR a)
      Match e cs a -> Match (eR e) (map (raiseP vR aR *** eR) cs) (aR a)
      BinOp a b c an -> BinOp (eR a) (eR b) (eR c) (aR an)

raiseP :: (Var p -> Var p') -- How to raise variables
       -> (Ann p -> Ann p')
       -> Pattern p -> Pattern p'
raiseP _ a (Wildcard p) = Wildcard (a p)
raiseP v a (Capture n p) = Capture (v n) (a p)
raiseP v a (Destructure c s p) = Destructure (v c) (map (raiseP v a) s) (a p)
raiseP v a (PType i t p) = PType (raiseP v a i) (raiseT v a t) (a p)

raiseT :: (Var p -> Var p') -- How to raise variables
       -> (Ann p -> Ann p')
       -> Type p -> Type p'
raiseT v a (TyCon n p) = TyCon (v n) (a p)
raiseT v a (TyVar n p) = TyVar (v n) (a p)
raiseT v a (TyForall n c t p) = TyForall (map v n)
                                     (map (raiseT v a) c)
                                     (raiseT v a t)
                                     (a p)
raiseT v a (TyArr x y p) = TyArr (raiseT v a x) (raiseT v a y) (a p)
raiseT v a (TyApp x y p) = TyApp (raiseT v a x) (raiseT v a y) (a p)
raiseT _ a (TyStar p) = TyStar (a p)

--- }}}

class InternalTV p where
  internalTyVar :: Type p

instance InternalTV Parsed where
  internalTyVar = TyVar (Name (pack "«internal»")) internal

instance InternalTV Typed where
  internalTyVar = TyVar (TvName (pack "«internal»") (TyStar internal)) internal

eraseVarTy :: Var Typed -> Var Parsed
eraseVarTy (TvName x _) = Name x
eraseVarTy (TvRefresh k _) = eraseVarTy k

--- vim: fdm=marker
