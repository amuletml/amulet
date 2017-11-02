{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}
module Syntax.Raise (raiseE, raiseP, raiseT) where

import Control.Arrow
import Syntax

raiseE :: forall p p'.
          (Var p -> Var p') -- How to raise variables
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
      Record rows ann -> Record (map (second eR) rows) (aR ann)
      RecordExt x rows ann -> RecordExt (eR x) (map (second eR) rows) (aR ann)
      Access ex v ann -> Access (eR ex) v (aR ann)
      LeftSection o v a -> LeftSection (eR o) (eR v) (aR a)
      RightSection o v a -> LeftSection (eR o) (eR v) (aR a)
      BothSection o a -> BothSection (eR o) (aR a)
      AccessSection k a -> AccessSection k (aR a)
      Tuple es a -> Tuple (map eR es) (aR a)
      EHasType e t a -> EHasType (eR e) (raiseT vR aR t) (aR a)

raiseP :: (Var p -> Var p') -- How to raise variables
       -> (Ann p -> Ann p')
       -> Pattern p -> Pattern p'
raiseP _ a (Wildcard p) = Wildcard (a p)
raiseP v a (Capture n p) = Capture (v n) (a p)
raiseP v a (Destructure c s' p)
  | Nothing <- s'
  = Destructure (v c) Nothing (a p)
  | Just s <- s'
  = Destructure (v c) (Just (raiseP v a s)) (a p)
raiseP v a (PType i t p) = PType (raiseP v a i) (raiseT v a t) (a p)
raiseP v a (PRecord rs p) = PRecord (map (second (raiseP v a)) rs) (a p)
raiseP v a (PTuple e p) = PTuple (map (raiseP v a) e) (a p)

raiseT :: (Var p -> Var p') -- How to raise variables
       -> (Ann p -> Ann p')
       -> Type p -> Type p'
raiseT v a (TyCon n p) = TyCon (v n) (a p)
raiseT v a (TyVar n p) = TyVar (v n) (a p)
raiseT v a (TyForall n t p) = TyForall (map v n)
                                       (raiseT v a t)
                                       (a p)
raiseT v a (TyArr x y p) = TyArr (raiseT v a x) (raiseT v a y) (a p)
raiseT v a (TyApp x y p) = TyApp (raiseT v a x) (raiseT v a y) (a p)
raiseT v a (TyTuple x y p) = TyTuple (raiseT v a x) (raiseT v a y) (a p)
raiseT v a (TyRows rho rows p) = TyRows (raiseT v a rho) (map (second (raiseT v a)) rows) (a p)
raiseT v a (TyExactRows rows p) = TyExactRows (map (second (raiseT v a)) rows) (a p)
raiseT _ a (TyStar p) = TyStar (a p)
raiseT v a (TyCons cs n p) = TyCons (map raiseCon cs) (raiseT v a n) (a p) where
  raiseCon (Equal l r ann) = Equal (raiseT v a l) (raiseT v a r) (a ann)
