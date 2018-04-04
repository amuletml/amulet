{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}
module Syntax.Raise (raiseE, raiseP, raiseT, raiseCo) where

import Control.Arrow
import Syntax

raiseE :: forall p p'.
          (Var p -> Var p') -- How to raise variables
       -> (Ann p -> Ann p') -- How to raise annotations
       -> Expr p -> Expr p'
raiseE vR aR =
  let eR = raiseE vR aR
      rv (a, b, c) = (vR a, eR b, aR c)
   in \case
      VarRef k a -> VarRef (vR k) (aR a)
      Hole k a -> Hole (vR k) (aR a)
      Let bs b a -> Let (map rv bs) (eR b) (aR a)
      If a b c ann -> If (eR a) (eR b) (eR c) (aR ann)
      App a b ann -> App (eR a) (eR b) (aR ann)
      Fun p b a -> Fun (raiseP vR aR p) (eR b) (aR a)
      Begin bs an -> Begin (map eR bs) (aR an)
      Literal l a -> Literal l (aR a)
      Match e cs a -> Match (eR e) (map (raiseP vR aR *** eR) cs) (aR a)
      Function bs a -> Function (map (raiseP vR aR *** eR) bs) (aR a)
      BinOp a b c an -> BinOp (eR a) (eR b) (eR c) (aR an)
      Record rows ann -> Record (map (second eR) rows) (aR ann)
      RecordExt x rows ann -> RecordExt (eR x) (map (second eR) rows) (aR ann)
      Access ex v ann -> Access (eR ex) v (aR ann)
      LeftSection o v a -> LeftSection (eR o) (eR v) (aR a)
      RightSection o v a -> LeftSection (eR o) (eR v) (aR a)
      BothSection o a -> BothSection (eR o) (aR a)
      AccessSection k a -> AccessSection k (aR a)
      Parens e a -> Parens (eR e) (aR a)
      Tuple es a -> Tuple (map eR es) (aR a)
      Ascription e t a -> Ascription (eR e) (raiseT vR aR t) (aR a)
      TypeApp f x a -> TypeApp (eR f) (raiseT vR aR x) (aR a)
      TupleSection es a -> TupleSection (fmap eR <$> es) (aR a)
      Cast e c a -> Cast (eR e) (raiseCo vR aR c) (aR a)

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
raiseP _ a (PLiteral l p) = PLiteral l (a p)

raiseT :: (Var p -> Var p') -- How to raise variables
       -> (Ann p -> Ann p')
       -> Type p -> Type p'
raiseT v _ (TyCon n) = TyCon (v n)
raiseT v f (TySkol (Skolem n u t m)) = TySkol (Skolem (v n) (v u) (raiseT v f t) (motive m)) where
  motive (BySubsumption a b) = BySubsumption (raiseT v f a) (raiseT v f b)
  motive (ByAscription t) = ByAscription (raiseT v f t)
  motive (ByExistential a t) = ByExistential (v a) (raiseT v f t)
raiseT v a (TyVar n) = TyVar (v n)
raiseT v a (TyApp x y) = TyApp (raiseT v a x) (raiseT v a y)
raiseT v a (TyTuple x y) = TyTuple (raiseT v a x) (raiseT v a y)
raiseT v a (TyRows rho rows) = TyRows (raiseT v a rho) (map (second (raiseT v a)) rows)
raiseT v a (TyExactRows rows) = TyExactRows (map (second (raiseT v a)) rows)
raiseT v f (TyWithConstraints eq a) = TyWithConstraints (map (\(a, b) -> (raiseT v f a, raiseT v f b)) eq) (raiseT v f a)
raiseT v a (TyPi binder t) = TyPi (go binder) (raiseT v a t) where
  go (Anon t) = Anon (raiseT v a t)
  go (Implicit var k) = Implicit (v var) (fmap (raiseT v a) k)
  go (Dependent var k) = Dependent (v var) (raiseT v a k)
raiseT _ _ TyType = TyType
raiseT v x (TyTerm t) = TyTerm (raiseE v x t)

raiseCo :: (Var p -> Var p') -> (Ann p -> Ann p') -> Coercion p -> Coercion p'
raiseCo v _ (VarCo a) = VarCo (v a)
raiseCo v a (ReflCo t) = ReflCo (raiseT v a t)
raiseCo v a (AssumedCo t t') = AssumedCo (raiseT v a t) (raiseT v a t')
raiseCo v a (SymCo x) = SymCo (raiseCo v a x)
raiseCo v a (AppCo f x) = AppCo (raiseCo v a f) (raiseCo v a x)
raiseCo v a (ArrCo f x) = ArrCo (raiseCo v a f) (raiseCo v a x)
raiseCo v a (ProdCo f x) = ProdCo (raiseCo v a f) (raiseCo v a x)
raiseCo v a (ExactRowsCo rs) = ExactRowsCo (map (second (raiseCo v a)) rs)
raiseCo v a (RowsCo c rs) = RowsCo (raiseCo v a c) (map (second (raiseCo v a)) rs)
raiseCo v a (ForallCo va c) = ForallCo (v va) (raiseCo v a c)
