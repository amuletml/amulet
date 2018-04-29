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
      Ascription e t a -> Ascription (eR e) (raiseT vR t) (aR a)
      TupleSection es a -> TupleSection (fmap eR <$> es) (aR a)
      OpenIn n e a -> OpenIn (vR n) (eR e) (aR a)
      ExprWrapper w e a -> ExprWrapper (raiseWrapper vR w) (eR e) (aR a)

raiseWrapper :: (Var p -> Var p') -> Wrapper p -> Wrapper p'
raiseWrapper v (Cast co) = Cast (raiseCo v co)
raiseWrapper v (TypeApp ty) = TypeApp (raiseT v ty)
raiseWrapper v (TypeLam (Skolem n u s m) t) = TypeLam (Skolem (v n) (v u) (raiseT v s) (motive m)) (raiseT v t) where
  motive (BySubsumption a b) = BySubsumption (raiseT v a) (raiseT v b)
  motive (ByAscription t) = ByAscription (raiseT v t)
  motive (ByExistential a t) = ByExistential (v a) (raiseT v t)
raiseWrapper v (x :> y) = raiseWrapper v x :> raiseWrapper v y
raiseWrapper v (WrapVar var) = WrapVar (v var)
raiseWrapper _ IdWrap = IdWrap

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
raiseP v a (PType i t p) = PType (raiseP v a i) (raiseT v t) (a p)
raiseP v a (PRecord rs p) = PRecord (map (second (raiseP v a)) rs) (a p)
raiseP v a (PTuple e p) = PTuple (map (raiseP v a) e) (a p)
raiseP _ a (PLiteral l p) = PLiteral l (a p)

raiseT :: (Var p -> Var p') -- How to raise variables
       -> Type p -> Type p'
raiseT v (TyCon n) = TyCon (v n)
raiseT v (TyPromotedCon n) = TyPromotedCon (v n)
raiseT v (TySkol (Skolem n u t m)) = TySkol (Skolem (v n) (v u) (raiseT v t) (motive m)) where
  motive (BySubsumption a b) = BySubsumption (raiseT v a) (raiseT v b)
  motive (ByAscription t) = ByAscription (raiseT v t)
  motive (ByExistential a t) = ByExistential (v a) (raiseT v t)
raiseT v (TyVar n) = TyVar (v n)
raiseT v (TyApp x y) = TyApp (raiseT v x) (raiseT v y)
raiseT v (TyTuple x y) = TyTuple (raiseT v x) (raiseT v y)
raiseT v (TyRows rho rows) = TyRows (raiseT v rho) (map (second (raiseT v)) rows)
raiseT v (TyExactRows rows) = TyExactRows (map (second (raiseT v)) rows)
raiseT v (TyWithConstraints eq a) = TyWithConstraints (map (\(a, b) -> (raiseT v a, raiseT v b)) eq) (raiseT v a)
raiseT v (TyPi binder t) = TyPi (go binder) (raiseT v t) where
  go (Anon t) = Anon (raiseT v t)
  go (Implicit var k) = Implicit (v var) (fmap (raiseT v) k)
raiseT _ TyType = TyType

raiseCo :: (Var p -> Var p') -> Coercion p -> Coercion p'
raiseCo v (VarCo a) = VarCo (v a)
raiseCo v (ReflCo t) = ReflCo (raiseT v t)
raiseCo v (AssumedCo t t') = AssumedCo (raiseT v t) (raiseT v t')
raiseCo v (SymCo x) = SymCo (raiseCo v x)
raiseCo v (AppCo f x) = AppCo (raiseCo v f) (raiseCo v x)
raiseCo v (ArrCo f x) = ArrCo (raiseCo v f) (raiseCo v x)
raiseCo v (ProdCo f x) = ProdCo (raiseCo v f) (raiseCo v x)
raiseCo v (ExactRowsCo rs) = ExactRowsCo (map (second (raiseCo v)) rs)
raiseCo v (RowsCo c rs) = RowsCo (raiseCo v c) (map (second (raiseCo v)) rs)
raiseCo v (ProjCo rs rs') = ProjCo (map (second (raiseT v)) rs) (map (second (raiseCo v)) rs')
raiseCo v (ForallCo va c' c) = ForallCo (v va) (raiseCo v c') (raiseCo v c)
