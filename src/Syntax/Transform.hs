{-# LANGUAGE TypeFamilies #-}
module Syntax.Transform where

import Control.Arrow
import Data.Triple

import Syntax

transformType
  :: (Type p -> Type p)
  -> Type p -> Type p
transformType ft = goT where
  transT (TyCon v) = TyCon v
  transT (TyVar v) = TyVar v
  transT (TyPromotedCon v) = TyPromotedCon v
  transT (TyPi x r) = TyPi (transB x) (goT r)
  transT (TyApp f x) = TyApp (goT f) (goT x)
  transT (TyRows f fs) = TyRows (goT f) (map (second goT) fs)
  transT (TyExactRows fs) = TyExactRows (map (second goT) fs)
  transT (TyTuple l r) = TyTuple (goT l) (goT r)

  transT (TySkol (Skolem i v ty m)) = TySkol (Skolem i v (goT ty) (transM m))
  transT (TyWithConstraints cons ty) = TyWithConstraints (map (goT***goT) cons) (goT ty)
  transT TyType = TyType

  transM (ByAscription ty) = ByAscription (goT ty)
  transM (BySubsumption l r) = BySubsumption (goT l) (goT r)
  transM (ByExistential v ty) = ByExistential v (goT ty)

  transB (Anon t) = Anon (goT t)
  transB (Implicit x k) = Implicit x (fmap goT k)
  transB (Explicit x k) = Explicit x (goT k)

  goT = transT . ft


transformCoercion
  :: (Coercion p -> Coercion p)
  -> (Type p -> Type p)
  -> Coercion p -> Coercion p
transformCoercion fc ft = goC where
  transC (VarCo c) = VarCo c
  transC (ReflCo l) = ReflCo (goT l)
  transC (AssumedCo l r) = AssumedCo (goT l) (goT r)
  transC (AppCo f x) = AppCo (goC f) (goC x)
  transC (ArrCo f x) = ArrCo (goC f) (goC x)
  transC (ProdCo f x) = ProdCo (goC f) (goC x)
  transC (ExactRowsCo rs) = ExactRowsCo (map (second goC) rs)
  transC (RowsCo c rs) = RowsCo (goC c) (map (second goC) rs)
  transC (ProjCo rs rs') = ProjCo (map (second goT) rs) (map (second goC) rs')
  transC (SymCo c) = SymCo (goC c)
  transC (ForallCo v a c) = ForallCo v (goC a) (goC c)

  goT = transformType ft . ft
  goC = transC . fc

transformExpr
  :: (Expr p -> Expr p)
  -> Expr p -> Expr p
transformExpr fe = goE where
  transE (VarRef v a) = VarRef v a
  transE (Let vs r a) = Let (map (second3 goE) vs) (goE r) a
  transE (If c t f a) = If (goE c) (goE t) (goE f) a
  transE (App f x a) = App (goE f) (goE x) a
  transE (Fun p b a) = Fun p (goE b) a
  transE (Begin es a) = Begin (map goE es) a
  transE (Literal l a) = Literal l a
  transE (Match e bs a) = Match (goE e) (map (second goE) bs) a
  transE (Function bs a) = Function (map (second goE) bs) a
  transE (BinOp l o r a) = BinOp (goE l) (goE o) (goE r) a
  transE (Hole v a) = Hole v a
  transE (Ascription e t a) = Ascription (goE e) t a

  transE (Record fs a) = Record (map (second goE) fs) a
  transE (RecordExt f fs a) = RecordExt (goE f) (map (second goE) fs) a
  transE (Access e f a) = Access (goE e) f a

  transE (LeftSection o r a) = LeftSection (goE o) (goE r) a
  transE (RightSection l o a) = RightSection (goE l) (goE o) a
  transE (BothSection o a) = BothSection (goE o) a
  transE (AccessSection t a) = AccessSection t a
  transE (Parens e a) = Parens (goE e) a

  transE (Tuple es a) = Tuple (map goE es) a
  transE (TupleSection es a) = TupleSection (map (goE<$>) es) a

  transE (OpenIn n e a) = OpenIn n (goE e) a

  transE (InstType t a) = InstType t a
  transE (InstHole a) = InstHole a

  transE (ExprWrapper w e a) = ExprWrapper w (goE e) a

  goE = transE . fe

transformExprTyped
  :: (Expr Typed -> Expr Typed)
  -> (Coercion Typed -> Coercion Typed)
  -> (Type Typed -> Type Typed)
  -> Expr Typed -> Expr Typed
transformExprTyped fe fc ft = goE where
  transE (VarRef v a) = VarRef v (goA a)
  transE (Let vs r a) = Let (map (trimap id goE goA) vs) (goE r) (goA a)
  transE (If c t f a) = If (goE c) (goE t) (goE f) (goA a)
  transE (App f x a) = App (goE f) (goE x) (goA a)
  transE (Fun p b a) = Fun (goP p) (goE b) (goA a)
  transE (Begin es a) = Begin (map goE es) (goA a)
  transE (Literal l a) = Literal l (goA a)
  transE (Match e bs a) = Match (goE e) (map (goP *** goE) bs) (goA a)
  transE (Function bs a) = Function (map (second goE) bs) a
  transE (BinOp l o r a) = BinOp (goE l) (goE o) (goE r) (goA a)
  transE (Hole v a) = Hole v (goA a)
  transE (Ascription e t a) = Ascription (goE e) (goT t) (goA a)

  transE (Record fs a) = Record (map (second goE) fs) (goA a)
  transE (RecordExt f fs a) = RecordExt (goE f) (map (second goE) fs) (goA a)
  transE (Access e f a) = Access (goE e) f (goA a)

  transE (LeftSection o r a) = LeftSection (goE o) (goE r) (goA a)
  transE (RightSection l o a) = RightSection (goE l) (goE o) (goA a)
  transE (BothSection o a) = BothSection (goE o) (goA a)
  transE (AccessSection t a) = AccessSection t (goA a)
  transE (Parens e a) = Parens (goE e) (goA a)

  transE (Tuple es a) = Tuple (map goE es) (goA a)
  transE (TupleSection es a) = TupleSection (map (goE<$>) es) (goA a)

  transE (OpenIn n e a) = OpenIn n (goE e) (goA a)
  transE (InstType t a) = InstType (goT t) (goA a)
  transE (InstHole a) = InstHole (goA a)

  transE (ExprWrapper w e a) = ExprWrapper (goW w) (goE e) (goA a)

  goW (Cast c) = Cast (goC c)
  goW (TypeApp t) = TypeApp (goT t)
  goW (WrapFn f) = WrapFn f
  goW (x :> y) = goW x :> goW y
  goW x@TypeLam{} = x
  goW x@WrapVar{} = x
  goW IdWrap = IdWrap

  goE = transE . fe
  goT = ft
  goC = transformCoercion fc ft
  goA (s, ty) = (s, goT ty)

  goP = transformPatternTyped id ft

transformPatternTyped
  :: (Pattern Typed -> Pattern Typed)
  -> (Type Typed -> Type Typed)
  -> Pattern Typed -> Pattern Typed
transformPatternTyped fp ft = goP where
  transP (Wildcard a) = Wildcard (goA a)
  transP (Capture v a) = Capture v (goA a)
  transP (Destructure v p a) = Destructure v (goP <$> p) (goA a)
  transP (PType p t a) = PType (goP p) (goT t) (goA a)
  transP (PRecord fs a) = PRecord (map (second goP) fs) (goA a)
  transP (PTuple ps a) = PTuple (map goP ps) (goA a)
  transP (PLiteral l a) = PLiteral l (goA a)
  transP (PWrapper c p a) = PWrapper c (goP p) (goA a)

  goA (s, ty) = (s, goT ty)
  goT = ft
  goP = transP . fp

correct :: Type Typed -> Expr Typed -> Expr Typed
correct ty (VarRef v a) = VarRef v (fst a, ty)
correct ty (Let vs r a) = Let vs r (fst a, ty)
correct ty (If c t f a) = If c t f (fst a, ty)
correct ty (App f x a) = App f x (fst a, ty)
correct ty (Fun p b a) = Fun p b (fst a, ty)
correct ty (Begin es a) = Begin es (fst a, ty)
correct ty (Literal l a) = Literal l (fst a, ty)
correct ty (Match e bs a) = Match e bs (fst a, ty)
correct ty (Function bs a) = Function bs (fst a, ty)
correct ty (BinOp l o r a) = BinOp l o r (fst a, ty)
correct ty (Hole v a) = Hole v (fst a, ty)
correct ty (Ascription e t a) = Ascription e t (fst a, ty)

correct ty (Record fs a) = Record fs (fst a, ty)
correct ty (RecordExt f fs a) = RecordExt f fs (fst a, ty)
correct ty (Access e f a) = Access e f (fst a, ty)

correct ty (LeftSection o r a) = LeftSection o r (fst a, ty)
correct ty (RightSection l o a) = RightSection l o (fst a, ty)
correct ty (BothSection o a) = BothSection o (fst a, ty)
correct ty (AccessSection t a) = AccessSection t (fst a, ty)
correct ty (Parens e a) = Parens e (fst a, ty)

correct ty (Tuple es a) = Tuple es (fst a, ty)
correct ty (TupleSection es a) = TupleSection es (fst a, ty)

correct ty (OpenIn n e a) = OpenIn n e (fst a, ty)
correct ty (InstType t a) = InstType t (fst a, ty)
correct ty (InstHole a) = InstHole (fst a, ty)

correct ty (ExprWrapper w e a) = ExprWrapper w e (fst a, ty)
