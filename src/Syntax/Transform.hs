{-# LANGUAGE TypeFamilies #-}
module Syntax.Transform where

import Control.Arrow
import Control.Lens hiding (Lazy, (:>))

import Syntax.Var
import Syntax.Expr
import Syntax.Type

transformType
  :: (Type p -> Type p)
  -> Type p -> Type p
transformType ft = goT where
  transT (TyCon v) = TyCon v
  transT (TyLit v) = TyLit v
  transT (TyVar v) = TyVar v
  transT (TyWildcard v) = TyWildcard v
  transT (TyPromotedCon v) = TyPromotedCon v
  transT (TyPi x r) = TyPi (transB x) (goT r)
  transT (TyApp f x) = TyApp (goT f) (goT x)
  transT (TyRows f fs) = TyRows (goT f) (map (second goT) fs)
  transT (TyExactRows fs) = TyExactRows (map (second goT) fs)
  transT (TyTuple l r) = TyTuple (goT l) (goT r)
  transT (TyTupleL l r) = TyTupleL (goT l) (goT r)
  transT (TyOperator l o r) = TyOperator (goT l) o (goT r)
  transT (TyParens t) = TyParens (goT t)

  transT (TySkol (Skolem i v ty m)) = TySkol (Skolem i v (goT ty) (transM m))
  transT (TyWithConstraints cons ty) = TyWithConstraints (map (goT***goT) cons) (goT ty)
  transT TyType = TyType

  transM (ByAscription ex ty) = ByAscription ex (goT ty)
  transM (BySubsumption l r) = BySubsumption (goT l) (goT r)
  transM (ByExistential v ty) = ByExistential v (goT ty)
  transM (ByConstraint ty) = ByConstraint (goT ty)
  transM (ByInstanceHead ty a) = ByInstanceHead (goT ty) a
  transM (ByTyFunLhs ty a) = ByTyFunLhs (goT ty) a

  transB (Anon t) = Anon (goT t)
  transB (Implicit t) = Implicit (goT t)
  transB (Invisible x k spec) = Invisible x (fmap goT k) spec

  goT = transT . ft


transformCoercion
  :: (Coercion p -> Coercion p)
  -> (Type p -> Type p)
  -> Coercion p -> Coercion p
transformCoercion fc ft = goC where
  transC (VarCo c) = VarCo c
  transC (P1 c) = P1 c
  transC (P2 c) = P2 c
  transC (MvCo c) = P2 c
  transC (ReflCo l) = ReflCo (goT l)
  transC (TransCo x y) = TransCo (goC x) (goC y)
  transC (AssumedCo l r) = AssumedCo (goT l) (goT r)
  transC (AppCo f x) = AppCo (goC f) (goC x)
  transC (ArrCo f x) = ArrCo (goC f) (goC x)
  transC (ProdCo f x) = ProdCo (goC f) (goC x)
  transC (ExactRowsCo rs) = ExactRowsCo (map (second goC) rs)
  transC (RowsCo c rs) = RowsCo (goC c) (map (second goC) rs)
  transC (ProjCo rs rs') = ProjCo (map (second goT) rs) (map (second goC) rs')
  transC (SymCo c) = SymCo (goC c)
  transC (ForallCo v vis a c) = ForallCo v vis (goC a) (goC c)
  transC (InstCo ax ts) = InstCo ax (map goC ts)

  goT = transformType ft . ft
  goC = transC . fc

transformExpr
  :: (Expr p -> Expr p)
  -> Expr p -> Expr p
transformExpr fe = goE where
  transE (VarRef v a) = VarRef v a
  transE (Let re vs r a) = Let re (map goB vs) (goE r) a
  transE (If c t f a) = If (goE c) (goE t) (goE f) a
  transE (App f x a) = App (goE f) (goE x) a
  transE (Fun p b a) = Fun p (goE b) a
  transE (Begin es a) = Begin (map goE es) a
  transE (Literal l a) = Literal l a
  transE (Match e bs a) = Match (goE e) (map goArm bs) a
  transE (Function bs a) = Function (map goArm bs) a
  transE (BinOp l o r a) = BinOp (goE l) (goE o) (goE r) a
  transE (Hole v a) = Hole v a
  transE (Ascription e t a) = Ascription (goE e) t a

  transE (Record fs a) = Record (map (over fExpr goE) fs) a
  transE (RecordExt f fs a) = RecordExt (goE f) (map (over fExpr goE) fs) a
  transE (Access e f a) = Access (goE e) f a

  transE (LeftSection o r a) = LeftSection (goE o) (goE r) a
  transE (RightSection l o a) = RightSection (goE l) (goE o) a
  transE (BothSection o a) = BothSection (goE o) a
  transE (AccessSection t a) = AccessSection t a
  transE (Parens e a) = Parens (goE e) a

  transE (Tuple es a) = Tuple (map goE es) a
  transE (TupleSection es a) = TupleSection (map (goE<$>) es) a

  transE (OpenIn n e a) = OpenIn n (goE e) a

  transE (ExprWrapper w e a) = ExprWrapper w (goE e) a
  transE (Lazy e a) = Lazy (goE e) a
  transE (Vta e t a) = Vta (goE e) t a
  transE (ListExp e a) = ListExp (map goE e) a
  transE (ListComp e qs a) = ListComp (goE e) (map goQ qs) a
  transE (DoExpr v qs a) = DoExpr v (map goQ qs) a
  transE (Idiom vp va es a) = Idiom vp va (goE es) a
  transE (ListFromTo v x y a) = ListFromTo v (goE x) (goE y) a
  transE (ListFromThenTo v x y z a) =
    ListFromThenTo v (goE x) (goE y) (goE z) a

  goB (Binding v e c a) = Binding v (goE e) c a
  goB (TypedMatching v e a b) = TypedMatching v (goE e) a b
  goB (Matching p e a) = Matching p (goE e) a

  goQ (CompGuard e) = CompGuard (goE e)
  goQ (CompGen p e a) = CompGen p (goE e) a
  goQ (CompLet b a) = CompLet (map goB b) a

  goE = transE . fe

  goArm (Arm p g e) = Arm p (goE <$> g) (goE e)

transformExprTyped
  :: (Expr Typed -> Expr Typed)
  -> (Coercion Typed -> Coercion Typed)
  -> (Type Typed -> Type Typed)
  -> Expr Typed -> Expr Typed
transformExprTyped fe fc ft = goE where
  transE (VarRef v a) = VarRef v (goA a)
  transE (Let re vs r a) = Let re (map transBind vs) (goE r) (goA a)
  transE (If c t f a) = If (goE c) (goE t) (goE f) (goA a)
  transE (App f x a) = App (goE f) (goE x) (goA a)
  transE (Fun p b a) = Fun (goPa p) (goE b) (goA a)
  transE (Begin es a) = Begin (map goE es) (goA a)
  transE (Literal l a) = Literal l (goA a)
  transE (Match e bs a) = Match (goE e) (map goArm bs) (goA a)
  transE (Function bs a) = Function (map goArm bs) a
  transE (BinOp l o r a) = BinOp (goE l) (goE o) (goE r) (goA a)
  transE (Hole v a) = Hole v (goA a)
  transE (Ascription e t a) = Ascription (goE e) (goT t) (goA a)
  transE (Vta e t a) = Vta (goE e) (goT t) (goA a)

  transE (Record fs a) = Record (map (over fExpr goE) fs) (goA a)
  transE (RecordExt f fs a) = RecordExt (goE f) (map (over fExpr goE) fs) (goA a)
  transE (Access e f a) = Access (goE e) f (goA a)

  transE (LeftSection o r a) = LeftSection (goE o) (goE r) (goA a)
  transE (RightSection l o a) = RightSection (goE l) (goE o) (goA a)
  transE (BothSection o a) = BothSection (goE o) (goA a)
  transE (AccessSection t a) = AccessSection t (goA a)
  transE (Parens e a) = Parens (goE e) (goA a)

  transE (Tuple es a) = Tuple (map goE es) (goA a)
  transE (TupleSection es a) = TupleSection (map (goE<$>) es) (goA a)

  transE (OpenIn n e a) = OpenIn n (goE e) (goA a)

  transE (ExprWrapper w e a) = ExprWrapper (goW w) (goE e) (goA a)
  transE (Lazy e a) = Lazy (goE e) (goA a)
  transE (ListExp e a) = ListExp (map goE e) (goA a)
  transE (ListComp e qs a) = ListComp (transE e) (map goQ qs) (goA a)
  transE (DoExpr v qs a) = DoExpr v (map goQ qs) (goA a)
  transE (Idiom vp va es a) = Idiom vp va (goE es) (goA a)
  transE (ListFromTo v x y a) = ListFromTo v (goE x) (goE y) (goA a)
  transE (ListFromThenTo v x y z a) =
    ListFromThenTo v (goE x) (goE y) (goE z) (goA a)

  transBind (Binding v e b a) = Binding v (goE e) b (goA a)
  transBind (Matching p e a) = Matching (goP p) (goE e) (goA a)
  transBind (TypedMatching p e a c) = TypedMatching (goP p) (goE e) (goA a) (map (second goT) c)

  goW (Cast c) = Cast (goC c)
  goW (ExprApp c) = ExprApp (goE c)
  goW (TypeApp t) = TypeApp (goT t)
  goW (TypeAsc t) = TypeAsc (goT t)
  goW (WrapFn f) = WrapFn f
  goW (x :> y) = goW x :> goW y
  goW x@TypeLam{} = x
  goW x@WrapVar{} = x
  goW IdWrap = IdWrap

  goQ (CompGuard e) = CompGuard (goE e)
  goQ (CompGen p e a) = CompGen p (goE e) a
  goQ (CompLet b a) = CompLet (map transBind b) a

  goPa = paramPat %~ goP

  goE = transE . fe
  goT = ft
  goC = transformCoercion fc ft
  goA (s, ty) = (s, goT ty)

  goP = transformPatternTyped id ft

  goArm (Arm p g e) = Arm (goP p) (goE <$> g) (goE e)

transformPatternTyped
  :: (Pattern Typed -> Pattern Typed)
  -> (Type Typed -> Type Typed)
  -> Pattern Typed -> Pattern Typed
transformPatternTyped fp ft = goP where
  transP (Wildcard a) = Wildcard (goA a)
  transP (Capture v a) = Capture v (goA a)
  transP (Destructure v p a) = Destructure v (goP <$> p) (goA a)
  transP (PAs p v a) = PAs (goP p) v (goA a)
  transP (PType p t a) = PType (goP p) (goT t) (goA a)
  transP (PRecord fs a) = PRecord (map (second goP) fs) (goA a)
  transP (PTuple ps a) = PTuple (map goP ps) (goA a)
  transP (PList ps a) = PList (map goP ps) (goA a)
  transP (PLiteral l a) = PLiteral l (goA a)
  transP (PGadtCon v vs vs' p a) = PGadtCon v vs (map (_2 %~ goT) vs') (goP <$> p) (goA a)

  goA (s, ty) = (s, goT ty)
  goT = ft
  goP = transP . fp

correct :: Type Typed -> Expr Typed -> Expr Typed
correct ty (VarRef v a) = VarRef v (fst a, ty)
correct ty (Let re vs r a) = Let re vs r (fst a, ty)
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
correct ty (Vta e t a) = Ascription e t (fst a, ty)

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

correct ty (Lazy e a) = Lazy e (fst a, ty)
correct ty (ListExp e a) = ListExp e (fst a, ty)
correct ty (ListComp e qs a) = ListComp e qs (fst a, ty)
correct ty (DoExpr v qs a) = DoExpr v qs (fst a, ty)
correct ty (Idiom vp va es a) = Idiom vp va es (fst a, ty)
correct ty (ListFromTo v x y a) = ListFromTo v x y (fst a, ty)
correct ty (ListFromThenTo v x y z a) =
  ListFromThenTo v x y z (fst a, ty)

correct ty (ExprWrapper w e a) = ExprWrapper w e (fst a, ty)
