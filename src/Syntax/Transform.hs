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
  transT (TyForall vs ty) = TyForall vs (goT ty)
  transT (TyArr x r) = TyArr (goT x) (goT r)
  transT (TyApp f x) = TyArr (goT f) (goT x)
  transT (TyRows f fs) = TyRows (goT f) (map (second goT) fs)
  transT (TyExactRows fs) = TyExactRows (map (second goT) fs)
  transT (TyTuple l r) = TyTuple (goT l) (goT r)

  transT (TySkol (Skolem i v ty m)) = TySkol (Skolem i v (goT ty) (transM m))
  transT (TyWithConstraints cons ty) = TyWithConstraints (map (goT***goT) cons) (goT ty)


  transM (ByAscription ty) = ByAscription (goT ty)
  transM (BySubsumption l r) = BySubsumption (goT l) (goT r)
  transM (ByExistential v ty) = ByExistential v (goT ty)

  goT = transT . ft


transformCoercion
  :: (Coercion p -> Coercion p)
  -> (Type p -> Type p)
  -> Coercion p -> Coercion p
transformCoercion fc ft = goC where
  transC (VarCo c) = VarCo c
  transC (ReflCo l r) = ReflCo (goT l) (goT r)
  transC (CompCo l r) = CompCo (goC l) (goC r)

  goT = transformType ft . ft
  goC = transC . fc

transformExpr
  :: (Expr p -> Expr p )
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

  transE (Tuple es a) = Tuple (map goE es) a
  transE (TupleSection es a) = TupleSection (map (goE<$>) es) a

  transE (TypeApp e t a) = TypeApp (goE e) t a
  transE (Cast e c a) = Cast (goE e) c a

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

  transE (Tuple es a) = Tuple (map goE es) (goA a)
  transE (TupleSection es a) = TupleSection (map (goE<$>) es) (goA a)

  transE (TypeApp e t a) = TypeApp (goE e) (goT t) (goA a)
  transE (Cast e c a) = Cast (goE e) (goC c) (goA a)

  transP (Wildcard a) = Wildcard (goA a)
  transP (Capture v a) = Capture v (goA a)
  transP (Destructure v p a) = Destructure v (goP <$> p) (goA a)
  transP (PType p t a) = PType (goP p) (goT t) (goA a)
  transP (PRecord fs a) = PRecord (map (second goP) fs) (goA a)
  transP (PTuple ps a) = PTuple (map goP ps) (goA a)

  goE = transE . fe
  goT = transformType ft
  goC = transformCoercion fc ft
  goA (s, ty) = (s, goT ty)

  goP = transP
