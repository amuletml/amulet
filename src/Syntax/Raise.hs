{-# LANGUAGE ScopedTypeVariables #-}
module Syntax.Raise (raiseT) where

import Data.Bifunctor

import Syntax.Var
import Syntax

-- | Raise variables from one phase to another within a type.
{-# DEPRECATED #-}
raiseT :: forall p p'
        . (Var p -> Var p')
       -- ^ Raise a single variable. This will be a type variable, type
       -- name or constructor.
       -> (TypeAnn p -> TypeAnn p')
       -> Type p -> Type p'
raiseT fv fa = ty where
  ty :: Type p -> Type p'
  ty (TyCon n a) = TyCon (fv n) (fa a)
  ty (TyWildcard n) = TyWildcard (ty <$> n)
  ty (TyPromotedCon n a) = TyPromotedCon (fv n) (fa a)
  ty (TySkol (Skolem n u t m)) = TySkol (Skolem (fv n) (fv u) (ty t) (motive m))
  ty (TyVar n a) = TyVar (fv n) (fa a)
  ty (TyApp x y) = TyApp (ty x) (ty y)
  ty (TyTuple x y) = TyTuple (ty x) (ty y)
  ty (TyTupleL x y) = TyTupleL (ty x) (ty y)
  ty (TyRows rho rows) = TyRows (ty rho) (map (second ty) rows)
  ty (TyExactRows rows) = TyExactRows (map (second ty) rows)
  ty (TyWithConstraints eq a) = TyWithConstraints (map (bimap ty ty) eq) (ty a)
  ty (TyPi binder t) = TyPi (bind binder) (ty t)
  ty TyType = TyType
  ty (TyOperator l o r) = TyOperator (ty l) (ty o) (ty r)
  ty (TyParens t) = ty t
  ty (TyLit l) = TyLit l

  motive (BySubsumption a b) = BySubsumption (ty a) (ty b)
  motive (ByAscription e t) = ByAscription e (ty t)
  motive (ByExistential a t) = ByExistential (fv a) (ty t)
  motive (ByConstraint t) = ByConstraint (ty t)
  motive (ByInstanceHead t a) = ByInstanceHead (ty t) a
  motive (ByTyFunLhs t a) = ByTyFunLhs (ty t) a

  bind (Anon t) = Anon (ty t)
  bind (Implicit t) = Implicit (ty t)
  bind (Invisible var k spec) = Invisible (fv var) (ty <$> k) spec
