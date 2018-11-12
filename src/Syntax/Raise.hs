{-# LANGUAGE ScopedTypeVariables #-}
module Syntax.Raise (raiseT) where

import Control.Arrow

import Syntax.Var
import Syntax

-- | Raise variables from one phase to another within a type.
raiseT :: (Var p -> Var p')
       -- ^ Raise a single variable. This will be a type variable, type
       -- name or constructor.
       -> Type p -> Type p'
raiseT v (TyCon n) = TyCon (v n)
raiseT v (TyWildcard n) = TyWildcard (raiseT v <$> n)
raiseT v (TyPromotedCon n) = TyPromotedCon (v n)
raiseT v (TySkol (Skolem n u t m)) = TySkol (Skolem (v n) (v u) (raiseT v t) (motive m)) where
  motive (BySubsumption a b) = BySubsumption (raiseT v a) (raiseT v b)
  motive (ByAscription e t) = ByAscription e (raiseT v t)
  motive (ByExistential a t) = ByExistential (v a) (raiseT v t)
  motive (ByConstraint t) = ByConstraint (raiseT v t)
  motive (ByInstanceHead t a) = ByInstanceHead (raiseT v t) a
raiseT v (TyVar n) = TyVar (v n)
raiseT v (TyApp x y) = TyApp (raiseT v x) (raiseT v y)
raiseT v (TyTuple x y) = TyTuple (raiseT v x) (raiseT v y)
raiseT v (TyRows rho rows) = TyRows (raiseT v rho) (map (second (raiseT v)) rows)
raiseT v (TyExactRows rows) = TyExactRows (map (second (raiseT v)) rows)
raiseT v (TyWithConstraints eq a) = TyWithConstraints (map (\(a, b) -> (raiseT v a, raiseT v b)) eq) (raiseT v a)
raiseT v (TyPi binder t) = TyPi (go binder) (raiseT v t) where
  go (Anon t) = Anon (raiseT v t)
  go (Implicit t) = Implicit (raiseT v t)
  go (Invisible var k) = Invisible (v var) (fmap (raiseT v) k)
raiseT _ TyType = TyType
raiseT v (TyOperator l o r) = TyOperator (raiseT v l) (v o) (raiseT v r)
raiseT v (TyParens t) = raiseT v t
