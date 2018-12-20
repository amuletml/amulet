-- | The "Core.Arity" module acts as a basic system for tracking the
-- arity of various definitions and so the purity of them.
--
-- For instance, consider the following term:
--
-- > \x -> print x
-- >       \y -> x + y
--
-- While the type may have arity 3, the tracker only considers this as
-- arity 2.
--
-- We also disambiguate between impure and pure functions, though
-- analysis of this is currently limited to constructors and
-- builtins. For instance our above function can only be applied to one
-- argument before side effects may occur. Meanwhile, @(+)@ can be
-- applied to 2 without any side effect.
module Core.Arity
  ( ArityScope
  , Arity(..)
  , emptyScope, varArity
  , isPure
  , extendPureLets, extendPureCtors, extendForeign
  ) where

import Control.Lens

import qualified Data.VarMap as VarMap
import Data.Triple
import Data.Maybe

import Core.Builtin
import Core.Core
import Core.Var

-- | The arity of a definition
data Arity =
  Arity
  { defArity  :: {-# UNPACK #-} !Int -- ^ The number of arguments this definition accepts
  , defPurity :: !Bool -- ^ Whether this definition is pure when fully applied
  }
  deriving (Show, Eq)

-- | Tracks the arity of variables in the current scope
newtype ArityScope = ArityScope { pureArity :: VarMap.Map Arity }
  deriving (Show)

-- | Lookup the arity of a variable
varArity :: IsVar a => a -> ArityScope -> Arity
varArity var = fromMaybe impureTerm . VarMap.lookup (toVar var) . pureArity where
  impureTerm = Arity 0 False

-- | The default arity scope
emptyScope :: ArityScope
emptyScope = ArityScope opArity

-- | Compute the arity of a term
atomArity :: IsVar a => ArityScope -> Atom a -> Arity
atomArity s (Ref r _) = varArity r s
atomArity _ (Lit _)   = Arity 0 True

termArity :: IsVar a => ArityScope -> AnnTerm b a -> Arity
-- Lookup within the parent scope
termArity s (AnnAtom _ a)     = atomArity s a
termArity s (AnnApp   _ a _)  = mapArity pred (atomArity s a)
termArity s (AnnTyApp  _ a _) = mapArity pred (atomArity s a)
termArity s (AnnLam _ _ b)    = mapArity succ (termArity s b)
-- Always pure to evaluate
termArity _ AnnExtend{}       = Arity 0 True
termArity _ AnnValues{}       = Arity 0 True
termArity _ AnnCast{}         = Arity 0 True
-- Just terminate on these. While it would be technically possible to
-- recurse arbitrarily deep, this will inevitably end up quadratic.
termArity _ AnnLet{}          = Arity 0 False
termArity _ AnnMatch{}        = Arity 0 False

-- | Determine if the given term can be evaluated without side effects
isPure :: IsVar a => ArityScope -> AnnTerm b a -> Bool
isPure _ AnnAtom{}   = True
isPure _ AnnExtend{} = True
isPure _ AnnValues{} = True
isPure _ AnnTyApp{}  = True
isPure _ AnnCast{}   = True
isPure _ AnnLam{}    = True
isPure s (AnnApp _ f _) = isPureA . mapArity pred $ atomArity s f
isPure s (AnnMatch _ _ bs) = all (isPure s . view armBody) bs

isPure s (AnnLet _ (One v) e) = isPure s (thd3 v) && isPure (extendPureLets s [v]) e
isPure s (AnnLet _ (Many vs) e) =
  let s' = extendPureLets s vs
  in all (isPure s' . thd3) vs && isPure s' e

isPureA :: Arity -> Bool
isPureA (Arity a p) | p = a >= 0
                    | otherwise = a > 0

maybeInsert :: IsVar a => a -> Arity -> VarMap.Map Arity -> VarMap.Map Arity
maybeInsert v a m
  | defArity a > 0 = VarMap.insert (toVar v) a m
  | otherwise = m

-- | Extend the arity scope with a series of bindings.
--
-- This will add any term which has an arity greater than 0 - we need not
-- consider terms which are strictly pure but have a arity of 0 (such as
-- atoms) as references to them will already be pure.
extendPureLets :: IsVar a => ArityScope -> [(a, Type a, AnnTerm b a)] -> ArityScope
extendPureLets s vs = s { pureArity = foldr (\(v, _, e) p -> maybeInsert v (termArity s e) p) (pureArity s) vs }

-- | Extend the arity scope with all constructors defined within a type.
extendPureCtors :: IsVar a => ArityScope -> [(a, Type a)] -> ArityScope
extendPureCtors s cts = s {
  pureArity = foldr (\(v, ty) p -> VarMap.insert (toVar v) (Arity (typeArity ty) True) p) (pureArity s) cts }

  where
    typeArity :: Type a -> Int
    typeArity (ForallTy _ _ ty) = 1 + typeArity ty
    typeArity _ = 0

extendForeign :: IsVar a => ArityScope -> (a, Type a) -> ArityScope
extendForeign (ArityScope scope) (var, ty) =
  ArityScope (VarMap.insert (toVar var) (Arity (typeArity ty) False) scope)
  where typeArity :: Type a -> Int
        typeArity (ForallTy _ _ ty) = 1 + typeArity ty
        typeArity _ = 0

mapArity :: (Int -> Int) -> Arity -> Arity
mapArity f (Arity a p) = Arity (f a) p

-- | Various built-in functions with a predetermined arity
opArity :: VarMap.Map Arity
opArity = VarMap.fromList . map (flip Arity True <$>) $
    [ (vOpAdd, 1), (vOpAddF, 1)
    , (vOpSub, 1), (vOpSubF, 1)
    , (vOpMul, 1), (vOpMulF, 1)
    , (vOpDiv, 1), (vOpDivF, 1)
    , (vOpExp, 1), (vOpExpF, 1)
    , (vOpLt,  1), (vOpLtF,  1)
    , (vOpGt,  1), (vOpGtF,  1)
    , (vOpLe,  1), (vOpLeF,  1)
    , (vOpGe,  1), (vOpGeF,  1)
    , (vLAZY,  2)
    , (vCONS,  2), (vNIL, 1)
    , (vSHOW,  2), (vOpShow, 2)

    , (vOpConcat,  1)

    , (vOpEq, 2)
    , (vOpNe, 2)
    ]
