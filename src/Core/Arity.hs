module Core.Arity
  ( ArityScope
  , Arity(..)
  , emptyScope, varArity
  , isPure
  , extendPureLets, extendPureCtors
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
atomArity _ (Lit _) = Arity 0 True

termArity :: IsVar a => ArityScope -> AnnTerm b a -> Arity
termArity s (AnnAtom _ a) = atomArity s a
termArity s (AnnApp   _ a _) = mapArity pred (atomArity s a)
termArity s (AnnTyApp  _ a _) = mapArity pred (atomArity s a)
termArity s (AnnLam _ _ b) = mapArity succ (termArity s b)
termArity _ _ = Arity 0 False

-- | Determine if the given term can be evaluated without side effects
isPure :: IsVar a => ArityScope -> AnnTerm b a -> Bool
isPure _ AnnAtom{}   = True
isPure _ AnnExtend{} = True
isPure _ AnnValues{} = True
isPure _ AnnTyApp{}  = True
isPure _ AnnCast{}   = True
isPure _ AnnLam{}    = True
isPure s (AnnLet _ (One v) e) = isPure s e && isPure s (thd3 v)
isPure s (AnnLet _ (Many vs) e) = isPure s e && all (isPure s . thd3) vs
isPure s (AnnMatch _ _ bs) = all (isPure s . view armBody) bs
isPure s (AnnApp _ f _) = isPureA . mapArity pred $ atomArity s f

isPureA :: Arity -> Bool
isPureA (Arity a p) | p = a >= 0
                    | otherwise = a > 0

extendPureLets :: IsVar a => ArityScope -> [(a, Type a, AnnTerm b a)] -> ArityScope
extendPureLets s vs = s { pureArity = foldr (\(v, _, e) p -> maybeInsert v (termArity s e) p) (pureArity s) vs }
  where
    maybeInsert v a m
      | defArity a > 0 = VarMap.insert (toVar v) a m
      | otherwise = m

extendPureCtors :: IsVar a => ArityScope -> [(a, Type a)] -> ArityScope
extendPureCtors s cts = s {
  pureArity = foldr (\(v, ty) p -> VarMap.insert (toVar v) (Arity (1 + typeArity ty) True) p) (pureArity s) cts }

  where
    typeArity :: Type a -> Int
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

    , (vOpConcat,  1)

    , (vOpEq, 2)
    , (vOpNe, 2)
    ]
