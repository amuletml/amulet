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

-- | Compute the arity of a function
atomArity :: IsVar a => ArityScope -> AnnAtom b a -> Arity
atomArity s (Ref r _) = varArity r s
atomArity s (Lam _ (AnnAtom _ a)) = mapArity succ (atomArity s a)
atomArity _ (Lam _ _) = Arity 1 False
atomArity _ (Lit _) = Arity 0 True

-- | Determine if the given term can be evaluated without side effects
isPure :: IsVar a => ArityScope -> AnnTerm b a -> Bool
isPure _ AnnAtom{}   = True
isPure _ AnnExtend{} = True
isPure _ AnnTyApp{}  = True
isPure _ AnnCast{}  = True
isPure s (AnnLet _ (One v) e) = isPure s e && isPure s (thd3 v)
isPure s (AnnLet _ (Many vs) e) = isPure s e && all (isPure s . thd3) vs
isPure s (AnnMatch _ _ bs) = all (isPure s . view armBody) bs
isPure s (AnnApp _ f _) = isPureA . mapArity pred $ atomArity s f

isPureA :: Arity -> Bool
isPureA (Arity a p) | p = a >= 0
                    | otherwise = a > 0

extendPureLets :: IsVar a => ArityScope -> [(a, Type a, AnnTerm b a)] -> ArityScope
extendPureLets s vs = s
  { pureArity = foldr (\(v, _, e) p ->
                         case e of
                           AnnAtom  _ a    -> maybeInsert v (atomArity s a) p
                           AnnApp   _ a _  -> maybeInsert v (mapArity pred (atomArity s a)) p
                           AnnTyApp  _ a _ -> maybeInsert v (mapArity pred (atomArity s a)) p
                           _ -> p) (pureArity s) vs
  }
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
    [ (vOpAdd, 2), (vOpAddF, 2)
    , (vOpSub, 2), (vOpSubF, 2)
    , (vOpMul, 2), (vOpMulF, 2)
    , (vOpDiv, 2), (vOpDivF, 2)
    , (vOpExp, 2), (vOpExpF, 2)
    , (vOpLt,  2), (vOpLtF,  2)
    , (vOpGt,  2), (vOpGtF,  2)
    , (vOpLe,  2), (vOpLeF,  2)
    , (vOpGe,  2), (vOpGeF,  2)
    , (vLAZY,  2)

    , (vOpConcat,  2)

    , (vOpEq, 3)
    , (vOpNe, 3)
    , (vOpOr, 2)
    , (vOpAnd, 2)
    ]
