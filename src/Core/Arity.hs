{-# LANGUAGE OverloadedStrings #-}
module Core.Arity
  ( ArityScope
  , emptyScope
  , atomArity
  , isPure
  , extendPureFuns, extendPureCtors
  ) where

import qualified Data.VarMap as VarMap
import qualified Data.Map as Map
import Data.VarSet (IsVar(..))
import Data.Text (Text)
import Data.Triple
import Data.Maybe

import Core.Core
import Syntax(Var(..))

newtype ArityScope = ArityScope { pureArity :: VarMap.Map Int }
  deriving (Show)

emptyScope :: ArityScope
emptyScope = ArityScope mempty

-- Compute the number of arguments which can be passed to a function before
-- it becomes "impure".
atomArity :: IsVar a => ArityScope -> AnnAtom b a -> Int
atomArity s (Ref r _)
  | TgInternal n <- toVar r
  = fromMaybe 0 (Map.lookup n opArity)
  | otherwise
  = fromMaybe 0 (VarMap.lookup (toVar r) (pureArity s))
atomArity s (Lam _ (AnnAtom _ a)) = 1 + atomArity s a
atomArity _ _ = 0

isPure :: IsVar a => ArityScope -> AnnTerm b a -> Bool
isPure _ AnnAtom{}   = True
isPure _ AnnExtend{} = True
isPure _ AnnTyApp{}  = True
isPure _ AnnCast{}  = True
isPure s (AnnLet _ (One v) e) = isPure s e && isPure s (thd3 v)
isPure s (AnnLet _ (Many vs) e) = isPure s e && all (isPure s . thd3) vs
isPure s (AnnMatch _ _ bs) = all (isPure s . armBody) bs
isPure s (AnnApp _ f _) = atomArity s f > 0

extendPureFuns :: IsVar a => ArityScope -> [(a, Type a, AnnTerm b a)] -> ArityScope
extendPureFuns s vs = s
  { pureArity = foldr (\(v, _, e) p ->
                         case e of
                           AnnAtom  _ a    -> maybeInsert v (atomArity s a) p
                           AnnApp   _ a _  -> maybeInsert v (atomArity s a - 1) p
                           AnnTyApp  _ a _ -> maybeInsert v (atomArity s a - 1) p
                           _ -> p) (pureArity s) vs
  }
  where
    maybeInsert v a m
      | a > 0     = VarMap.insert (toVar v) a m
      | otherwise = m

extendPureCtors :: IsVar a => ArityScope -> [(a, Type a)] -> ArityScope
extendPureCtors s cts = s {
  pureArity = foldr (\(v, ty) p -> VarMap.insert (toVar v) (1 + typeArity ty) p) (pureArity s) cts }

  where
    typeArity :: Type a -> Int
    typeArity (ForallTy _ _ ty) = 1 + typeArity ty
    typeArity _ = 0


-- Various built-in functions with a predetermined arity
opArity :: Map.Map Text Int
opArity = Map.fromList
    [ ("+",  2), ("+.",  2)
    , ("-",  2), ("-.",  2)
    , ("*",  2), ("*.",  2)
    , ("/",  2), ("/.",  2)
    , ("**", 2), ("**.", 2)
    , ("^",  2)
    , ("<",  2)
    , (">",  2)
    , (">=", 2)
    , ("<=", 2)
    , ("==", 3)
    , ("<>", 3)
    , ("||", 2)
    , ("&&", 2)
    ]
