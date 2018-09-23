module Core.Optimise.Reduce.Inline
  ( sizeAtom
  , sizeTerm
  ) where

import Control.Lens

import qualified Data.VarMap as VarMap
import Data.Triple

import Core.Optimise.Reduce.Base

sizeAtom :: IsVar a => ReduceScope a -> Atom a -> Int
sizeAtom s (Ref v _) = if toVar v `VarMap.member` (s ^. ctorScope) then 0 else 5
sizeAtom _ (Lit _) = 1

sizeTerm :: IsVar a => ReduceScope a -> Term a  -> Int
sizeTerm s (Atom a) = sizeAtom s a
sizeTerm s (App f x) = sizeAtom s f + sizeAtom s x + 2
sizeTerm s (Lam TypeArgument{} b) = sizeTerm s b
sizeTerm s (Lam TermArgument{} b) = 1 + sizeTerm s b
sizeTerm s (Let (One v) e) = sizeTerm s (thd3 v) + sizeTerm s e
sizeTerm s (Let (Many vs) e) = sum (map (sizeTerm s . thd3) vs) + sizeTerm s e
sizeTerm s (Match e bs) = sizeAtom s e + sum (map (sizeTerm s . view armBody) bs)
sizeTerm s (Extend e rs) = sizeAtom s e + sum (map (sizeAtom s . thd3) rs)
sizeTerm s (Values xs) = sum (map (sizeAtom s) xs)
sizeTerm s (TyApp t _) = sizeAtom s t
sizeTerm s (Cast t _) = sizeAtom s t
