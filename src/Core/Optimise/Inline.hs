{-# LANGUAGE ScopedTypeVariables #-}
module Core.Optimise.Inline
  ( inlineVariable
  , inlineVariablePass
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.VarSet as VarSet
import Data.VarSet (IsVar(..))
import Data.Triple

import Core.Optimise.Transform
import Core.Core

import Debug.Trace
import Pretty

limit :: Int
limit = 500

inlineVariablePass :: IsVar a => [CoStmt a] -> [CoStmt a]
inlineVariablePass = transformStmts inlineVariable (const id) mempty

-- The "opposite" (dual?) of a propagation pass, inlining attempts to
-- "pull in" the definition of a variable it considers cheap.
inlineVariable :: IsVar a => Transform CoTerm a
inlineVariable s x@(CotApp (CoaRef v _) a)
  | Just (CotAtom f@(CoaLam Small _ _)) <- Map.lookup v (vars s) =
      let cost = scoreAtom s f
      in if cost < limit && not (recursive f v)
         then CotApp f a else x
inlineVariable s x@(CotTyApp (CoaRef v _) t)
  | Just (CotAtom f@(CoaLam Big _ _)) <- Map.lookup v (vars s) =
      let cost = scoreAtom s f
      in if cost < limit && not (recursive f v)
         then CotTyApp f t else x
inlineVariable _ x = x

scoreAtom :: IsVar a => Scope a -> CoAtom a -> Int
scoreAtom s (CoaRef v _) = if isCon s v then 0 else 5
scoreAtom _ (CoaLit _) = 1
scoreAtom s (CoaLam Big _ b) = scoreTerm s b
scoreAtom s (CoaLam Small _ b) = 1 + scoreTerm s b

scoreTerm :: IsVar a => Scope a -> CoTerm a  -> Int
scoreTerm s (CotAtom a) = scoreAtom s a
scoreTerm s (CotApp f x) = scoreAtom s f + scoreAtom s x + 2
scoreTerm s (CotLet vs e) = sum (map (scoreTerm s . thd3) vs) + scoreTerm s e
scoreTerm s (CotMatch e bs) = scoreAtom s e + sum (map (scoreTerm s . thd3) bs)
scoreTerm s (CotExtend e rs) = scoreAtom s e + sum (map (scoreAtom s . thd3) rs)
scoreTerm s (CotTyApp t _) = scoreAtom s t

recursive :: IsVar a => CoAtom a -> a -> Bool
recursive e v = toVar v `VarSet.member` freeInAtom e
