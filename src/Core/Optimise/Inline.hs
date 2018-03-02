{-# LANGUAGE ScopedTypeVariables #-}
module Core.Optimise.Inline
  ( inlineVariablePass
  ) where

import qualified Data.VarMap as VarMap
import qualified Data.VarSet as VarSet
import qualified Data.Map.Strict as Map
import Data.VarSet (IsVar(..))
import Data.Triple

import Core.Optimise

limit :: Int
limit = 500

data InlineScope a = InlineScope { scores :: VarMap.Map (CoAtom a, Int)
                                 -- , vars :: VarMap.Map (CoTerm a)
                                 , cons :: VarSet.Set }

inlineVariablePass :: IsVar a => [CoStmt a] -> [CoStmt a]
inlineVariablePass = transS (InlineScope mempty mempty) where
  transS _ [] = []
  transS s (x@CosForeign{}:xs) = x:transS s xs
  transS s (CosLet vars:xs) =
    let vars' = map (third3 (transT s)) vars
        xs' = transS (extendVars vars' s) xs
    in CosLet vars':xs'
  transS s (x@(CosType _ cases):xs) =
    let s' = s { cons = VarSet.fromList (map (toVar . fst) cases) `VarSet.union` cons s }
    in x:transS s' xs

  transA _ t@CoaRef{} = t
  transA _ t@CoaLit{} = t
  transA s (CoaLam t v b) = CoaLam t v (transT s b)

  transT :: IsVar a => InlineScope a -> CoTerm a -> CoTerm a
  transT s (CotAtom a) = CotAtom (transA s a)
  transT s (CotApp f a) =
    let f' = transA s f
        a' = transA s a
     in case f' of
         CoaRef r _
           | Just (CoaLam Small (v, t) b, score) <- VarMap.lookup (toVar r) (scores s)
           , score <= limit -> CotLet [(v, t, CotAtom a')] b
         CoaLam Small (v, t) b -> CotLet [(v, t, CotAtom a')] b
         _ -> CotApp f' a'
  transT s (CotTyApp f t) =
    case transA s f of
      CoaRef r _
        | Just (CoaLam Big (v, _) b, score) <- VarMap.lookup (toVar r) (scores s)
        , score <= limit -> substituteInTys (Map.singleton v t) b
      CoaLam Big (v, _) b -> substituteInTys (Map.singleton v t) b
      f' -> CotTyApp f' t
  transT s (CotExtend t rs) = CotExtend (transA s t) (map (third3 (transA s)) rs)
  transT s (CotLet vars body) =
    let vars' = map (third3 (transT s)) vars
        body' = transT (extendVars vars' s) body
    in CotLet vars' body'
  transT s (CotMatch test branches) =
    let test' = transA s test
        branches' = map (third3 (transT s)) branches
    in CotMatch test' branches'

  extendVars vs s = s
    { scores = foldr (\(v, _, e) m ->
                        case e of
                          CotAtom a
                            | isLambda a && not (occursInTerm v e)
                            -> VarMap.insert (toVar v) (a, scoreTerm s e) m
                          _ -> m) (scores s) vs
    -- , vars = foldr (\(v, _, e) m -> VarMap.insert (toVar v) e m) (vars s) vs
    }

  isLambda (CoaLam Small _ _) = True
  isLambda (CoaLam Big _ (CotAtom b)) = isLambda b
  isLambda _ = False

scoreAtom :: IsVar a => InlineScope a -> CoAtom a -> Int
scoreAtom s (CoaRef v _) = if toVar v `VarSet.member` cons s then 0 else 5
scoreAtom _ (CoaLit _) = 1
scoreAtom s (CoaLam Big _ b) = scoreTerm s b
scoreAtom s (CoaLam Small _ b) = 1 + scoreTerm s b

scoreTerm :: IsVar a => InlineScope a -> CoTerm a  -> Int
scoreTerm s (CotAtom a) = scoreAtom s a
scoreTerm s (CotApp f x) = scoreAtom s f + scoreAtom s x + 2
scoreTerm s (CotLet vs e) = sum (map (scoreTerm s . thd3) vs) + scoreTerm s e
scoreTerm s (CotMatch e bs) = scoreAtom s e + sum (map (scoreTerm s . thd3) bs)
scoreTerm s (CotExtend e rs) = scoreAtom s e + sum (map (scoreAtom s . thd3) rs)
scoreTerm s (CotTyApp t _) = scoreAtom s t
