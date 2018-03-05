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

data InlineScope a = InlineScope { scores :: VarMap.Map (Atom a, Int)
                                 -- , vars :: VarMap.Map (Term a)
                                 , cons :: VarSet.Set }

inlineVariablePass :: IsVar a => [Stmt a] -> [Stmt a]
inlineVariablePass = transS (InlineScope mempty mempty) where
  transS _ [] = []
  transS s (x@Foreign{}:xs) = x:transS s xs
  transS s (StmtLet vars:xs) =
    let vars' = map (third3 (transT s)) vars
        xs' = transS (extendVars vars' s) xs
    in StmtLet vars':xs'
  transS s (x@(Type _ cases):xs) =
    let s' = s { cons = VarSet.fromList (map (toVar . fst) cases) `VarSet.union` cons s }
    in x:transS s' xs

  transA _ t@Ref{} = t
  transA _ t@Lit{} = t
  transA s (Lam t v b) = Lam t v (transT s b)

  transT :: IsVar a => InlineScope a -> Term a -> Term a
  transT s (Atom a) = Atom (transA s a)
  transT s (App f a) =
    let f' = transA s f
        a' = transA s a
     in case f' of
         Ref r _
           | Just (Lam Small (v, t) b, score) <- VarMap.lookup (toVar r) (scores s)
           , score <= limit -> Let [(v, t, Atom a')] b
         Lam Small (v, t) b -> Let [(v, t, Atom a')] b
         _ -> App f' a'
  transT s (Cast f t) = Cast (transA s f) t
  transT s (TyApp f t) =
    case transA s f of
      Ref r _
        | Just (Lam Big (v, _) b, score) <- VarMap.lookup (toVar r) (scores s)
        , score <= limit -> substituteInTys (Map.singleton v t) b
      Lam Big (v, _) b -> substituteInTys (Map.singleton v t) b
      f' -> TyApp f' t
  transT s (Extend t rs) = Extend (transA s t) (map (third3 (transA s)) rs)
  transT s (Let vars body) =
    let vars' = map (third3 (transT s)) vars
        body' = transT (extendVars vars' s) body
    in Let vars' body'
  transT s (Match test branches) =
    let test' = transA s test
        branches' = map (third3 (transT s)) branches
    in Match test' branches'

  extendVars vs s = s
    { scores = foldr (\(v, _, e) m ->
                        case e of
                          Atom a
                            | isLambda a && not (occursInTerm v e)
                            -> VarMap.insert (toVar v) (a, scoreTerm s e) m
                          _ -> m) (scores s) vs
    -- , vars = foldr (\(v, _, e) m -> VarMap.insert (toVar v) e m) (vars s) vs
    }

  isLambda (Lam Small _ _) = True
  isLambda (Lam Big _ (Atom b)) = isLambda b
  isLambda _ = False

scoreAtom :: IsVar a => InlineScope a -> Atom a -> Int
scoreAtom s (Ref v _) = if toVar v `VarSet.member` cons s then 0 else 5
scoreAtom _ (Lit _) = 1
scoreAtom s (Lam Big _ b) = scoreTerm s b
scoreAtom s (Lam Small _ b) = 1 + scoreTerm s b

scoreTerm :: IsVar a => InlineScope a -> Term a  -> Int
scoreTerm s (Atom a) = scoreAtom s a
scoreTerm s (App f x) = scoreAtom s f + scoreAtom s x + 2
scoreTerm s (Let vs e) = sum (map (scoreTerm s . thd3) vs) + scoreTerm s e
scoreTerm s (Match e bs) = scoreAtom s e + sum (map (scoreTerm s . thd3) bs)
scoreTerm s (Extend e rs) = scoreAtom s e + sum (map (scoreAtom s . thd3) rs)
scoreTerm s (TyApp t _) = scoreAtom s t
scoreTerm s (Cast t _) = scoreAtom s t
