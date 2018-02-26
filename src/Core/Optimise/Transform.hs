module Core.Optimise.Transform
  ( Scope(..)
  , Transform, transformOver, transformStmts
  , isCon, simplifyVar, simplifyTerm, lookupVar
  ) where

import qualified Data.VarMap as VarMap
import Data.VarSet (IsVar(..))
import Data.Triple
import Data.Semigroup

import Control.Arrow

import Core.Core

data Scope a = Scope { vars :: VarMap.Map (CoTerm a)
                     , types :: VarMap.Map [(a, CoType a)]
                     , cons :: VarMap.Map (CoType a) }
  deriving (Show)

instance Ord a => Semigroup (Scope a) where
  (Scope v t c) <> (Scope v' t' c') = Scope (v <> v') (t <> t') (c <> c')

instance Ord a => Monoid (Scope a) where
  mempty = Scope mempty mempty mempty
  mappend = (<>)

isCon :: IsVar a => Scope a -> a -> Bool
isCon s var = VarMap.member (toVar var) (cons s)

lookupVar :: IsVar a => Scope a -> a -> Maybe (CoTerm a)
lookupVar s v = VarMap.lookup (toVar v) (vars s)

simplifyVar :: IsVar a => Scope a -> a -> a
simplifyVar s v = case VarMap.lookup (toVar v) (vars s) of
                  Just (CotTyApp (CoaRef v' _) _) -> simplifyVar s v'
                  Just (CotAtom (CoaRef v' _)) -> simplifyVar s v'
                  _ -> v

simplifyTerm :: IsVar a => Scope a -> a -> Maybe (CoTerm a)
simplifyTerm s v = VarMap.lookup (toVar (simplifyVar s v)) (vars s)

type Transform b a = Scope a -> b a -> b a

extendVars :: IsVar a => [(a, CoType a, CoTerm a)] -> Scope a -> Scope a
extendVars vs s = s { vars = foldr (\(v, _, e) m -> VarMap.insert (toVar v) e m) (vars s) vs }

transformOver :: IsVar a => Transform CoTerm a -> Transform CoAtom a -> Transform CoTerm a
transformOver ft fa = transT where
  mapA _ t@CoaRef{} = t
  mapA _ t@CoaLit{} = t
  mapA s (CoaLam t v b) = CoaLam t v (transT s b)

  transA s = fa s . mapA s

  mapT s (CotAtom a) = CotAtom (transA s a)
  mapT s (CotApp f a) = CotApp (transA s f) (transA s a)
  mapT s (CotTyApp f t) = CotTyApp (transA s f) t
  mapT s (CotExtend t rs) = CotExtend (transA s t) (map (third3 (transA s)) rs)
  mapT s (CotLet vars body) =
    let vars' = map (third3 (transT (extendVars vars s))) vars
        body' = transT (extendVars vars' s) body
    in CotLet vars' body'
  mapT s (CotMatch test branches) =
    let test' = transA s test
        branches' = map (third3 (transT s)) branches
    in CotMatch test' branches'

  transT s = ft s . mapT s

transformStmts :: IsVar a => Transform CoTerm a -> Transform CoAtom a -> Scope a -> [CoStmt a] -> [CoStmt a]
transformStmts _  _  _ [] = []
transformStmts ft fa s (CosForeign{}:xs) = transformStmts ft fa s xs
transformStmts ft fa s (CosLet vars:xs) =
  let vars' = map (third3 (transformOver ft fa (extendVars vars s))) vars
      xs' = transformStmts ft fa (extendVars vars' s) xs
  in CosLet vars':xs'
transformStmts ft fa s (x@(CosType v cases):xs) =
  let s' = s { types = VarMap.insert (toVar v) cases (types s)
             , cons = VarMap.union (VarMap.fromList (map (first toVar) cases)) (cons s) }
  in x:transformStmts ft fa s' xs
