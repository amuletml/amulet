module Core.Terms
  ( mapTerm, mapTerm1
  , substitute
  ) where

import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)

import Core.Core
import Syntax

-- Apply a function to each child in the provided expr. Note this does not
-- apply to all descendants.
mapTerm1 :: (CoTerm -> CoTerm) -> CoTerm -> CoTerm
mapTerm1 _ c@(CotRef _ _ ) = c
mapTerm1 f (CotLam s v bod) = CotLam s v (f bod)
mapTerm1 f (CotApp fn ag) = CotApp (f fn) (f ag)
mapTerm1 f (CotLet binds bod) = CotLet (map (\(v, t, e) -> (v, t, (f e))) binds) (f bod)
mapTerm1 f (CotMatch e bods) = CotMatch (f e) (map (\(p, t, e) -> (p, t, (f e))) bods)
mapTerm1 f (CotBegin ts t) = CotBegin (map f ts) (f t)
mapTerm1 _ c@(CotLit _) = c
mapTerm1 f (CotExtend r fs) = CotExtend (f r) (map (\(n, t, e) -> (n, t, (f e))) fs)
mapTerm1 f (CotTyApp fn ag) = CotTyApp (f fn) ag

-- Apply a function to all descendants in the provided expr.
mapTerm :: (CoTerm -> CoTerm) -> CoTerm -> CoTerm
mapTerm f = mapTerm1 (f . mapTerm f)

substitute :: M.Map (Var Resolved) CoTerm -> CoTerm -> CoTerm
substitute m = mapTerm subst
  where subst e@(CotRef v _) = fromMaybe e (M.lookup v m)
        subst e = e
