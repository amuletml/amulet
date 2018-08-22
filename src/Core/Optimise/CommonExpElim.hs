{-# LANGUAGE ScopedTypeVariables, ViewPatterns #-}
-- | Eliminate common subexpressions by keeping a hashmap of terms to
-- binding sites and recursively deduplicating each one-valued let.
module Core.Optimise.CommonExpElim (csePass) where

import qualified Data.HashMap.Strict as Map
import qualified Data.VarMap as Vm
import Data.HashMap.Strict (HashMap)

import Control.Lens

import Core.Optimise
import Core.Arity

type CseScope a = HashMap (Term a) a

csePass :: forall a. IsVar a => [Stmt a] -> [Stmt a]
csePass = cseStmt emptyScope where
  cseStmt :: ArityScope -> [Stmt a] -> [Stmt a]
  cseStmt scope (x@Foreign{}:xs) = x:cseStmt scope xs
  cseStmt scope (StmtLet (One b@(v, ty, ex)):xs) =
    let s' = extendPureLets scope [b]
     in StmtLet (One (v, ty, cseTerm scope mempty ex)):cseStmt s' xs
  cseStmt scope (StmtLet (Many bs):xs) =
    let s' = extendPureLets scope bs
        bs' = map (\(var, ty, ex) -> (var, ty, cseTerm scope mempty ex)) bs
     in StmtLet (Many bs'):cseStmt s' xs
  cseStmt scope (d@(Type _ cs):xs) = d:cseStmt (extendPureCtors scope cs) xs
  cseStmt _ [] = []

cseTerm :: forall a. IsVar a => ArityScope -> CseScope a -> Term a -> Term a
cseTerm scope map (Let (One bind@(v, ty, cseTerm scope map -> ex)) body)
  | Just var <- ex `Map.lookup` map = -- eliminate it
    cseTerm scope map $ substitute (Vm.singleton (toVar v) (Ref var ty)) body
  | worthIt ex && isPure scope ex = -- include it for elimination
    let scope' = extendPureLets scope [bind]
        map' = Map.insert ex v map
     in Let (One (v, ty, ex)) (cseTerm scope' map' body)
  | otherwise = -- carry on
    Let (One (v, ty, ex)) (cseTerm (extendPureLets scope [bind]) map body)
cseTerm _ _ x@Atom{} = x
cseTerm _ _ x@Cast{} = x
cseTerm _ _ x@App{} = x
cseTerm scope map (Lam arg body) = Lam arg (cseTerm scope map body)
cseTerm _ _ x@TyApp{} = x
cseTerm _ _ x@Values{} = x
cseTerm _ _ x@Extend{} = x
cseTerm scope map' (Match ex as) = Match ex (map cseArm as) where
  cseArm a = a & armBody %~ cseTerm scope map'
cseTerm scope map' (Let (Many vs) body) =
  let vs' = map (_3 %~ cseTerm scope map') vs
      scope' = extendPureLets scope vs'
   in Let (Many vs') (cseTerm scope' map' body)

worthIt :: Term a -> Bool
worthIt Lam{} = False
worthIt Atom{} = False
worthIt _ = True
