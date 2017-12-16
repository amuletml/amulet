module Syntax.Toplevel
  ( extractVars, extractVarsN
  ) where


import Syntax

extractVars :: Toplevel p -> ([Var p], [Var p])
extractVars (LetStmt vs _) = (map (\(x,_,_)->x) vs, [])
extractVars (ForeignVal v _ _ _) = ([v], [])
extractVars (TypeDecl v _ cs _) = (map extractCons cs, [v])
  where extractCons (UnitCon v _) = v
        extractCons (ArgCon v _ _) = v

extractVarsN :: [Toplevel p] -> ([Var p], [Var p])
extractVarsN ts =
    let (vs, tys) = unzip (map extractVars ts)
    in (concat vs, concat tys)
