module Core.Optimise
  ( optimise
  ) where

import Core.Optimise.Transform
import Core.Core

optimise :: [CoStmt] -> [CoStmt]
optimise = map optimiseStmt where
  optimiseStmt (CosLet vs) = CosLet (map (\(v, t, e) -> (v, t, optimiseTerm e)) vs)
  optimiseStmt s = s

optimiseTerm :: CoTerm -> CoTerm
optimiseTerm = transformTerm (zipPass [dropBranches
                                      ])
