module Core.Simplify
  ( optimise
  ) where

import Core.Optimise.Match
import Core.Optimise

optimise :: [CoStmt] -> [CoStmt]
optimise = map optimiseStmt where
  optimiseStmt (CosLet vs) = CosLet (map (\(v, t, e) -> (v, t, optimiseTerm e)) vs)
  optimiseStmt s = s

optimiseTerm :: CoTerm -> CoTerm
optimiseTerm = transformTerm (mconcat [dropBranches
                                      ])
