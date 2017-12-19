module Core.Simplify
  ( optimise
  ) where

import Core.Optimise.Match
import Core.Optimise.Fold
import Core.Optimise.Eval
import Core.Optimise

optimise :: [CoStmt] -> [CoStmt]
optimise = runTransform . transformStmts passes . peval where
  passes = mconcat [ dropBranches
                   , foldExpr
                   , dropUselessLets

                   , foldExpr
                   , dropUselessLets
                   ]
