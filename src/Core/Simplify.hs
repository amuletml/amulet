module Core.Simplify
  ( optimise
  ) where

import Core.Optimise.Inline
import Core.Optimise.Match
import Core.Optimise.Fold
import Core.Optimise

optimise :: [CoStmt] -> [CoStmt]
optimise = runTransform . transformStmts
  (mconcat [ dropBranches
           , foldExpr
           , dropUselessLets
           , inlineOnce

           , foldExpr
           , dropUselessLets
           ])
