module Core.Simplify
  ( optimise
  ) where

import Core.Optimise.Match
import Core.Optimise
import Core.Optimise.Fold

optimise :: [CoStmt] -> [CoStmt]
optimise = runTransform . transformStmts (mconcat [ dropBranches
                                                  , foldExpr
                                                  ])
