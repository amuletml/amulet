module Core.Simplify
  ( optimise
  , optimiseOnce
  ) where

import Core.Optimise.Match
import Core.Optimise.Fold
import Core.Optimise.Eval
import Core.Optimise

optimise :: [CoStmt] -> [CoStmt]
optimise = optimiseTil 10 where
  optimiseTil :: Int -> [CoStmt] -> [CoStmt]
  optimiseTil n stmts | n <= 0 = stmts
                      | otherwise = let stmts' = optimiseOnce stmts
                                    in if stmts == stmts'
                                       then optimiseTil (n - 1) stmts'
                                       else  stmts'

optimiseOnce :: [CoStmt] -> [CoStmt]
optimiseOnce = runTransform . transformStmts passes . peval where
  passes = mconcat [ dropBranches
                   , foldExpr
                   ]
