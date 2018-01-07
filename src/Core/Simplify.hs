module Core.Simplify
  ( optimise
  , optimiseOnce
  ) where

import Control.Monad.Gen

import Core.Optimise.Propagate
import Core.Optimise.Match
import Core.Optimise.Fold
import Core.Optimise

optimise :: [CoStmt] -> Gen Int [CoStmt]
optimise = optimiseTil 10 where
  optimiseTil :: Int -> [CoStmt] -> Gen Int [CoStmt]
  optimiseTil n stmts
    | n <= 0 = pure stmts
    | otherwise = do
      stmts' <- optimiseOnce stmts
      if stmts == stmts'
        then optimiseTil (n - 1) stmts'
        else pure stmts'

optimiseOnce :: [CoStmt] -> Gen Int [CoStmt]
optimiseOnce = runTransform . transformStmts passes where
  passes = mconcat [ dropBranches
      , foldExpr
      , trivialPropag
      , constrPropag
      , matchKnownConstr
      ]
