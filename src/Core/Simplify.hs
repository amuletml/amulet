module Core.Simplify
  ( optimise
  ) where

import Core.Optimise.Propagate
import Core.Optimise.Inline
import Core.Optimise.Match
import Core.Optimise.Fold
import Core.Optimise

import Control.Monad.Gen

optmOnce :: [CoStmt] -> Gen Int ([CoStmt], Integer)
optmOnce = runTransform . transformStmts passes where
  passes = mconcat . concat . replicate 10 $
      [ dropBranches
      , foldExpr
      , trivialPropag
      , constrPropag
      , matchKnownConstr
      , inlineVariable
      , betaReduce
      , dropUselessLet
      ]

optimise :: [CoStmt] -> Gen Int [CoStmt]
optimise = go 10 where
  go :: Integer -> [CoStmt] -> Gen Int [CoStmt]
  go k sts
    | k > 0 = do
      (sts', changes) <- optmOnce sts
      if changes == 0
        then pure sts
        else go (k - 1) sts'
    | otherwise = pure sts
