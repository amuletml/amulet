module Core.Simplify
  ( optimise
  ) where

import Core.Optimise.Propagate
import Core.Optimise.Inline
import Core.Optimise.Match
import Core.Optimise.Fold
import Core.Optimise

import Control.Monad.Gen

optmOnce :: [CoStmt] -> Gen Int [CoStmt]
optmOnce t = fmap (maybe t id) . runTransform . transformStmts passes $ t where
  passes = mconcat
      [ dropBranches
      , foldExpr
      , trivialPropag
      , constrPropag
      , matchKnownConstr
      , matchOfMatch
      , matchOfBottom
      , inlineVariable
      , betaReduce
      , dropUselessLet
      ]

optimise :: [CoStmt] -> Gen Int [CoStmt]
optimise = go 25 where
  go :: Integer -> [CoStmt] -> Gen Int [CoStmt]
  go k sts
    | k > 0 = go (k - 1) =<< optmOnce sts
    | otherwise = pure sts
