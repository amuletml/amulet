module Core.Simplify
  ( optimise
  ) where

import Core.Optimise.Propagate
import Core.Optimise.Match
import Core.Optimise.Fold
import Core.Optimise

import Control.Monad.Gen

optimise :: [CoStmt] -> Gen Int [CoStmt]
optimise = runTransform . transformStmts passes where
  passes = mconcat . concat . replicate 10 $
      [ dropBranches
      , foldExpr
      , trivialPropag
      , constrPropag
      , matchKnownConstr
      ]
