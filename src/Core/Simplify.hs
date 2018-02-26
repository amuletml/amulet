module Core.Simplify
  ( optimise
  ) where

-- import Core.Optimise.Propagate
-- import Core.Optimise.Inline
-- import Core.Optimise.Match
import Core.Optimise.Fold
import Core.Optimise.Reduce
import Core.Optimise

import Control.Monad.Gen

import Syntax (Var(..), Resolved)

import Data.Maybe

optmOnce :: [CoStmt (Var Resolved)] -> Gen Int [CoStmt (Var Resolved)]
optmOnce t = fmap (fromMaybe t) . runTransform . transformStmts passes $ t where
  passes = mconcat
           [ reduceTermPass
           , dropUselessLet
           ]
      -- [ dropBranches
      -- , foldExpr
      -- , trivialPropag
      -- , constrPropag
      -- , matchKnownConstr
      -- , matchOfMatch
      -- , matchOfBottom
      -- , inlineVariable
      -- , betaReduce
      -- , dropUselessLet
      -- -- , complexSafePropag
      -- ]

optimise :: [CoStmt (Var Resolved)] -> Gen Int [CoStmt (Var Resolved)]
optimise = go 25 where
  go :: Integer -> [CoStmt (Var Resolved)] -> Gen Int [CoStmt (Var Resolved)]
  go k sts
    | k > 0 = go (k - 1) =<< optmOnce sts
    | otherwise = pure sts
