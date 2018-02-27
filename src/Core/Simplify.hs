module Core.Simplify
  ( optimise
  ) where

import Core.Optimise.Inline
import Core.Optimise.Reduce
import Core.Optimise

import Control.Monad.Gen

import Syntax (Var(..), Resolved)

optmOnce :: [CoStmt (Var Resolved)] -> Gen Int [CoStmt (Var Resolved)]
optmOnce = pure . passes where
  passes = foldr (.) id $ reverse
           [ id
           , reduceTermPass
           , inlineVariablePass
           ]

optimise :: [CoStmt (Var Resolved)] -> Gen Int [CoStmt (Var Resolved)]
optimise = go 25 where
  go :: Integer -> [CoStmt (Var Resolved)] -> Gen Int [CoStmt (Var Resolved)]
  go k sts
    | k > 0 = go (k - 1) =<< optmOnce sts
    | otherwise = pure sts
