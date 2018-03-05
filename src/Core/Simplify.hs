module Core.Simplify
  ( optimise
  ) where

import Core.Optimise.DeadCode
import Core.Optimise.Newtype
import Core.Optimise.Inline
import Core.Optimise.Reduce
import Core.Optimise

import Control.Monad.Gen

import Syntax (Var(..), Resolved)

optmOnce :: [Stmt (Var Resolved)] -> Gen Int [Stmt (Var Resolved)]
optmOnce = pure . passes where
  passes = foldr (.) id $ reverse
           [ id
           , reducePass
           , inlineVariablePass
           , deadCodePass
           , killNewtypePass
           ]

optimise :: [Stmt (Var Resolved)] -> Gen Int [Stmt (Var Resolved)]
optimise = go 25 where
  go :: Integer -> [Stmt (Var Resolved)] -> Gen Int [Stmt (Var Resolved)]
  go k sts
    | k > 0 = go (k - 1) =<< optmOnce sts
    | otherwise = pure sts
