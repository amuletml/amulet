module Core.Simplify
  ( optimise
  ) where

import Core.Optimise.DeadCode
import Core.Optimise.Newtype
import Core.Optimise.Inline
import Core.Optimise.Reduce
import Core.Optimise.Sinking
import Core.Optimise

import Core.Free

import Control.Monad.Gen
import Control.Monad

import Syntax (Var(..), Resolved)

optmOnce :: [Stmt (Var Resolved)] -> Gen Int [Stmt (Var Resolved)]
optmOnce = passes where
  passes = foldr1 (>=>)
           [ pure

           , pure . reducePass
           , inlineVariablePass

           , pure . deadCodePass
           , killNewtypePass
           , pure . sinkingPass . tagFreeSet

           , pure . reducePass
           ]

optimise :: [Stmt (Var Resolved)] -> Gen Int [Stmt (Var Resolved)]
optimise = go 25 where
  go :: Integer -> [Stmt (Var Resolved)] -> Gen Int [Stmt (Var Resolved)]
  go k sts
    | k > 0 = go (k - 1) =<< optmOnce sts
    | otherwise = pure sts
