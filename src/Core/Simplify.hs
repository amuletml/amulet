module Core.Simplify
  ( optimise
  ) where

import Data.List

-- import Core.Optimise.Newtype
import Core.Optimise.DeadCode
import Core.Optimise.Sinking
import Core.Optimise.Joinify
import Core.Optimise.Inline
import Core.Optimise.Reduce
import Core.Optimise

import Core.Free
import Core.Lint

import Control.Monad.Gen
import Control.Monad

lintPasses :: Bool
lintPasses = True

optmOnce :: [Stmt CoVar] -> Gen Int [Stmt CoVar]
optmOnce = passes where
  passes = foldr1 (>=>) $ linted
           [ pure

           , pure . reducePass
           , inlineVariablePass

           , pure . deadCodePass
           , matchJoinPass
           -- , killNewtypePass

           , pure . sinkingPass . tagFreeSet

           , pure . reducePass
           , pure
           ]

  linted
    | lintPasses
    = intersperse (pure . (runLint =<< checkStmt emptyScope))
    | otherwise = id

optimise :: [Stmt CoVar] -> Gen Int [Stmt CoVar]
optimise = go 25 where
  go :: Integer -> [Stmt CoVar] -> Gen Int [Stmt CoVar]
  go k sts
    | k > 0 = go (k - 1) . (runLint =<< checkStmt emptyScope) =<< optmOnce sts
    | otherwise = pure sts
