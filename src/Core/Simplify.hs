-- | The frontend to the optimiser.
module Core.Simplify
  ( optimise
  ) where

import Data.List

import Core.Optimise.Newtype
import Core.Optimise.DeadCode
import Core.Optimise.Sinking
import Core.Optimise.Joinify
import Core.Optimise.Inline
import Core.Optimise.Reduce
import Core.Optimise

import Core.Free
import Core.Lint

import Control.Monad.Namey
import Control.Monad

lintPasses :: Bool
lintPasses = True

optmOnce :: [Stmt CoVar] -> Namey [Stmt CoVar]
optmOnce = passes <=< killNewtypePass where
  passes = foldr1 (>=>) $ linted
           [ pure

           , pure . reducePass
           , inlineVariablePass

           , pure . deadCodePass
           , matchJoinPass

           , pure . sinkingPass . tagFreeSet

           , pure . reducePass
           , pure
           ]

  linted
    | lintPasses
    = intersperse (pure . (runLint =<< checkStmt emptyScope))
    | otherwise = id

-- | Run the optimiser multiple times over the input core.
optimise :: [Stmt CoVar] -> Namey [Stmt CoVar]
optimise = go 25 where
  go :: Integer -> [Stmt CoVar] -> Namey [Stmt CoVar]
  go k sts
    | k > 0 = go (k - 1) . (runLint =<< checkStmt emptyScope) =<< optmOnce sts
    | otherwise = pure sts
