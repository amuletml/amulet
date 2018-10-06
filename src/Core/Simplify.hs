-- | The frontend to the optimiser.
module Core.Simplify
  ( optimise
  ) where

import Core.Optimise.CommonExpElim
import Core.Optimise.Newtype
import Core.Optimise.DeadCode
import Core.Optimise.Sinking
import Core.Optimise.Reduce
import Core.Optimise.Unbox
import Core.Optimise

import Core.Free
import Core.Lint

import Control.Monad.Namey
import Control.Monad

lintPasses :: Bool
lintPasses = False

optmOnce :: [Stmt CoVar] -> Namey [Stmt CoVar]
optmOnce = passes where
  passes :: [Stmt CoVar] -> Namey [Stmt CoVar]
  passes = foldr1 (>=>)
           [ linted "Reduce" reducePass
           , linted "Dead code" $ pure . deadCodePass
           , linted "Unboxer"  unboxPass

           , linted "Sinking" $ pure . sinkingPass . tagFreeSet

           , linted "CSE" (pure . csePass)
           , linted "Reduce #2" reducePass
           ]

linted :: Monad f => String -> ([Stmt CoVar] -> f [Stmt CoVar]) -> [Stmt CoVar] -> f [Stmt CoVar]
linted pass fn
  | lintPasses
  = fmap (runLint pass =<< checkStmt emptyScope) . fn
  | otherwise = fn

-- | Run the optimiser multiple times over the input core.
optimise :: [Stmt CoVar] -> Namey [Stmt CoVar]
optimise = go 10 <=< prepasses . if lintPasses then runLint "Lower" =<< checkStmt emptyScope else id where
  go :: Integer -> [Stmt CoVar] -> Namey [Stmt CoVar]
  go k sts
    | k > 0 = go (k - 1) =<< optmOnce sts
    | otherwise = pure sts

  prepasses :: [Stmt CoVar] -> Namey [Stmt CoVar]
  prepasses = linted "Newtype" killNewtypePass
