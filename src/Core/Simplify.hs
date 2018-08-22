-- | The frontend to the optimiser.
module Core.Simplify
  ( optimise
  ) where

-- import Core.Optimise.Newtype
import Core.Optimise.CommonExpElim
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
optmOnce = passes where
  passes :: [Stmt CoVar] -> Namey [Stmt CoVar]
  passes = foldr1 (>=>)
           [ linted "Reduce" $ pure . reducePass
           , linted "Inline"   inlineVariablePass

           , linted "Dead code" $ pure . deadCodePass
           , linted "Match Join"   matchJoinPass

           , linted "Sinking" $ pure . sinkingPass . tagFreeSet

           , linted "Reduce" $ pure . reducePass
           , linted "CSE" $ pure . csePass
           ]

  linted :: Functor f => String -> ([Stmt CoVar] -> f [Stmt CoVar]) -> [Stmt CoVar] -> f [Stmt CoVar]
  linted pass fn
    | lintPasses
    = fmap (runLint pass =<< checkStmt emptyScope) . fn
    | otherwise = fn

-- | Run the optimiser multiple times over the input core.
optimise :: [Stmt CoVar] -> Namey [Stmt CoVar]
optimise = go 25 . (runLint "Lower" =<< checkStmt emptyScope) where
  go :: Integer -> [Stmt CoVar] -> Namey [Stmt CoVar]
  go k sts
    | k > 0 = go (k - 1) =<< optmOnce sts
    | otherwise = pure sts
