-- | The frontend to the optimiser.
{-# LANGUAGE ScopedTypeVariables #-}
module Core.Simplify
  ( optimise
  ) where

import Core.Optimise.CommonExpElim
import Core.Optimise.DeadCode
import Core.Optimise.Sinking
import Core.Optimise.Reduce
import Core.Optimise.Uncurry
import Core.Optimise

import Core.Free
import Core.Lint

import Control.Monad.Namey
import Control.Monad

optmOnce :: forall m. Monad m => Bool -> [Stmt CoVar] -> NameyT m [Stmt CoVar]
optmOnce lint = passes where
  passes :: [Stmt CoVar] -> NameyT m [Stmt CoVar]
  passes = foldr1 (>=>)
           [ linting "Reduce" reducePass
           , linting "Dead code" $ pure . deadCodePass
           , linting "Uncurry" uncurryPass

           , linting "Sinking" $ pure . sinkingPass . tagFreeSet

           , linting "CSE" (pure . csePass)
           , linting "Reduce #2" reducePass
           ]
  linting = if lint then linted else flip const

linted :: Monad f => String -> ([Stmt CoVar] -> f [Stmt CoVar]) -> [Stmt CoVar] -> f [Stmt CoVar]
linted pass fn = fmap (runLint pass =<< checkStmt emptyScope) . fn

-- | Run the optimiser multiple times over the input core.
optimise :: forall m. Monad m => Bool -> [Stmt CoVar] -> NameyT m [Stmt CoVar]
optimise lint = go 10 <=< linting "Lower" pure where
  go :: Integer -> [Stmt CoVar] -> NameyT m [Stmt CoVar]
  go k sts
    | k > 0 = go (k - 1) =<< optmOnce lint sts
    | otherwise = pure sts

  linting = if lint then linted else flip const
