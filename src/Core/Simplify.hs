-- | The frontend to the optimiser.
{-# LANGUAGE ScopedTypeVariables #-}
module Core.Simplify
  ( OptimiseInfo(..), defaultInfo
  , optimise
  ) where

import Control.Monad.Namey
import Control.Monad

import Data.Functor

import Core.Optimise.CommonExpElim
import Core.Optimise.DeadCode
import Core.Optimise.Sinking
import Core.Optimise.Reduce
import Core.Optimise.Uncurry
import Core.Optimise

import Core.Free
import Core.Lint

defaultInfo :: OptimiseInfo
defaultInfo = OptimiseInfo mempty True

optmOnce :: forall m. Monad m => OptimiseInfo -> [Stmt CoVar] -> NameyT m [Stmt CoVar]
optmOnce info = passes where
  passes :: [Stmt CoVar] -> NameyT m [Stmt CoVar]
  passes = foldr1 (>=>)
           [ linting "Reduce" reducePass
           , linting' "Dead code" deadCodePass
           , linting "Uncurry" (const uncurryPass)

           , linting "Sinking" (const (pure . sinkingPass . tagFreeSet))

           , linting "CSE" (const (pure . csePass))
           , linting "Reduce #2" reducePass
           ]

  linting :: Functor f
          => String -> (OptimiseInfo -> [Stmt CoVar] -> f [Stmt CoVar])
          -> [Stmt CoVar] -> f [Stmt CoVar]
  linting name fn prog = fn info prog
    <&> (\prog' -> if useLint info
                   then runLint name (checkStmt emptyScope prog') prog'
                   else prog')

  linting' name pass = linting name (\i -> pure . pass i)

-- | Run the optimiser multiple times over the input core.
optimise :: forall m. Monad m => OptimiseInfo -> [Stmt CoVar] -> NameyT m [Stmt CoVar]
optimise info prog = do
  when (useLint info) (runLint "Lowered" (checkStmt emptyScope prog) (pure ()))
  go 10 prog
  where
    go :: Integer -> [Stmt CoVar] -> NameyT m [Stmt CoVar]
    go k sts
      | k > 0 = go (k - 1) =<< optmOnce info sts
      | otherwise = pure sts
