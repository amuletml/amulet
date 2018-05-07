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

import Pretty
import System.IO.Unsafe

lintPasses :: Bool
lintPasses = True

optmOnce :: Int -> [Stmt CoVar] -> Gen Int [Stmt CoVar]
optmOnce k = passes where
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
    -- = intersperse (pure . (runLint =<< checkStmt emptyScope))
    = zipWith (\i f x -> (runLint =<< checkStmt emptyScope) . dump i <$> f x) ([0..] :: [Int])
    | otherwise = id

  {-# NOINLINE dump #-}
  dump i x = unsafePerformIO $ do
    let ok = case runLintOK (checkStmt emptyScope x) of
               Left _ -> "er"
               Right _ -> "ok"
    writeFile ("dump_" ++ show k ++"_" ++ show i ++ "_" ++ ok ++ ".ml") (show $ pretty x)
    pure x

optimise :: [Stmt CoVar] -> Gen Int [Stmt CoVar]
optimise = go 25 where
  go :: Int -> [Stmt CoVar] -> Gen Int [Stmt CoVar]
  go k sts
    | k > 0 = go (k - 1) =<< optmOnce k sts
    | otherwise = pure sts
