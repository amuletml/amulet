module Core.Optimise.Fold
  ( foldExpr
  ) where

import qualified Data.Map.Strict as Map

import Control.Monad.Reader

import Core.Optimise

--- Folds simple variable accesses
foldExpr :: TransformPass
foldExpr = afterPass pass where
  pass :: CoTerm -> TransM CoTerm
  pass e@(CotRef v _) = do
    env <- vars <$> ask
    case Map.lookup v env of
      Just d@(CotRef _ _) -> pure d
      Just d@(CotLit _) -> pure d
      _ -> pure e

  pass e = pure e
