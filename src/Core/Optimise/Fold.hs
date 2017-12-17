module Core.Optimise.Fold
  ( foldExpr
  , dropUselessLets
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Data.Triple
import Data.Maybe

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

dropUselessLets :: TransformPass
dropUselessLets = afterPass' go where
  go (CotLet gr1 e)
    | Set.null (Set.fromList (map fst3 gr1) `Set.intersection` freeIn e) =
      case mapMaybe (keep . thd3) gr1 of
        [] -> e
        xs -> CotBegin xs e
    | otherwise = CotLet gr1 e
  go e = e

  keep CotLit{} = Nothing
  keep CotRef{} = Nothing
  keep x = Just x
