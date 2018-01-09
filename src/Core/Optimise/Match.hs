module Core.Optimise.Match
  ( dropBranches, matchKnownConstr
  ) where

import qualified Data.Map.Strict as Map
import Data.Triple
import Data.Monoid
import Data.Maybe
import Data.List

import Syntax (Var, Resolved)
import Core.Optimise

-- Attempts to simplify match expression, dropping redundant branches and
-- replacing matches with flat expressions where possible.
dropBranches :: TransformPass
dropBranches = pass' pass where
  pass (CotMatch e ptrns) =
    case reducePatterns ptrns of
      [(CopCapture v _, ty, bod)] ->
        case e of
          CotRef _ _ -> substitute (Map.singleton v e) bod
          _ -> CotLet [(v, ty, e)] bod
      ptrns' -> CotMatch e ptrns'
  pass e = e

  reducePatterns [] = []
  reducePatterns (p@(CopCapture _ _, _, _):_) = [p]
  reducePatterns (p:xs) = p : reducePatterns xs

matchKnownConstr :: TransformPass
matchKnownConstr = pass go where
  go :: CoTerm -> Trans CoTerm
  go it@(CotMatch e ptrns) = do
    weCan <- canWe e
    pure $ if weCan
              then fromMaybe it (doIt e ptrns)
              else it
  go x = pure x

  canWe :: CoTerm -> Trans Bool
  canWe (CotRef v _) = isCon v
  canWe (CotApp f _) = canWe f
  canWe (CotTyApp x _) = canWe x
  canWe (CotExtend t xs) = do
    t' <- canWe t
    xs' <- mapM (canWe . thd3) xs
    pure (t' && and xs')
  canWe _ = pure False

  doIt :: CoTerm -> [(CoPattern, CoType, CoTerm)] -> Maybe CoTerm
  doIt x ((p, _, k):ps)
    | Just binds <- match p (stripTyApp x) = Just (CotLet binds k)
    | otherwise = doIt x ps
  doIt _ _ = Nothing

  match :: CoPattern -> CoTerm -> Maybe [(Var Resolved, CoType, CoTerm)]
  match (CopCapture v t) x = Just [(v, t, x)]
  match (CopConstr v) (CotRef v' _)
    | v == v' = Just []
    | otherwise = Nothing
  match (CopDestr v p) (CotApp (CotRef v' _) x)
    | v == v' = match p x
    | otherwise = Nothing
  match (CopExtend p xs) (CotExtend e ys) = match p e <> rows where
    xs' = sortOn fst xs
    ys' = sortOn fst (map (\(x, _, y) -> (x, y)) ys)
    rows = fmap concat $ sequence (zipWith mr xs' ys')
    mr (t, p) (t', e)
      | t == t' = match p e
      | otherwise = Nothing
  match (CopLit l) (CotLit l')
    | l == l' = Just []
    | otherwise = Nothing
  match _ _ = Nothing
