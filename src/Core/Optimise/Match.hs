module Core.Optimise.Match
  ( dropBranches, matchKnownConstr, matchOfMatch, matchOfBottom
  ) where

import qualified Data.Map.Strict as Map
import Data.Triple
import Data.Monoid
import Data.Maybe
import Data.List

import Control.Monad

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
  go :: CoTerm (Var Resolved) -> Trans (CoTerm (Var Resolved))
  go it@(CotMatch e ptrns) = do
    weCan <- canWe e
    pure $ if weCan
              then fromMaybe it (doIt e ptrns)
              else it
  go x = pure x

  canWe :: CoTerm (Var Resolved) -> Trans Bool
  canWe (CotRef v _) = isCon v
  canWe (CotApp f _) = canWe f
  canWe (CotTyApp x _) = canWe x
  canWe (CotExtend t xs) = do
    t' <- canWe t
    xs' <- traverse (canWe . thd3) xs
    pure (t' && and xs')
  canWe CotLit{} = pure True
  canWe CotLam{} = pure True
  canWe _ = pure False

  doIt :: CoTerm (Var Resolved)
       -> [(CoPattern (Var Resolved), CoType (Var Resolved), CoTerm (Var Resolved))]
       -> Maybe (CoTerm (Var Resolved))
  doIt x ((p, _, k):ps)
    | Just binds <- match p (stripTyApp x) = 
      if not (isError k)
         then Just (CotLet binds k)
         else Nothing
    | otherwise = doIt x ps
  doIt _ _ = Nothing

  match :: CoPattern (Var Resolved)
        -> CoTerm (Var Resolved)
        -> Maybe [(Var Resolved, CoType (Var Resolved), CoTerm (Var Resolved))]
  match p (CotLam Big _ t) = match p t
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
    rows = concat <$> zipWithM mr xs' ys'
    mr (t, p) (t', e)
      | t == t' = match p e
      | otherwise = Nothing
  match (CopLit l) (CotLit l')
    | l == l' = Just []
    | otherwise = Nothing
  match _ _ = Nothing

matchOfMatch :: TransformPass
matchOfMatch = pass' go where
  go (CotMatch (CotMatch ie ibs) obs) = CotMatch ie (map (push obs) ibs)
  go x = x

  push :: [(CoPattern (Var Resolved), CoType (Var Resolved), CoTerm (Var Resolved))]
       -> (CoPattern (Var Resolved), CoType (Var Resolved), CoTerm (Var Resolved))
       -> (CoPattern (Var Resolved), CoType (Var Resolved), CoTerm (Var Resolved))
  push x (p, t, e) = (p, t, CotMatch e x)

matchOfBottom :: TransformPass
matchOfBottom = pass' go where
  go t@(CotMatch e cs)
    | any (isError . thd3) cs && isError e =
      let CotApp (CotTyApp err _) msg = e
          CotApp (CotTyApp _ t) _ = maybe (error em) thd3 (Data.List.find (isError . thd3) cs)
       in CotApp (CotTyApp err t) msg
    | isError e = e
    | otherwise = t
  go x = x

  em = "could not Data.List.find errorring branch even though it exists"
