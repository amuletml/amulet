module Core.Optimise.Transform
  ( TransformPass, beforePass, afterPass, zipPass
  , transformTerm, mapTerm1
  , dropBranches
  ) where

import qualified Data.Map.Strict as M

import Core.Core
import Core.Terms

data TransformPass = TransformPass
                     { before :: (CoTerm -> CoTerm)
                     , after :: (CoTerm -> CoTerm) }

beforePass, afterPass :: (CoTerm -> CoTerm) -> TransformPass
beforePass fn = TransformPass { before = fn, after = id }
afterPass  fn = TransformPass { before = id, after = fn }

zipPass :: [TransformPass] -> TransformPass
zipPass ps = TransformPass { before = foldr (.) id (map before ps)
                           , after = foldr (.) id (map after ps) }

transformTerm :: TransformPass -> CoTerm -> CoTerm
transformTerm pass = after pass . mapTerm1 (transformTerm pass) . before pass


-- Attempts to simplify match expression, dropping redundant branches and
-- replacing matches with flat expressions where possible.
dropBranches :: TransformPass
dropBranches = beforePass pass where
  pass (CotMatch e ptrns) =
    case reducePatterns ptrns of
      [(CopCapture v, ty, bod)] ->
        case e of
          CotRef _ _ -> substitute (M.singleton v e) bod
          _ -> CotLet [(v, ty, e)] bod
      ptrns' -> CotMatch e ptrns'
  pass e = e

  reducePatterns [] = []
  reducePatterns (p@(CopCapture _, _, _):_) = [p]
  reducePatterns (p:xs) = p : reducePatterns xs
