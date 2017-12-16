module Core.Optimise.Match
  ( dropBranches
  ) where

import qualified Data.Map.Strict as Map

import Core.Optimise

-- Attempts to simplify match expression, dropping redundant branches and
-- replacing matches with flat expressions where possible.
dropBranches :: TransformPass
dropBranches = beforePass pass where
  pass (CotMatch e ptrns) =
    case reducePatterns ptrns of
      [(CopCapture v, ty, bod)] ->
        case e of
          CotRef _ _ -> substitute (Map.singleton v e) bod
          _ -> CotLet [(v, ty, e)] bod
      ptrns' -> CotMatch e ptrns'
  pass e = e

  reducePatterns [] = []
  reducePatterns (p@(CopCapture _, _, _):_) = [p]
  reducePatterns (p:xs) = p : reducePatterns xs
