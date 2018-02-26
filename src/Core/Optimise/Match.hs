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
