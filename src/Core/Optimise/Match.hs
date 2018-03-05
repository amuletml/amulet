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
  go (Match (Match ie ibs) obs) = Match ie (map (push obs) ibs)
  go x = x

  push :: [(Pattern (Var Resolved), Type (Var Resolved), Term (Var Resolved))]
       -> (Pattern (Var Resolved), Type (Var Resolved), Term (Var Resolved))
       -> (Pattern (Var Resolved), Type (Var Resolved), Term (Var Resolved))
  push x (p, t, e) = (p, t, Match e x)

matchOfBottom :: TransformPass
matchOfBottom = pass' go where
  go t@(Match e cs)
    | any (isError . thd3) cs && isError e =
      let App (TyApp err _) msg = e
          App (TyApp _ t) _ = maybe (error em) thd3 (Data.List.find (isError . thd3) cs)
       in App (TyApp err t) msg
    | isError e = e
    | otherwise = t
  go x = x

  em = "could not Data.List.find errorring branch even though it exists"
