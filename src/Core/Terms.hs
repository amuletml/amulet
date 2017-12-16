module Core.Terms
  ( mapTerm, mapTerm1
  , substitute
  ) where

import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Generics (gmapT, mkT)
import Data.Triple (third3)

import Core.Core
import Syntax

type TupleT a = [(a, CoType, CoTerm)] -> [(a, CoType, CoTerm)]

-- Apply a function to each child in the provided expr. Note this does not
-- apply to all descendants.
mapTerm1 :: (CoTerm -> CoTerm) -> CoTerm -> CoTerm
mapTerm1 f = gmapT (mkT (tupT :: TupleT (Var Resolved))) -- CotLet
           . gmapT (mkT (tupT :: TupleT CoPattern))      -- CotMatch
           . gmapT (mkT (tupT :: TupleT Text))           -- CotExtend
           . gmapT (mkT (map f))                         -- CotBegin
           . gmapT (mkT f) where
  tupT = map (third3 f)


-- Apply a function to all descendants in the provided expr.
mapTerm :: (CoTerm -> CoTerm) -> CoTerm -> CoTerm
mapTerm f = f . mapTerm1 (mapTerm f)

substitute :: M.Map (Var Resolved) CoTerm -> CoTerm -> CoTerm
substitute m = mapTerm subst
  where subst e@(CotRef v _) = fromMaybe e (M.lookup v m)
        subst e = e
