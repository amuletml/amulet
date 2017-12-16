module Core.Optimise
  ( mapTerm, mapTerm1
  , substitute
  , module Core.Core
  , Var(..)
  , TransformPass(..)
  , beforePass, afterPass, transformTerm
  ) where

import qualified Data.Map.Strict as Map
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

substitute :: Map.Map (Var Resolved) CoTerm -> CoTerm -> CoTerm
substitute m = mapTerm subst
  where subst e@(CotRef v _) = fromMaybe e (Map.lookup v m)
        subst e = e

data TransformPass
  = TransformPass { before :: CoTerm -> CoTerm
                  , after :: CoTerm -> CoTerm }

instance Monoid TransformPass where
  mempty = TransformPass id id
  mappend (TransformPass b a) (TransformPass b' a') = TransformPass (b . b') (a . a')
beforePass, afterPass :: (CoTerm -> CoTerm) -> TransformPass
beforePass fn = TransformPass { before = fn, after = id }
afterPass  fn = TransformPass { before = id, after = fn }

transformTerm :: TransformPass -> CoTerm -> CoTerm
transformTerm pass = after pass . mapTerm1 (transformTerm pass) . before pass
