{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, DeriveDataTypeable #-}
module Core.Occurrence
  ( OccursVar(..)
  , Occurs(..)
  , tagOccurence, doesItOccur
  ) where


import qualified Core.Core as C
import Core.Core hiding (CoAtom, CoTerm, CoPattern, CoType, CoStmt)

import qualified Data.Map.Merge.Strict as Map
import qualified Data.Map.Strict as Map
import Data.VarSet (IsVar(..))
import Data.Generics
import Data.Maybe
import Data.List

import Control.Monad.Infer
import Control.Arrow

import Syntax (Var(..), Resolved)
import Pretty

type CoAtom a = C.CoAtom (OccursVar a)
type CoTerm a = C.CoTerm (OccursVar a)
type CoType a = C.CoType (OccursVar a)
type CoStmt a = C.CoStmt (OccursVar a)

type OccursMap = Map.Map (Var Resolved) Int

data OccursVar v
  = OccursVar { underlying :: v
              , used :: !Int
              }
  deriving (Eq, Show, Ord, Data)

instance IsVar a => IsVar (OccursVar a) where
  toVar = toVar . underlying

instance Pretty a => Pretty (OccursVar a) where
  pretty = pretty . underlying

tagOccurence :: forall a. IsVar a => Env -> [C.CoStmt a] -> [CoStmt a]
tagOccurence env sts = fst (go sts) where
  go :: [C.CoStmt a] -> ([CoStmt a], OccursMap)
  go (CosLet vs:prg) =
    let (prg', free) = go prg
        vs' = tagBindings vs free
        free' = foldMap (\(v, _, e) -> toVar v `Map.delete` countUsages e) vs <> free
     in (CosLet vs':prg', free')
  go (CosForeign v t c:prg) =
    let (prg', free) = go prg
     in (CosForeign (depends v free) (convert t) c:prg', toVar v `Map.delete` free)
  go (t':prg) = first (convert t':) (go prg)
  go [] = ([], maybe mempty (flip Map.singleton 1) (main env))

  main = fmap (toVar . fst) . uncons
       . sortOn key
       . filter isMain
       . Map.keys
       . _values
  isMain (TgName x _) = x == "main"
  isMain _ = False
  key (TgName k _) = k
  key _ = undefined

tagBindings :: forall a. IsVar a => [(a, C.CoType a, C.CoTerm a)] -> OccursMap -> [(OccursVar a, CoType a, CoTerm a)]
tagBindings vs ss =
  let free = foldMap (\(v, _, e) -> toVar v `Map.delete` countUsages e) vs <> ss
      attach :: a -> OccursVar a
      attach v = OccursVar v (fromMaybe 0 (toVar v `Map.lookup` free))
   in map (\(v, t, e) -> (attach v, fmap attach t, tagTerm e)) vs

tagAtom :: IsVar a => C.CoAtom a -> CoAtom a
tagAtom (CoaRef v t) = CoaRef (depends v mempty) (convert t)
tagAtom (CoaLam Small (v, t) e) = CoaLam Small (depends v (countUsages e), convert t) (tagTerm e)
tagAtom (CoaLam Big (v, t) b) = CoaLam Big (OccursVar v 1, convert t) (tagTerm b)
tagAtom (CoaLit l) = CoaLit l

tagTerm :: IsVar a => C.CoTerm a -> CoTerm a
tagTerm (CotLet vs e) = CotLet (tagBindings vs (countUsages e)) (tagTerm e)
tagTerm (CotAtom a) = CotAtom (tagAtom a)
tagTerm (CotApp f x) = CotApp (tagAtom f) (tagAtom x)
tagTerm (CotMatch e bs) = CotMatch (tagAtom e) (map tagArm bs) where
  tagArm (p, t, e) = (fmap (flip depends (countUsages e)) p, convert t, tagTerm e)
tagTerm (CotExtend l rs) = CotExtend (tagAtom l) (map (\(r, t, e) -> (r, convert t, tagAtom e)) rs)
tagTerm (CotTyApp f x) = CotTyApp (tagAtom f) (convert x)

depends :: IsVar a => a -> OccursMap -> OccursVar a
depends v ss = OccursVar v (fromMaybe 0 (toVar v `Map.lookup` ss))

convert :: Functor f => f a -> f (OccursVar a)
convert = fmap (flip OccursVar 1)

class IsVar a => Occurs a where
  usedWhen :: a -> Int

instance IsVar a => Occurs (OccursVar a) where
  usedWhen = used

doesItOccur :: Occurs a => a -> Bool
doesItOccur = (>= 1) . usedWhen

countUsages :: (Data (f a), Data a, IsVar a) => f a -> OccursMap
countUsages = everything merge (mkQ mempty go) where
  merge = Map.merge Map.preserveMissing Map.preserveMissing (Map.zipWithMatched (const (+)))
  go (CoaRef v _) = Map.singleton v 1
  go _ = mempty
