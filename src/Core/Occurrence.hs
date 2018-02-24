{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module Core.Occurrence
  ( OccursVar(..)
  , Occurs(..)
  , tagOccurence
  ) where


import qualified Core.Core as C
import Core.Core hiding (CoTerm, CoPattern, CoType, CoStmt)

import qualified Data.Map.Strict as Map
import qualified Data.VarSet as VarSet
import Data.VarSet (IsVar(..))
import Data.Triple
import Data.List

import Control.Monad.Infer
import Control.Arrow

import Syntax (Var(..))
import Pretty

type CoTerm a = C.CoTerm (OccursVar a)
type CoType a = C.CoType (OccursVar a)
type CoStmt a = C.CoStmt (OccursVar a)

data OccursVar v
  = OccursVar { underlying :: v
              , used :: Bool
              }
  deriving (Eq, Show, Ord)

instance IsVar a => IsVar (OccursVar a) where
  toVar = toVar . underlying

instance Pretty a => Pretty (OccursVar a) where
  pretty = pretty . underlying

tagOccurence :: forall a. IsVar a => Env -> [C.CoStmt a] -> [CoStmt a]
tagOccurence env sts = fst (go sts) where
  go :: [C.CoStmt a] -> ([CoStmt a], VarSet.Set)
  go (CosLet vs:prg) =
    let (prg', free) = go prg
        vs' = tagBindings vs free
     in (CosLet vs':prg', foldMap (freeIn . thd3) vs <> free)
  go (t':prg) = first (fmap (flip OccursVar False) t':) (go prg)
  go [] = ([], VarSet.singleton (main env))

  main = toVar . head
       . sortOn key
       . filter isMain
       . Map.keys
       . _values
  isMain (TgName x _) = x == "main"
  isMain _ = False
  key (TgName k _) = k
  key _ = undefined

tagBindings :: forall a. IsVar a => [(a, C.CoType a, C.CoTerm a)] -> VarSet.Set -> [(OccursVar a, CoType a, CoTerm a)]
tagBindings vs ss =
  let free = foldMap (\(v, _, e) -> toVar v `VarSet.delete` freeIn e) vs <> ss
      attach :: a -> OccursVar a
      attach v = OccursVar v (toVar v `VarSet.member` free)
   in map (\(v, t, e) -> (attach v, fmap attach t, tagTerm e)) vs

tagTerm :: IsVar a => C.CoTerm a -> CoTerm a
tagTerm (CotLet vs e) = CotLet (tagBindings vs (freeIn e)) (tagTerm e)
tagTerm (CotRef v t) = CotRef (depends v mempty) (convert t)
tagTerm (CotLam Small (v, t) e) = CotLam Small (depends v (freeIn e), convert t) (tagTerm e)
tagTerm (CotLam Big (v, t) b) = CotLam Big (OccursVar v True, convert t) (tagTerm b)
tagTerm (CotApp f x) = CotApp (tagTerm f) (tagTerm x)
tagTerm (CotMatch e bs) = CotMatch (tagTerm e) (map tagArm bs) where
  tagArm (p, t, e) = (fmap (flip depends (freeIn e)) p, convert t, tagTerm e)
tagTerm (CotLit l) = CotLit l
tagTerm (CotExtend l rs) = CotExtend (tagTerm l) (map (\(r, t, e) -> (r, convert t, tagTerm e)) rs)
tagTerm (CotTyApp f x) = CotTyApp (tagTerm f) (convert x)

depends :: IsVar a => a -> VarSet.Set -> OccursVar a
depends v ss = OccursVar v (toVar v `VarSet.member` ss)

convert :: Functor f => f a -> f (OccursVar a)
convert = fmap (flip OccursVar True)

class IsVar a => Occurs a where
  doesItOccur :: a -> Bool

instance IsVar a => Occurs (OccursVar a) where
  doesItOccur = used
