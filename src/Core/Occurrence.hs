{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, DeriveDataTypeable, ExplicitNamespaces #-}
module Core.Occurrence
  ( OccursVar(..)
  , Occurs(..)
  , tagOccurence, doesItOccur
  ) where


import Core.Core as C

import qualified Data.Map.Merge.Strict as Map
import qualified Data.Map.Strict as Map
import Data.VarSet (IsVar(..))
import Data.Generics
import Data.Maybe
import Data.List
import Data.Triple

import Control.Monad.Infer
import Control.Arrow

import Syntax (Var(..), Resolved)
import Pretty

type OccAtom a = C.Atom (OccursVar a)
type OccTerm a = C.Term (OccursVar a)
type OccType a = C.Type (OccursVar a)
type OccStmt a = C.Stmt (OccursVar a)

type OccursMap = Map.Map (Var Resolved) Int

data OccursVar v
  = OccursVar { underlying :: v
              , used :: !Int
              }
  deriving (Eq, Show, Ord, Data)

instance IsVar a => IsVar (OccursVar a) where
  toVar = toVar . underlying
  fromVar = flip OccursVar 1 . fromVar

instance Pretty a => Pretty (OccursVar a) where
  pretty = pretty . underlying

tagOccurence :: forall a. IsVar a => Env -> [C.Stmt a] -> [OccStmt a]
tagOccurence env sts = fst (go sts) where
  go :: [C.Stmt a] -> ([OccStmt a], OccursMap)
  go (StmtLet vs:prg) =
    let (prg', free) = go prg
        vs' = tagBindings vs free
        free' = foldMap (\(v, _, e) -> toVar v `Map.delete` countUsages e) vs <> free
     in (StmtLet vs':prg', free')
  go (Foreign v t c:prg) =
    let (prg', free) = go prg
     in (Foreign (depends v free) (convert t) c:prg', toVar v `Map.delete` free)
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

tagBindings :: forall a. IsVar a => [(a, C.Type a, C.Term a)] -> OccursMap -> [(OccursVar a, OccType a, OccTerm a)]
tagBindings vs ss =
  let free = foldMap (\(v, _, e) -> toVar v `Map.delete` countUsages e) vs <> ss
      attach :: a -> OccursVar a
      attach v = OccursVar v (fromMaybe 0 (toVar v `Map.lookup` free))
   in map (\(v, t, e) -> (attach v, fmap attach t, tagOccTerm e)) vs

tagOccAtom :: IsVar a => C.Atom a -> OccAtom a
tagOccAtom (Ref v t) = Ref (depends v mempty) (convert t)
tagOccAtom (Lam Small (v, t) e) = Lam Small (depends v (countUsages e), convert t) (tagOccTerm e)
tagOccAtom (Lam Big (v, t) b) = Lam Big (OccursVar v 1, convert t) (tagOccTerm b)
tagOccAtom (Lit l) = Lit l

tagOccTerm :: IsVar a => C.Term a -> OccTerm a
tagOccTerm (Let vs e) = Let (tagBindings vs (countUsages e)) (tagOccTerm e)
tagOccTerm (Atom a) = Atom (tagOccAtom a)
tagOccTerm (App f x) = App (tagOccAtom f) (tagOccAtom x)
tagOccTerm (Match e bs) = Match (tagOccAtom e) (map tagArm bs) where
  tagArm (p, t, e) = (fmap (flip depends (countUsages e)) p, convert t, tagOccTerm e)
tagOccTerm (Extend l rs) = Extend (tagOccAtom l) (map (\(r, t, e) -> (r, convert t, tagOccAtom e)) rs)
tagOccTerm (TyApp f x) = TyApp (tagOccAtom f) (convert x)
tagOccTerm (Cast f x) = Cast (tagOccAtom f) (convert x)

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

countUsages :: forall a. IsVar a => C.Term a -> OccursMap
countUsages = term where
  term (Atom a) = atom a
  term (App f x) = atom f # atom x
  term (Let vs e) = foldr (#) mempty (map (term . thd3) vs) # term e
  term (Match e vs) = atom e # foldr (#) mempty (map (term . thd3) vs)
  term (Extend e rs) = atom e # foldr (#) mempty (map (atom . thd3) rs)
  term (TyApp f _) = atom f
  term (Cast f _) = atom f

  atom (Ref v _) = Map.singleton (toVar v) 1
  atom (Lam _ _ b) = term b
  atom Lit{} = mempty

  (#) :: OccursMap -> OccursMap -> OccursMap
  (#) = Map.merge Map.preserveMissing Map.preserveMissing (Map.zipWithMatched (const (+)))
