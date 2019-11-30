{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable,
   ExplicitNamespaces, DeriveGeneric, DeriveAnyClass #-}

{- | Similar to "Core.Free", but also tracking /how/ a variable is
   used. Namely, we annotate variables and bindings with 'Occurrence'.
-}
module Core.Occurrence
  ( OccursVar(..)
  , Occurs(..)
  , Occurrence(..), OccursMap
  , occursSet
  , tagOccursVar, tagOccursMap
  , tagOccurStmt, tagOccurTerm
  , doesItOccur, occurrenceIn
  ) where

import Control.Lens hiding ((#))

import Core.Core as C
import Core.Var

import qualified Data.VarMap as VarMap
import qualified Data.VarSet as VarSet
import Data.Triple
import Data.Maybe
import Data.Data

import Text.Pretty.Semantic

import Data.Hashable
import GHC.Generics

-- | The occurrence of a variable
data Occurrence
  = Dead        -- ^ This variable is never used
  | Once        -- ^ This variable is only used once
  | OnceLambda  -- ^ This variable is used once and is captured by a lambda
  | Multi       -- ^ This variable is used multiple times (or in multiple cases)
  | MultiLambda -- ^ This variable is used multiple times and is captured by a lambda
  deriving (Show, Eq, Ord, Data, Generic, Hashable)

defOcc :: Occurrence
defOcc = MultiLambda

escapeLambda :: Occurrence -> Occurrence
escapeLambda Once = OnceLambda
escapeLambda Multi = MultiLambda
escapeLambda x = x

instance Semigroup Occurrence where
  x <> Dead = x
  Dead <> x = x

  Once <> Once = Multi
  Once <> Multi = Multi
  Multi <> Once = Multi
  Multi <> Multi = Multi

  _ <> _ = MultiLambda

instance Monoid Occurrence where
  mempty = Dead
  mappend = (<>)

-- | A mapping of variables to occurrences. If a variable does not occur
-- in this map it can be assumed 'Dead'.
type OccursMap = VarMap.Map Occurrence

data OccursVar v
  = OccursVar { underlying :: v
              , used :: !Occurrence
              }
  deriving (Eq, Show, Ord, Data, Generic, Hashable)

instance IsVar a => IsVar (OccursVar a) where
  toVar = toVar . underlying
  fromVar = flip OccursVar MultiLambda . fromVar

instance Pretty a => Pretty (OccursVar a) where
  pretty = pretty . underlying

-- | Tag each variable with it's 'Occurrence' /at the binding point/.
tagOccursVar :: IsVar a => VarSet.Set -> [AnnStmt b a] -> [AnnStmt b (OccursVar a)]
tagOccursVar exp = snd . tagOccurStmt const OccursVar exp

-- | Tag each expression with its free variables and their occurrence.
tagOccursMap :: IsVar a => VarSet.Set -> [AnnStmt b a] -> [AnnStmt OccursMap a]
tagOccursMap exp = snd . tagOccurStmt (flip const) const exp

-- | Compute the occurrence set from an 'OccursMap'.
occursSet :: OccursMap -> VarSet.Set
occursSet = VarMap.foldrWithKey ins mempty where
  ins _ Dead s = s
  ins k _    s = VarSet.insert k s

-- | Tag some statements with occurrence information
tagOccurStmt :: forall a a' b b'. IsVar a
             => (b -> OccursMap -> b')  -- ^ Build a new annotation from the set of free variables
             -> (a -> Occurrence -> a') -- ^ Build a new variable from its occurrence.
             -> VarSet.Set              -- ^ Always used variables. i.e. those which are exported.
             -> [AnnStmt b a]           -- ^ The statements to tag
             -> (OccursMap, [AnnStmt b' a'])
tagOccurStmt ann var export = tagStmt where
  var' v = var v . occurrenceIn v

  tagStmt :: [AnnStmt b a] -> (OccursMap, [AnnStmt b' a'])
  tagStmt [] = (VarSet.foldr (`VarMap.insert` Multi) mempty export, [])
  tagStmt (RawCode c:xs) =
    let (fv, xs') = tagStmt xs
     in (fv, RawCode c:xs')
  tagStmt (Foreign v ty txt:xs) =
    let (fv, xs') = tagStmt xs
    in ( toVar v `VarMap.delete` fv
       , Foreign (var' v fv) ty txt:xs')
  tagStmt (Type v tys:xs) =
    let (fv, xs') = tagStmt xs
    in ( fv
       , Type (var v defOcc) (map (tagCons fv) tys):xs') where
      tagCons fv (cons, ty) = (var' cons fv, ty)
  tagStmt (StmtLet (One (v, ty, e)):xs) =
    let (fve, e') = tagOccurTerm ann var e
        (fvr, xs') = tagStmt xs
        fv = fve # VarMap.delete (toVar v) fvr
    in (fv, StmtLet (One (var' v fvr, ty, e')):xs')
  tagStmt (StmtLet (Many vs):xs) =
    let (fvvs, vs') = unzip (map (tagOccurTerm ann var . thd3) vs)
        (fvr, xs') = tagStmt xs

        fvs = occConcat (fvr : fvvs)
        fv = foldr (VarMap.delete . toVar . fst3) fvs vs
    in (fv, StmtLet (Many (zipWith (\(v, ty, _) e -> (var' v fvs, ty, e)) vs vs')):xs')

-- | Tag a term with occurrence information
tagOccurTerm :: forall a a' b b'. IsVar a
             => (b -> OccursMap -> b')  -- ^ Build a new annotation from the set of free variables
             -> (a -> Occurrence -> a') -- ^ Build a new variable from its occurrence.
             -> AnnTerm b a             -- ^ The term to tag
             -> (OccursMap, AnnTerm b' a')
tagOccurTerm ann var = tagTerm where
  var' v = var v . occurrenceIn v

  tagAtom (Lit l) = (mempty, Lit l)
  tagAtom (Ref a ty) = ( VarMap.singleton (toVar a) Once
                       , Ref a ty)

  tagTerm (AnnAtom an a) =
    let (fv, a') = tagAtom a
    in (fv, AnnAtom (ann an fv) a')

  tagTerm (AnnApp an f x) =
    let (fvf, f') = tagAtom f
        (fvx, x') = tagAtom x
        fv = fvf # fvx
    in (fv, AnnApp (ann an fv) f' x')

  tagTerm (AnnLam an (TermArgument arg ty) bod) =
    let (fv, bod') = tagTerm bod
        fv' = VarMap.map escapeLambda (VarMap.delete (toVar arg) fv)
    in ( fv'
       , AnnLam (ann an fv') (TermArgument (var' arg fv) ty) bod')
  tagTerm (AnnLam an (TypeArgument arg ty) bod) =
    let (fv, bod') = tagTerm bod
        fv' = VarMap.map escapeLambda fv
    in ( fv'
       , AnnLam (ann an fv') (TypeArgument (var arg MultiLambda) ty) bod')

  tagTerm (AnnLet an (One (v, ty, e)) r) =
    let (fve, e') = tagTerm e
        (fvr, r') = tagTerm r
        fv = fve # VarMap.delete (toVar v) fvr
    in (fv, AnnLet (ann an fv) (One (var' v fvr, ty, e')) r')
  tagTerm (AnnLet an (Many vs) r) =
    let (fvvs, vs') = unzip (map (tagTerm . thd3) vs)
        (fvr, r') = tagTerm r

        fvs = occConcat (fvr : fvvs)
        fv = foldr (VarMap.delete . toVar . fst3) fvs vs
    in (fv, AnnLet (ann an fv) (Many (zipWith (\(v, ty, _) e -> (var' v fvs, ty, e)) vs vs')) r')

  tagTerm (AnnMatch an t bs) =
    let (fvt, t') = tagAtom t
        (ftbs, bs') = unzip (map tagPtrn bs)
        fv = occConcat (fvt : ftbs)
    in (fv, AnnMatch (ann an fv) t' bs') where
      tagPtrn a@Arm { _armPtrn = p, _armBody = b, _armVars = pv } =
        let (fvb, b') = tagTerm b
            p' = flip var' fvb <$> p
        in (foldr (VarMap.delete . toVar . fst) fvb pv
           , Arm { _armPtrn = p'
                 , _armTy = a ^. armTy
                 , _armBody = b'
                 , _armVars = map (\(v, ty) -> (var' v fvb, ty)) pv
                 , _armTyvars = map (\(v, ty) -> (var v defOcc, ty)) (a ^. armTyvars)
                 })

  tagTerm (AnnExtend an f fs) =
    let (fvf, f') = tagAtom f
        (fvfs, fs') = unzip (map (\(n, ty, a) ->
                                     let (fva, a') = tagAtom a
                                     in (fva, (n, ty, a'))) fs)
        fv = occConcat (fvf : fvfs)
    in (fv, AnnExtend (ann an fv) f' fs')

  tagTerm (AnnValues an xs) =
    let (fvfs, xs') = unzip (map tagAtom xs)
        fv = occConcat fvfs
    in (fv, AnnValues (ann an fv) xs')

  tagTerm (AnnTyApp an f ty) =
    let (fv, f') = tagAtom f
    in (fv, AnnTyApp (ann an fv) f' ty)

  tagTerm (AnnCast an x to co) =
    let (fv, x') = tagAtom x
    in (fv, AnnCast (ann an fv) x' to co)

-- | An extension of 'IsVar' which also tracks occurrence information
class IsVar a => Occurs a where
  -- | How this variable is used
  usedWhen :: a -> Occurrence

instance IsVar a => Occurs (OccursVar a) where
  usedWhen = used

-- | Does this variable occur /somewhere/.
doesItOccur :: Occurs a => a -> Bool
doesItOccur = (/= Dead) . usedWhen

(#) :: OccursMap -> OccursMap -> OccursMap
(#) = VarMap.unionSemigroup

occConcat :: [OccursMap] -> OccursMap
occConcat [] = mempty
occConcat (x:xs) = foldr (#) x xs

-- | Lookup a variable in an 'OccursMap', returning 'Dead' if it is not
-- used.
occurrenceIn :: IsVar a => a -> OccursMap -> Occurrence
occurrenceIn v m = fromMaybe Dead $ VarMap.lookup (toVar v) m
