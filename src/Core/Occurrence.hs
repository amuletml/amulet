{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, DeriveDataTypeable, ExplicitNamespaces #-}
module Core.Occurrence
  ( OccursVar(..)
  , Occurs(..)
  , Occurrence(..)
  , tagOccursVar, tagOccursMap
  , tagOccurStmt, tagOccurTerm
  , doesItOccur
  ) where


import Core.Core as C

import qualified Data.VarMap as VarMap
import Data.VarSet (IsVar(..))
import Data.Semigroup
import Data.Generics
import Data.Triple

import Pretty

data Occurrence = Dead | Once | OnceLambda | Multi | MultiLambda
  deriving (Show, Eq, Ord, Data)

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

type OccursMap = VarMap.Map Occurrence

data OccursVar v
  = OccursVar { underlying :: v
              , used :: !Occurrence
              }
  deriving (Eq, Show, Ord, Data)

instance IsVar a => IsVar (OccursVar a) where
  toVar = toVar . underlying
  fromVar = flip OccursVar MultiLambda . fromVar

instance Pretty a => Pretty (OccursVar a) where
  pretty = pretty . underlying

tagOccursVar :: IsVar a => [AnnStmt b a] -> [AnnStmt b (OccursVar a)]
tagOccursVar = snd . tagOccurStmt const OccursVar

tagOccursMap :: IsVar a => [AnnStmt b a] -> [AnnStmt OccursMap a]
tagOccursMap = snd . tagOccurStmt (flip const) const

tagOccurStmt :: forall a a' b b'. (IsVar a, IsVar a')
             => (b -> OccursMap -> b')
             -> (a -> Occurrence -> a')
             -> [AnnStmt b a] -> (OccursMap, [AnnStmt b' a'])
tagOccurStmt ann var = tagStmt where
  conv = fmap (`var` defOcc)

  var' v m = case VarMap.lookup (toVar v) m of
               Nothing -> var v Dead
               Just x -> var v x

  tagStmt :: [AnnStmt b a] -> (OccursMap, [AnnStmt b' a'])
  tagStmt [] = (mempty, [])
  tagStmt (Foreign v ty txt:xs) =
    let (fv, xs') = tagStmt xs
    in ( toVar v `VarMap.delete` fv
       , Foreign (var' v fv) (conv ty) txt:xs')
  tagStmt (Type v tys:xs) =
    let (fv, xs') = tagStmt xs
    in ( fv
       , Type (var v defOcc) (map (tagCons fv) tys):xs') where
      tagCons fv (cons, ty) = (var' cons fv, conv ty)
  tagStmt (StmtLet vs:xs) =
    let (fvxs, xs') = tagStmt xs
        (fvvs, vs') = unzip (map (tagOccurTerm ann var . thd3) vs)

        fvs = occConcat (fvxs : fvvs)
        fv = foldr (VarMap.delete . toVar . fst3) fvs vs
    in (fv, StmtLet (zipWith (\(v, ty, _) e -> (var' v fvs, conv ty, e)) vs vs'):xs')

tagOccurTerm :: forall a a' b b' . (IsVar a, IsVar a')
             => (b -> OccursMap -> b')
             -> (a -> Occurrence -> a')
             -> AnnTerm b a -> (OccursMap, AnnTerm b' a')
tagOccurTerm ann var = tagTerm where
  conv :: Functor f => f a -> f a'
  conv = fmap (`var` defOcc)

  var' v m = case VarMap.lookup (toVar v) m of
               Nothing -> var v Dead
               Just x -> var v x

  tagAtom (Lit l) = (mempty, Lit l)
  tagAtom (Ref a ty) = (VarMap.singleton (toVar a) Once
                       , Ref (var a defOcc) (conv ty))
  tagAtom (Lam (TermArgument arg ty) bod) =
    let (fv, bod') = tagTerm bod
    in (VarMap.map escapeLambda (VarMap.delete (toVar arg) fv)
       , Lam (TermArgument (var' arg fv) (conv ty)) bod')
  tagAtom (Lam (TypeArgument arg ty) bod) =
    let (fv, bod') = tagTerm bod
    in (VarMap.map escapeLambda fv
       , Lam (TypeArgument (var arg MultiLambda) (conv ty)) bod')

  tagTerm (AnnAtom an a) =
    let (fv, a') = tagAtom a
    in (fv, AnnAtom (ann an fv) a')

  tagTerm (AnnApp an f x) =
    let (fvf, f') = tagAtom f
        (fvx, x') = tagAtom x
        fv = fvf # fvx
    in (fv, AnnApp (ann an fv) f' x')

  tagTerm (AnnLet an (One (v, ty, e)) r) =
    let (fve, e') = tagTerm e
        (fvr, r') = tagTerm r
        fv = fve # VarMap.delete (toVar v) fvr
    in (fv, AnnLet (ann an fv) (One (var' v fvr, conv ty, e')) r')
  tagTerm (AnnLet an (Many vs) r) =
    let (fvvs, vs') = unzip (map (tagTerm . thd3) vs)
        (fvr, r') = tagTerm r

        fvs = occConcat (fvr : fvvs)
        fv = foldr (VarMap.delete . toVar . fst3) fvs vs
    in (fv, AnnLet (ann an fv) (Many (zipWith (\(v, ty, _) e -> (var' v fvs, conv ty, e)) vs vs')) r')

  tagTerm (AnnMatch an t bs) =
    let (fvt, t') = tagAtom t
        (ftbs, bs') = unzip (map tagPtrn bs)
        fv = occConcat (fvt : ftbs)
    in (fv, AnnMatch (ann an fv) t' bs') where
      tagPtrn (p, ty, b) =
        let (fvb, b') = tagTerm b
            p' = flip var' fvb <$> p
            pv = patternVarsA p :: [a]
        in (foldr (VarMap.delete . toVar) fvb pv, (p', conv ty, b'))

  tagTerm (AnnExtend an f fs) =
    let (fvf, f') = tagAtom f
        (fvfs, fs') = unzip (map (\(n, ty, a) ->
                                     let (fva, a') = tagAtom a
                                     in (fva, (n, conv ty, a'))) fs)
        fv = occConcat (fvf : fvfs)
    in (fv, AnnExtend (ann an fv) f' fs')

  tagTerm (AnnTyApp an f ty) =
    let (fv, f') = tagAtom f
    in (fv, AnnTyApp (ann an fv) f' (conv ty))

  tagTerm (AnnCast an x co) =
    let (fv, x') = tagAtom x
    in (fv, AnnCast (ann an fv) x' (conv co))

class IsVar a => Occurs a where
  usedWhen :: a -> Occurrence

instance IsVar a => Occurs (OccursVar a) where
  usedWhen = used

doesItOccur :: Occurs a => a -> Bool
doesItOccur = (/= Dead) . usedWhen

(#) :: OccursMap -> OccursMap -> OccursMap
(#) = VarMap.unionSemigroup

occConcat :: [OccursMap] -> OccursMap
occConcat [] = mempty
occConcat (x:xs) = foldr (#) x xs
