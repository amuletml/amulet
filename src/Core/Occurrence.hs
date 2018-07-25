{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable, ExplicitNamespaces #-}

{- | Similar to "Core.Free", but also tracking /how/ a variable is
   used. Namely, we annotate variables and bindings with 'Occurrence'.
-}
module Core.Occurrence
  ( OccursVar(..)
  , Occurs(..)
  , Occurrence(..), OccursMap
  , tagOccursVar, tagOccursMap
  , tagOccurStmt, tagOccurTerm
  , doesItOccur, occurrenceIn
  ) where

import Control.Lens hiding ((#))

import Core.Core as C
import Core.Var

import qualified Data.VarMap as VarMap
import Data.Triple
import Data.Maybe
import Data.Data

import Text.Pretty.Semantic

-- | The occurrence of a variable
data Occurrence
  = Dead        -- ^ This variable is never used
  | Once        -- ^ This variable is only used once
  | OnceLambda  -- ^ This variable is used once and is captured by a lambda
  | Multi       -- ^ This variable is used multiple times (or in multiple cases)
  | MultiLambda -- ^ This variable is used multiple times and is captured by a lambda
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
  deriving (Eq, Show, Ord, Data)

instance IsVar a => IsVar (OccursVar a) where
  toVar = toVar . underlying
  fromVar = flip OccursVar MultiLambda . fromVar

instance Pretty a => Pretty (OccursVar a) where
  pretty = pretty . underlying

-- | Tag each variable with it's 'Occurrence' /at the binding point/.
tagOccursVar :: IsVar a => [AnnStmt b a] -> [AnnStmt b (OccursVar a)]
tagOccursVar = snd . tagOccurStmt const OccursVar

-- | Tag each expression with its free variables and their occurrence.
tagOccursMap :: IsVar a => [AnnStmt b a] -> [AnnStmt OccursMap a]
tagOccursMap = snd . tagOccurStmt (flip const) const

-- | Tag some statements with occurrence information
tagOccurStmt :: forall a a' b b'. (IsVar a, IsVar a')
             => (b -> OccursMap -> b')  -- ^ Build a new annotation from the set of free variables
             -> (a -> Occurrence -> a') -- ^ Build a new variable from its occurrence.
             -> [AnnStmt b a]           -- ^ The statements to tag
             -> (OccursMap, [AnnStmt b' a'])
tagOccurStmt ann var = tagStmt where
  conv = fmap (`var` defOcc)
  var' v = var v . occurrenceIn v

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
  tagStmt (StmtLet (One (v, ty, e)):xs) =
    let (fve, e') = tagOccurTerm ann var e
        (fvr, xs') = tagStmt xs
        fv = fve # VarMap.delete (toVar v) fvr
    in (fv, StmtLet (One (var' v fvr, conv ty, e')):xs')
  tagStmt (StmtLet (Many vs):xs) =
    let (fvvs, vs') = unzip (map (tagOccurTerm ann var . thd3) vs)
        (fvr, xs') = tagStmt xs

        fvs = occConcat (fvr : fvvs)
        fv = foldr (VarMap.delete . toVar . fst3) fvs vs
    in (fv, StmtLet (Many (zipWith (\(v, ty, _) e -> (var' v fvs, conv ty, e)) vs vs')):xs')

-- | Tag a term with occurrence information
tagOccurTerm :: forall a a' b b'. (IsVar a, IsVar a')
             => (b -> OccursMap -> b')  -- ^ Build a new annotation from the set of free variables
             -> (a -> Occurrence -> a') -- ^ Build a new variable from its occurrence.
             -> AnnTerm b a             -- ^ The term to tag
             -> (OccursMap, AnnTerm b' a')
tagOccurTerm ann var = tagTerm where
  conv :: Functor f => f a -> f a'
  conv = fmap (`var` defOcc)

  var' v = var v . occurrenceIn v

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
      tagPtrn a@Arm { _armPtrn = p, _armBody = b, _armVars = pv } =
        let (fvb, b') = tagTerm b
            p' = flip var' fvb <$> p
        in (foldr (VarMap.delete . toVar . fst) fvb pv
           , Arm { _armPtrn = p'
                 , _armTy = conv (a ^. armTy)
                 , _armBody = b'
                 , _armVars = map (\(v, ty) -> (var' v fvb, conv ty)) pv
                 , _armTyvars = map (\(v, ty) -> (var v defOcc, conv ty)) (a ^. armTyvars)
                 })

  tagTerm (AnnExtend an f fs) =
    let (fvf, f') = tagAtom f
        (fvfs, fs') = unzip (map (\(n, ty, a) ->
                                     let (fva, a') = tagAtom a
                                     in (fva, (n, conv ty, a'))) fs)
        fv = occConcat (fvf : fvfs)
    in (fv, AnnExtend (ann an fv) f' fs')

  tagTerm (AnnValues an xs) =
    let (fvfs, xs') = unzip (map tagAtom xs)
        fv = mconcat fvfs
    in (fv, AnnValues (ann an fv) xs')

  tagTerm (AnnTyApp an f ty) =
    let (fv, f') = tagAtom f
    in (fv, AnnTyApp (ann an fv) f' (conv ty))

  tagTerm (AnnCast an x co) =
    let (fv, x') = tagAtom x
    in (fv, AnnCast (ann an fv) x' (conv co))

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
