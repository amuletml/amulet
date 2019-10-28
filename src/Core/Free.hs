{-# LANGUAGE ScopedTypeVariables, ExplicitNamespaces #-}

-- | Annotate terms with which variables are free within them
module Core.Free
  ( tagFreeSet, freeSet
  , tagFreeStmt, tagFreeTerm
  ) where

import Control.Lens

import Core.Core as C
import Core.Var

import qualified Data.VarSet as VarSet
import Data.Triple

-- | The set of free variables with in a list of statements
freeSet :: IsVar a => [AnnStmt b a] -> VarSet.Set
freeSet = fst . tagFreeStmt const const

-- | Tag a list of statements with their free variables
tagFreeSet :: IsVar a => [AnnStmt b a] -> [AnnStmt VarSet.Set a]
tagFreeSet = snd . tagFreeStmt (flip const) const

-- | Tag some statements with free variable information
tagFreeStmt :: forall a a' b b'. IsVar a
            => (b -> VarSet.Set -> b') -- ^ Build a new annotation from the set of free variables
            -> (a -> Bool -> a')       -- ^ Build a new variable from its occurrence.
            -> [AnnStmt b a]           -- ^ The statements to annotate
            -> (VarSet.Set, [AnnStmt b' a'])
tagFreeStmt ann var = tagStmt where
  conv :: Type a -> Type a'
  conv = fmap (`var` True)
  var' v = var v . VarSet.member (toVar v)

  tagStmt :: [AnnStmt b a] -> (VarSet.Set, [AnnStmt b' a'])
  tagStmt [] = (mempty, [])
  tagStmt (Foreign v ty txt:xs) =
    let (fv, xs') = tagStmt xs
    in ( toVar v `VarSet.delete` fv
       , Foreign (var' v fv) (conv ty) txt:xs')
  tagStmt (Type v tys:xs) =
    let (fv, xs') = tagStmt xs
    in ( fv
       , Type (var v True) (map (tagCons fv) tys):xs') where
      tagCons fv (cons, ty) = (var' cons fv, conv ty)
  tagStmt (StmtLet (One (v, ty, e)):xs) =
    let (fve, e') = tagFreeTerm ann var e
        (fvr, xs') = tagStmt xs
        fv = fve <> VarSet.delete (toVar v) fvr
    in (fv, StmtLet (One (var' v fvr, conv ty, e')):xs')
  tagStmt (StmtLet (Many vs):xs) =
    let (fvvs, vs') = unzip (map (tagFreeTerm ann var . thd3) vs)
        (fvr, xs') = tagStmt xs

        fvs = mconcat (fvr : fvvs)
        fv = foldr (VarSet.delete . toVar . fst3) fvs vs
    in (fv, StmtLet (Many (zipWith (\(v, ty, _) e -> (var' v fvs, conv ty, e)) vs vs')):xs')

-- | Tag some statements with free variable information
tagFreeTerm :: forall a a' b b'. IsVar a
            => (b -> VarSet.Set -> b')  -- ^ Build a new annotation from the set of free variables
            -> (a -> Bool -> a')        -- ^ Build a new variable from its occurrence.
            -> AnnTerm b a              -- ^ The term to annotate
            -> (VarSet.Set, AnnTerm b' a')
tagFreeTerm ann var = tagTerm where
  conv :: Functor f => f a -> f a'
  conv = fmap (`var` True)

  var' v = var v . VarSet.member (toVar v)

  tagAtom (Lit l) = (mempty, Lit l)
  tagAtom (Ref a ty) = (VarSet.singleton (toVar a)
                       , Ref (var a True) (conv ty))

  tagTerm (AnnAtom an a) =
    let (fv, a') = tagAtom a
    in (fv, AnnAtom (ann an fv) a')

  tagTerm (AnnApp an f x) =
    let (fvf, f') = tagAtom f
        (fvx, x') = tagAtom x
        fv = fvf <> fvx
    in (fv, AnnApp (ann an fv) f' x')

  tagTerm (AnnLam an (TermArgument arg ty) bod) =
    let (fv, bod') = tagTerm bod
        fv' = VarSet.delete (toVar arg) fv
    in ( fv
       , AnnLam (ann an fv') (TermArgument (var' arg fv) (conv ty)) bod')
  tagTerm (AnnLam an (TypeArgument arg ty) bod) =
    let (fv, bod') = tagTerm bod
    in ( fv
       , AnnLam (ann an fv) (TypeArgument (var arg True) (conv ty)) bod')

  tagTerm (AnnLet an (One (v, ty, e)) r) =
    let (fve, e') = tagTerm e
        (fvr, r') = tagTerm r
        fv = fve <> VarSet.delete (toVar v) fvr
    in (fv, AnnLet (ann an fv) (One (var' v fvr, conv ty, e')) r')
  tagTerm (AnnLet an (Many vs) r) =
    let (fvvs, vs') = unzip (map (tagTerm . thd3) vs)
        (fvr, r') = tagTerm r

        fvs = mconcat (fvr : fvvs)
        fv = foldr (VarSet.delete . toVar . fst3) fvs vs
    in (fv, AnnLet (ann an fv) (Many (zipWith (\(v, ty, _) e -> (var' v fvs, conv ty, e)) vs vs')) r')

  tagTerm (AnnMatch an t bs) =
    let (fvt, t') = tagAtom t
        (ftbs, bs') = unzip (map tagPtrn bs)
        fv = mconcat (fvt : ftbs)
    in (fv, AnnMatch (ann an fv) t' bs') where
      tagPtrn a@Arm { _armPtrn = p, _armBody = b, _armVars = pv } =
        let (fvb, b') = tagTerm b
            p' = flip var' fvb <$> p
        in (foldr (VarSet.delete . toVar . fst) fvb pv
           , Arm { _armPtrn = p'
                 , _armTy = conv (a ^. armTy)
                 , _armBody = b'
                 , _armVars = map (\(v, ty) -> (var' v fvb, conv ty)) pv
                 , _armTyvars = map (\(v, ty) -> (var v True, conv ty)) (a ^. armTyvars)
                 })

  tagTerm (AnnExtend an f fs) =
    let (fvf, f') = tagAtom f
        (fvfs, fs') = unzip (map (\(n, ty, a) ->
                                     let (fva, a') = tagAtom a
                                     in (fva, (n, conv ty, a'))) fs)
        fv = mconcat (fvf : fvfs)
    in (fv, AnnExtend (ann an fv) f' fs')

  tagTerm (AnnValues an xs) =
    let (fvfs, xs') = unzip (map tagAtom xs)
        fv = mconcat fvfs
    in (fv, AnnValues (ann an fv) xs')

  tagTerm (AnnTyApp an f ty) =
    let (fv, f') = tagAtom f
    in (fv, AnnTyApp (ann an fv) f' (conv ty))

  tagTerm (AnnCast an x to co) =
    let (fv, x') = tagAtom x
    in (fv, AnnCast (ann an fv) x' (conv to) (conv co))
