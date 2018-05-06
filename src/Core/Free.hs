{-# LANGUAGE ScopedTypeVariables, ExplicitNamespaces #-}
module Core.Free
  ( tagFreeSet, freeSet
  , tagFreeStmt, tagFreeTerm
  ) where

import Control.Lens

import Core.Core as C
import Core.Var

import qualified Data.VarSet as VarSet
import Data.Triple

freeSet :: IsVar a => [AnnStmt b a] -> VarSet.Set
freeSet = fst . tagFreeStmt const const

tagFreeSet :: IsVar a => [AnnStmt b a] -> [AnnStmt VarSet.Set a]
tagFreeSet = snd . tagFreeStmt (flip const) const

tagFreeStmt :: forall a a' b b'. (IsVar a, IsVar a')
             => (b -> VarSet.Set -> b')
             -> (a -> Bool -> a')
             -> [AnnStmt b a] -> (VarSet.Set, [AnnStmt b' a'])
tagFreeStmt ann var = tagStmt where
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
  tagStmt (StmtLet vs:xs) =
    let (fvxs, xs') = tagStmt xs
        (fvvs, vs') = unzip (map (tagFreeTerm ann var . thd3) vs)

        fvs = mconcat (fvxs : fvvs)
        fv = foldr (VarSet.delete . toVar . fst3) fvs vs
    in (fv, StmtLet (zipWith (\(v, ty, _) e -> (var' v fvs, conv ty, e)) vs vs'):xs')

tagFreeTerm :: forall a a' b b' . (IsVar a, IsVar a')
             => (b -> VarSet.Set -> b')
             -> (a -> Bool -> a')
             -> AnnTerm b a -> (VarSet.Set, AnnTerm b' a')
tagFreeTerm ann var = tagTerm where
  conv :: Functor f => f a -> f a'
  conv = fmap (`var` True)

  var' v = var v . VarSet.member (toVar v)

  tagAtom (Lit l) = (mempty, Lit l)
  tagAtom (Ref a ty) = (VarSet.singleton (toVar a)
                       , Ref (var a True) (conv ty))
  tagAtom (Lam (TermArgument arg ty) bod) =
    let (fv, bod') = tagTerm bod
    in (VarSet.delete (toVar arg) fv
       , Lam (TermArgument (var' arg fv) (conv ty)) bod')
  tagAtom (Lam (TypeArgument arg ty) bod) =
    let (fv, bod') = tagTerm bod
    in (fv
       , Lam (TypeArgument (var arg True) (conv ty)) bod')

  tagTerm (AnnAtom an a) =
    let (fv, a') = tagAtom a
    in (fv, AnnAtom (ann an fv) a')

  tagTerm (AnnApp an f x) =
    let (fvf, f') = tagAtom f
        (fvx, x') = tagAtom x
        fv = fvf <> fvx
    in (fv, AnnApp (ann an fv) f' x')

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
      tagPtrn (a@Arm { _armPtrn = p, _armBody = b, _armVars = pv }) =
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

  tagTerm (AnnTyApp an f ty) =
    let (fv, f') = tagAtom f
    in (fv, AnnTyApp (ann an fv) f' (conv ty))

  tagTerm (AnnCast an x co) =
    let (fv, x') = tagAtom x
    in (fv, AnnCast (ann an fv) x' (conv co))
