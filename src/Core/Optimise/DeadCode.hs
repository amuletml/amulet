{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Core.Optimise.DeadCode ( deadCodePass ) where

import qualified Data.VarSet as VarSet
import Data.Triple
import Data.List

import Control.Applicative
import Control.Lens

import Core.Optimise
import Core.Arity

deadCodePass :: IsVar a => [Stmt a] -> [Stmt a]
deadCodePass = snd . freeS emptyScope Nothing where
  freeS :: IsVar a => ArityScope -> Maybe a -> [Stmt a] -> (VarSet.Set, [Stmt a])
  freeS _ (Just m) [] = (VarSet.singleton (toVar m), mempty)
  freeS _ Nothing  [] = (mempty, mempty)

  freeS s m (x@(Foreign v _ _):xs) =
    let (fxs, xs') = freeS s m xs
     in if toVar v `VarSet.member` fxs
           then (toVar v `VarSet.delete` fxs, x:xs')
           else (fxs, xs')
  freeS s m (StmtLet vs:xs) =
    let m' = find (\x -> case toVar x of
                           CoVar _ "main" _ -> True
                           _ -> False) (map fst3 vs)
        s' = extendPureFuns s vs
    in case uncurry (buildLet s' vs) (freeS s' (m <|> m') xs) of
         -- If we've no bindings, just return the primary expression
         (f, [], xs') -> (f, xs')
         -- Otherwise emit as normal
         (f, vs', xs') -> (f, StmtLet vs':xs')

  freeS s m (x@(Type _ cases):xs) =
    let s' = extendPureCtors s cases
    in (x:) <$> freeS s' m xs

  freeA :: IsVar a => ArityScope -> Atom a -> (VarSet.Set, Atom a)
  freeA _ t@(Ref v _)= (VarSet.singleton (toVar v), t)
  freeA _ t@Lit{} = (mempty, t)
  freeA s (Lam a b) =
    let (fb, b') = freeT s b
     in (argVar a `VarSet.delete` fb, Lam a b')

  freeT :: IsVar a => ArityScope -> Term a -> (VarSet.Set, Term a)
  freeT s (Atom a) = Atom <$> freeA s a
  freeT s (App f a) = App <$> freeA s f <*> freeA s a
  freeT s (TyApp f t) = TyApp <$> freeA s f <*> pure t
  freeT s (Cast f t) = Cast <$> freeA s f <*> pure t
  freeT s (Extend t rs) = Extend <$> freeA s t <*> traverse (third3A (freeA s)) rs
  freeT s (Let (One vs@(v, ty, e)) b) =
    let s' = extendPureFuns s [vs]
        (fe, e') = freeT s e
        (fb, b') = freeT s' b
    in if isPure s' e' && toVar v `VarSet.notMember` fb
       then (fb, b')
       else (fe <> fb, Let (One (v, ty, e')) b')

  freeT s (Let (Many vs) b) =
    let s' = extendPureFuns s vs in
    case uncurry (buildLet s' vs) (freeT s' b) of
      -- If we've no bindings, just return the primary expression
      (f, [], b') -> (f, b')
      -- Otherwise emit as normal
      (f, vs', b') -> (f , Let (Many vs') b')

  freeT s (Match t bs) =
    let (ft, t') = freeA s t
        (fbs, bs') = unzip $ map (armBody %%~ freeT s) bs
        pbs = map (VarSet.fromList . map (toVar . fst) . view armVars) bs

        matchFree = mconcat (zipWith VarSet.difference fbs pbs)
    in case (pbs, fbs,  bs') of
         -- If we've got a single pattern match with nothing captured, then inline
         ([pb], [fb], [b])
           | VarSet.isEmpty (VarSet.intersection pb fb) -> (matchFree, b ^. armBody)
         -- Otherwise assume everything is used
         _ -> (ft <> matchFree, Match t' bs')

  buildFrees :: IsVar a => ArityScope -> VarSet.Set -> [(VarSet.Set, (a, Type a, Term a))]
            -> Bool -> [(VarSet.Set, (a, Type a, Term a))] -> VarSet.Set
  buildFrees s free [] True rest = buildFrees s free rest False []
  buildFrees _ free [] False _   = free
  buildFrees s free (b@(fs, (v, _, d)):bs) change rest
    -- If we're pure then we can safely skip this
    | isPure s d && toVar v `VarSet.notMember` free
    = buildFrees s free bs change (b:rest)
    -- Otherwise (we're used or impure) then extend the free set
    | otherwise
    = buildFrees s (toVar v `VarSet.insert` free `VarSet.union` fs) bs True rest

  buildLet s vs frees rest =
    let binds = map (\(v,t,e) -> (v,t,) <$> freeT s e) vs
        letFree = buildFrees s frees binds False []
        termFree = foldr (VarSet.delete . toVar . fst3 . snd) letFree binds
        vs' = filter ((`VarSet.member` letFree) . toVar . fst3) (map snd binds)
    in (termFree, vs', rest)
