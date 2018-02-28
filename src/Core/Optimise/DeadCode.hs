{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Core.Optimise.DeadCode ( deadCodePass ) where

import qualified Data.VarMap as VarMap
import qualified Data.VarSet as VarSet
import qualified Data.Map as Map
import Data.VarSet (IsVar(..))
import Data.Text (Text)
import Data.Semigroup
import Data.Triple
import Data.Maybe
import Data.List

import Control.Applicative

import Core.Optimise

newtype DeadScope = DeadScope { pureArity :: VarMap.Map Int }
  deriving (Show)

deadCodePass :: IsVar a => [CoStmt a] -> [CoStmt a]
deadCodePass = snd . freeS (DeadScope mempty) Nothing where
  freeS :: IsVar a => DeadScope -> Maybe a -> [CoStmt a] -> (VarSet.Set, [CoStmt a])
  freeS _ (Just m) [] = (VarSet.singleton (toVar m), mempty)
  freeS _ Nothing  [] = (mempty, mempty)

  freeS s m (x@(CosForeign v _ _):xs) =
    let (fxs, xs') = freeS s m xs
     in if toVar v `VarSet.member` fxs
           then (toVar v `VarSet.delete` fxs, x:xs')
           else (fxs, xs')
  freeS s m (CosLet vs:xs) =
    let m' = find (\x -> case toVar x of
                           TgName "main" _ -> True
                           _ -> False) (map fst3 vs)
        s' = extendPureFuns s vs
    in case uncurry (buildLet s' vs) (freeS s' (m <|> m') xs) of
         -- If we've no bindings, just return the primary expression
         (f, [], xs') -> (f, xs')
         -- Otherwise emit as normal
         (f, vs', xs') -> (f, CosLet vs':xs')

  freeS s m (x@(CosType _ cases):xs) =
    let s' = s { pureArity = foldr (\(v, ty) p -> VarMap.insert (toVar v) (1 + typeArity ty) p)
                             (pureArity s) cases }
    in (x:) <$> freeS s' m xs

  freeA :: IsVar a => DeadScope -> CoAtom a -> (VarSet.Set, CoAtom a)
  freeA _ t@(CoaRef v _)= (VarSet.singleton (toVar v), t)
  freeA _ t@CoaLit{} = (mempty, t)
  freeA s (CoaLam t a@(v, _) b) =
    let (fb, b') = freeT s b
     in (toVar v `VarSet.delete` fb, CoaLam t a b')

  freeT :: IsVar a => DeadScope -> CoTerm a -> (VarSet.Set, CoTerm a)
  freeT s (CotAtom a) = CotAtom <$> freeA s a
  freeT s (CotApp f a) = CotApp <$> freeA s f <*> freeA s a
  freeT s (CotTyApp f t) = CotTyApp <$> freeA s f <*> pure t
  freeT s (CotExtend t rs) = CotExtend <$> freeA s t <*> traverse (third3A (freeA s)) rs

  freeT s (CotLet vs b) =
    let s' = extendPureFuns s vs in
    case uncurry (buildLet s' vs) (freeT s' b) of
      -- If we've no bindings, just return the primary expression
      (f, [], b') -> (f, b')
      -- If we're of the form `let x = y in x`, simplify to `y`.
      (f, [(v, _, b')], CotAtom(CoaRef v' _))
        | v == v' -> (f, b')
      -- Otherwise emit as normal
      (f, vs', b') -> (f , CotLet vs' b')

  freeT s (CotMatch t bs) =
    let (ft, t') = freeA s t
        (fbs, bs') = unzip $ map (\(p,t,b) -> (p,t,) <$> freeT s b) bs
        pbs = map (patternVars . fst3) bs

        matchFree = mconcat (zipWith VarSet.difference fbs pbs)
    in case (pbs, fbs,  bs') of
         -- If we've got a single pattern match with nothing captured, then inline
         ([pb], [fb], [(_, _, b)])
           | VarSet.isEmpty (VarSet.intersection pb fb) -> (matchFree, b)
         -- Otherwise assume everything is used
         _ -> (ft <> matchFree, CotMatch t' bs')

  typeArity :: CoType a -> Int
  typeArity (CotyForall _ ty) = 1 + typeArity ty
  typeArity _ = 0

  -- Compute the number of arguments which can be passed to a function before
  -- it becomes "impure".
  atomArity s (CoaRef r _)
    | TgInternal n <- toVar r
    = fromMaybe 0 (Map.lookup n opArity)
    | otherwise
    = fromMaybe 0 (VarMap.lookup (toVar r) (pureArity s))
  atomArity s (CoaLam _ _ (CotAtom a)) = 1 + atomArity s a
  atomArity _ _ = 0

  isPure _ CotAtom{}   = True
  isPure _ CotExtend{} = True
  isPure _ CotTyApp{}  = True
  isPure s (CotLet vs e) = isPure s e && all (isPure s . thd3) vs
  isPure s (CotMatch _ bs) = all (isPure s . thd3) bs
  isPure s (CotApp f _) = atomArity s f > 0

  extendPureFuns :: IsVar a => DeadScope -> [(a, CoType a, CoTerm a)] -> DeadScope
  extendPureFuns s vs = s
    { pureArity = foldr (\(v, _, e) p ->
                           case e of
                             CotAtom  a   -> maybeInsert v (atomArity s a) p
                             CotTyApp a _ -> maybeInsert v (atomArity s a - 1) p
                             CotApp   a _ -> maybeInsert v (atomArity s a - 1) p
                             _ -> p) (pureArity s) vs
    }

  maybeInsert v a m
    | a > 0     = VarMap.insert (toVar v) a m
    | otherwise = m

  buildFrees :: IsVar a => DeadScope -> VarSet.Set -> [(VarSet.Set, (a, CoType a, CoTerm a))]
            -> Bool -> [(VarSet.Set, (a, CoType a, CoTerm a))] -> VarSet.Set
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

-- Dead code is always free
opArity :: Map.Map Text Int
opArity = Map.fromList
    [ ( "+",  2 )
    , ( "-",  2 )
    , ( "*",  2 )
    , ( "/",  2 )
    , ( "^",  2 )
    , ( "<",  2 )
    , ( ">",  2 )
    , ( ">=", 2 )
    , ( "<=", 2 )
    , ( "==", 3 )
    , ( "<>", 3 )
    , ( "||", 2 )
    , ( "&&", 2 )
    ]
