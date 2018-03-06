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

deadCodePass :: IsVar a => [Stmt a] -> [Stmt a]
deadCodePass = snd . freeS (DeadScope mempty) Nothing where
  freeS :: IsVar a => DeadScope -> Maybe a -> [Stmt a] -> (VarSet.Set, [Stmt a])
  freeS _ (Just m) [] = (VarSet.singleton (toVar m), mempty)
  freeS _ Nothing  [] = (mempty, mempty)

  freeS s m (x@(Foreign v _ _):xs) =
    let (fxs, xs') = freeS s m xs
     in if toVar v `VarSet.member` fxs
           then (toVar v `VarSet.delete` fxs, x:xs')
           else (fxs, xs')
  freeS s m (StmtLet vs:xs) =
    let m' = find (\x -> case toVar x of
                           TgName "main" _ -> True
                           _ -> False) (map fst3 vs)
        s' = extendPureFuns s vs
    in case uncurry (buildLet s' vs) (freeS s' (m <|> m') xs) of
         -- If we've no bindings, just return the primary expression
         (f, [], xs') -> (f, xs')
         -- Otherwise emit as normal
         (f, vs', xs') -> (f, StmtLet vs':xs')

  freeS s m (x@(Type _ cases):xs) =
    let s' = s { pureArity = foldr (\(v, ty) p -> VarMap.insert (toVar v) (1 + typeArity ty) p)
                             (pureArity s) cases }
    in (x:) <$> freeS s' m xs

  freeA :: IsVar a => DeadScope -> Atom a -> (VarSet.Set, Atom a)
  freeA _ t@(Ref v _)= (VarSet.singleton (toVar v), t)
  freeA _ t@Lit{} = (mempty, t)
  freeA s (Lam t a@(v, _) b) =
    let (fb, b') = freeT s b
     in (toVar v `VarSet.delete` fb, Lam t a b')

  freeT :: IsVar a => DeadScope -> Term a -> (VarSet.Set, Term a)
  freeT s (Atom a) = Atom <$> freeA s a
  freeT s (App f a) = App <$> freeA s f <*> freeA s a
  freeT s (TyApp f t) = TyApp <$> freeA s f <*> pure t
  freeT s (Cast f t) = Cast <$> freeA s f <*> pure t
  freeT s (Extend t rs) = Extend <$> freeA s t <*> traverse (third3A (freeA s)) rs

  freeT s (Let vs b) =
    let s' = extendPureFuns s vs in
    case uncurry (buildLet s' vs) (freeT s' b) of
      -- If we've no bindings, just return the primary expression
      (f, [], b') -> (f, b')
      -- If we're of the form `let x = y in x`, simplify to `y`.
      (f, [(v, _, b')], Atom(Ref v' _))
        | v == v' -> (f, b')
      -- Otherwise emit as normal
      (f, vs', b') -> (f , Let vs' b')

  freeT s (Match t bs) =
    let (ft, t') = freeA s t
        (fbs, bs') = unzip $ map (\(p,t,b) -> (p,t,) <$> freeT s b) bs
        pbs = map (patternVars . fst3) bs

        matchFree = mconcat (zipWith VarSet.difference fbs pbs)
    in case (pbs, fbs,  bs') of
         -- If we've got a single pattern match with nothing captured, then inline
         ([pb], [fb], [(_, _, b)])
           | VarSet.isEmpty (VarSet.intersection pb fb) -> (matchFree, b)
         -- Otherwise assume everything is used
         _ -> (ft <> matchFree, Match t' bs')

  typeArity :: Type a -> Int
  typeArity (ForallTy _ ty) = 1 + typeArity ty
  typeArity _ = 0

  -- Compute the number of arguments which can be passed to a function before
  -- it becomes "impure".
  atomArity s (Ref r _)
    | TgInternal n <- toVar r
    = fromMaybe 0 (Map.lookup n opArity)
    | otherwise
    = fromMaybe 0 (VarMap.lookup (toVar r) (pureArity s))
  atomArity s (Lam _ _ (Atom a)) = 1 + atomArity s a
  atomArity _ _ = 0

  isPure _ Atom{}   = True
  isPure _ Extend{} = True
  isPure _ TyApp{}  = True
  isPure _ Cast{}  = True
  isPure s (Let vs e) = isPure s e && all (isPure s . thd3) vs
  isPure s (Match _ bs) = all (isPure s . thd3) bs
  isPure s (App f _) = atomArity s f > 0

  extendPureFuns :: IsVar a => DeadScope -> [(a, Type a, Term a)] -> DeadScope
  extendPureFuns s vs = s
    { pureArity = foldr (\(v, _, e) p ->
                           case e of
                             Atom  a   -> maybeInsert v (atomArity s a) p
                             TyApp a _ -> maybeInsert v (atomArity s a - 1) p
                             App   a _ -> maybeInsert v (atomArity s a - 1) p
                             _ -> p) (pureArity s) vs
    }

  maybeInsert v a m
    | a > 0     = VarMap.insert (toVar v) a m
    | otherwise = m

  buildFrees :: IsVar a => DeadScope -> VarSet.Set -> [(VarSet.Set, (a, Type a, Term a))]
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
