{-# LANGUAGE TupleSections #-}

{- | The dead code pass performs dead code elimination on the
   input core. This pass assumes all pure expressions are dead,
   and will only preserve them if it can find some expression
   which consumes it.

   We consider an expression as pure by using the methods
   provided by "Core.Arity". Thus we must maintain an
   'ArityScope' while traversing the expression tree.

   Each function within the pass returns the optimised
   expression and a set of free variables consumed. If a
   variable is not used, we will remove it (unless the body is
   impure).

   While it may be possible to eliminate unused arguments from
   within this pass, it would probably be more elegant to
   perform this within a lambda lifting pass.
-}
module Core.Optimise.DeadCode ( deadCodePass ) where

import qualified Data.VarSet as VarSet
import Data.Triple

import Control.Lens

import Core.Optimise
import Core.Arity

-- | Perform a dead-code elimination pass
deadCodePass :: IsVar a => [Stmt a] -> [Stmt a]
deadCodePass = snd . freeS emptyScope where
  freeS :: IsVar a => ArityScope -> [Stmt a] -> (VarSet.Set, [Stmt a])
  freeS _ [] = (mempty, mempty)

  freeS s (x@(Foreign v ty _):xs) =
    let s' = extendForeign s (v, ty)
        (fxs, xs') = freeS s' xs
     in if toVar v `VarSet.member` fxs
           then (toVar v `VarSet.delete` fxs, x:xs')
           else (fxs, xs')
  freeS s (StmtLet (One vs@(v, ty, e)):xs) =
    let s' = extendPureLets s [vs]
        (fe, e') = freeT s e
        (fb, xs') = freeS s xs
    in if isPure s' e' && toVar v `VarSet.notMember` fb
       then (fb, xs')
       else (fe <> fb, StmtLet (One (v, ty, e')):xs')
  freeS s (StmtLet (Many vs):xs) =
    let s' = extendPureLets s vs
    in case uncurry (buildLet s' vs) (freeS s' xs) of
         -- If we've no bindings, just return the primary expression
         (f, [], xs') -> (f, xs')
         -- Otherwise emit as normal
         (f, vs', xs') -> (f, StmtLet (Many vs'):xs')
  freeS s (x@(Type _ cases):xs) =
    let s' = extendPureCtors s cases
    in (x:) <$> freeS s' xs

  freeA :: IsVar a => ArityScope -> Atom a -> (VarSet.Set, Atom a)
  freeA _ t@(Ref v _)= (VarSet.singleton (toVar v), t)
  freeA _ t@Lit{} = (mempty, t)

  freeT :: IsVar a => ArityScope -> Term a -> (VarSet.Set, Term a)
  freeT s (Atom a) = Atom <$> freeA s a
  freeT s (App f a) = App <$> freeA s f <*> freeA s a
  freeT s (TyApp f t) = TyApp <$> freeA s f <*> pure t
  freeT s (Cast f t co) = Cast <$> freeA s f <*> pure t <*> pure co
  freeT s (Extend t rs) = Extend <$> freeA s t <*> traverse (third3A (freeA s)) rs
  freeT s (Values xs) = Values <$> traverse (freeA s) xs
  freeT s (Lam a b) =
    let (fb, b') = freeT s b
     in (argVar a `VarSet.delete` fb, Lam a b')
  freeT s (Let (One vs@(v, ty, e)) b) =
    let s' = extendPureLets s [vs]
        (fe, e') = freeT s e
        (fb, b') = freeT s' b
    in if isPure s' e' && toVar v `VarSet.notMember` fb
       then (fb, b')
       else (fe <> fb, Let (One (v, ty, e')) b')

  freeT s (Let (Many vs) b) =
    let s' = extendPureLets s vs in
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

  -- | A worker function for 'buildLet'. This finds all variables within
  -- a binding group which are live, adding their dependencies to the
  -- live variable set.
  --
  -- The complexity mostly stems from the fact that we perform multiple
  -- passes over the binding group, as we loop until a steady state is
  -- found.
  buildFrees :: IsVar a
             => ArityScope
             -> VarSet.Set -- ^ The free variables within the body of this binding group
             -> [(VarSet.Set, (a, Type a, Term a))] -- ^ Each binding group and free variables within the value
             -> Bool -- ^ Whether we need to perform another round
             -> [(VarSet.Set, (a, Type a, Term a))] -- ^ Bindings which we consider dead
             -> VarSet.Set -- ^ The set of free variables including the bindings and their variables
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
