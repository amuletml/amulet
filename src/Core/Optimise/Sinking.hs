{-# LANGUAGE TupleSections #-}

{- | The allocation sinking pass (also referred to as float-in or lambda
   dropping) moves pure expressions closer to where they are used. If an
   expression is only used within a single branch of a match case, then
   it will be moved into that case.

   This hopefully reduces the times when allocation is needed, as only
   code paths which require the value actually execute it.

   This operates very much as you'd expect: keep track of which variables
   we're trying to sink, determine if they can be sunk to the next level
   and, if not, emit their corresponding bindings here. The main
   exception to this rule is lambdas, as one should not shift values
   /inside/ lambdas: that could lead to duplicating work instead.

   The current purity tracking is rather naive: atoms, constructors and
   records are the only "pure" expressions. It may be a good idea to
   extend this in the future to include partially applied functions (like
   the DCE pass does).
-}
module Core.Optimise.Sinking (sinkingPass) where

import Control.Lens

import qualified Data.VarSet as VarSet
import Data.Triple

import Core.Optimise
import qualified Core.Arity as A

data Sinkable a = Sinkable { sBind  :: (a, Type, Term a)
                           , sFree  :: VarSet.Set
                           , sBound :: VarSet.Set }
  deriving (Show)

data SinkState a = SinkState { sinkable :: [Sinkable a]
                             , arity :: A.ArityScope }
  deriving (Show)

-- | Run the sinking pass on a series of statements with free variable
-- information.
--
-- This is generally called after applying 'tagFreeSet' to the program.
sinkingPass :: IsVar a => [AnnStmt VarSet.Set a] -> [Stmt a]
sinkingPass = sinkStmts (SinkState [] A.emptyScope)

sinkStmts :: IsVar a => SinkState a -> [AnnStmt VarSet.Set a] -> [Stmt a]
sinkStmts _ [] = []
sinkStmts s (Foreign v ty bod:xs) =
  let s' = s { arity = A.extendForeign (arity s) (v, ty) }
   in Foreign v ty bod:sinkStmts s' xs
sinkStmts s (StmtLet (One v):xs) =
  let v' = third3 (sinkTerm s) v
      s' = s { arity = A.extendPureLets (arity s) [v] }
  in StmtLet (One v'):sinkStmts s' xs
sinkStmts s (StmtLet (Many vs):xs) =
  let s' = s { arity = A.extendPureLets (arity s) vs }
      vs' = map (third3 (sinkTerm s')) vs
  in StmtLet (Many vs'):sinkStmts s' xs
sinkStmts s (Type v cases:xs) =
  let s' = s { arity = A.extendPureCtors (arity s) cases }
  in Type v cases:sinkStmts s' xs

sinkTerm :: IsVar a => SinkState a -> AnnTerm VarSet.Set a -> Term a
sinkTerm s (AnnAtom _ a) = flushBinds (sinkable s) (Atom a)
sinkTerm s (AnnApp _ f x) = flushBinds (sinkable s) (App f x)
sinkTerm s (AnnLam _ var term) = Lam var (sinkTerm s term)

sinkTerm s (AnnLet _ (One b@(v, ty, e)) r)
  -- If we're pure, add it to the sink set
  | A.isPure (arity s) e
  = let e' = sinkTerm (nullBinds s) e
        s' = s { sinkable = Sinkable { sBind = (v, ty, e')
                                     , sBound = VarSet.singleton (toVar v)
                                     , sFree = extractAnn e } : sinkable s
               , arity = A.extendPureLets (arity s) [b] }
    in sinkTerm s' r

  -- Otherwise, partition into sinkable/nonsinkable
  | otherwise
  = let (fs, [rs, es]) = partitionBinds (sinkable s) [extractAnn r, extractAnn e]
        a' = A.extendPureLets (arity s) [b]
        e' = sinkTerm (s { sinkable = es, arity = a' }) e
        r' = sinkTerm (s { sinkable = rs, arity = a' }) r
    in flushBinds fs (Let (One (v, ty, e')) r')

sinkTerm s (AnnLet _ (Many vs) r) =
  let (fs, rs:vss) = partitionBinds (sinkable s) (extractAnn r : map (extractAnn . thd3) vs)
      a' = A.extendPureLets (arity s) vs
      vs' = zipWith (\fv -> third3 (sinkTerm (s { sinkable = fv, arity = a' }))) vss vs
      r'  = sinkTerm (s { sinkable = rs, arity = a' }) r
  in flushBinds fs (Let (Many vs') r')

sinkTerm s (AnnMatch _ t bs) =
  let (fs, ts:bss) = partitionBinds (sinkable s) (freeInAtom t : map (extractAnn . view armBody) bs)
      bs' = zipWith (\fv -> armBody %~ sinkTerm (s { sinkable = fv })) bss bs
  in flushBinds fs $ flushBinds ts $ Match t bs'

sinkTerm s (AnnTyApp _ f ty) = flushBinds (sinkable s) (TyApp f ty)
sinkTerm s (AnnExtend _ f fs) = flushBinds (sinkable s) (Extend f fs)
sinkTerm s (AnnValues _ xs) = flushBinds (sinkable s) (Values xs)
sinkTerm s (AnnCast _ f ty co) = flushBinds (sinkable s) (Cast f ty co)

flushBinds :: [Sinkable a] -> Term a -> Term a
flushBinds [] t = t
flushBinds (si@Sinkable{}:xs) t = flushBinds xs (Let (One (sBind si)) t)

nullBinds :: SinkState a -> SinkState a
nullBinds s = s { sinkable = [] }

partitionBinds
  :: [Sinkable a] -- Terms which may be sunk
  -> [VarSet.Set] -- Free variables of the places which these terms may be sunk into
  -> ([Sinkable a], [[Sinkable a]]) -- A tuple of non-sunk terms and a list with sunken terms, with
                                    -- each entry corresponding to an entry in the places list.
partitionBinds sink free = go sink (mempty, []) (map (,[]) free) where
  go [] (_, here) binds = (reverse here, map (reverse . snd) binds)
  go (si:sis) here binds =
    if occursIn si here
    then
      -- If any term dependent on this occurs here, we should emit this binding here
      go sis (insertSink si here) binds
    else
      -- Find which branches depend on one of these variables
      let usedIn = map (occursIn si) binds
      in case length (filter id usedIn) of
           1 -> go sis here (zipWith (insertMaybe si) binds usedIn)
           _ -> go sis (insertSink si here) binds

  insertSink si@Sinkable{} (free, sis) = (sFree si `VarSet.union` free, si:sis)
  insertMaybe _  bind False = bind
  insertMaybe si bind True = insertSink si bind

  occursIn si = not . VarSet.isEmpty . VarSet.intersection (sBound si) . fst
