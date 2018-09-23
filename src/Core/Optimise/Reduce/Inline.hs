{-# LANGUAGE ScopedTypeVariables #-}
module Core.Optimise.Reduce.Inline
  ( loopBreakers
  , sizeAtom
  , sizeTerm
  ) where

import Control.Lens

import qualified Data.VarMap as VarMap
import qualified Data.VarSet as VarSet
import qualified Data.Text as T
import Data.Triple
import Data.Graph
import Data.List
import Data.Ord

import Core.Optimise.Reduce.Base

-- | The score of a variable used for determining loops
--
-- The first element determines the "score" of the node. Nodes with lower
-- scores variables are better breakers.
--
-- The second is the inverse size of the node. Larger nodes are less
-- likely to be inlined, and so are better breakers.
data LoopScore = LS !Int Int
  deriving (Show, Eq, Ord)

type Binder a = (a, Type a, AnnTerm VarSet.Set a)

-- | Return a set of all loop breakers within this binding group
loopBreakers :: forall a. IsVar a
             => ReduceScope a -- ^ The scope to resolve in
             -> [Binder a] -- ^ The binders we're interested in
             -> VarSet.Set -- ^ The set of loop breaker variables
loopBreakers s = go . map (\b -> (b, extractAnn (thd3 b)))
  where
    buildNode :: VarSet.Set -> (Binder a, VarSet.Set)
              -> ((Binder a, VarSet.Set), CoVar, [CoVar])
    buildNode vs (b, fv) =
      let fv' = VarSet.intersection fv vs
      in ((b, fv'), toVar (fst3 b), VarSet.toList fv')

    go :: [(Binder a, VarSet.Set)] -> VarSet.Set
    go nodes =
      let vs = VarSet.fromList (map (toVar . fst3 . fst) nodes)
          nodes' = map (buildNode vs) nodes
      in foldMap unwrapNode (stronglyConnComp nodes')

    unwrapNode (AcyclicSCC _) = mempty
    unwrapNode (CyclicSCC bs) =
      -- Find a loop breaker, remove it from the graph and continue as before
      let ((breaker, _, _), _) = minimumBy (comparing (uncurry (loopScore s))) bs
      in VarSet.singleton (toVar breaker) <> go (filter ((/=breaker) . fst3 . fst) bs)

-- | Determine the score for this loop breaker
loopScore :: IsVar a
          => ReduceScope a
          -> Binder a -> VarSet.Set
          -> LoopScore
loopScore s (v, _, t) fv
  -- Self recursive definitions are wonderful. We'll never inline them
  -- after all.
  | toVar v `VarSet.member` fv = LS 0 0

  -- Trivial terms will be inlined so they're a terrible choice
  | isTrivialTerm t = scored 4
  -- Similarly, variables used once will /probably/ be inlined, so let's
  -- avoid them.
  -- TODO: | usedWhen v == Once = scored 3
  -- If we're some form of constructor then we /may/ inline it due to
  -- pattern matches.
  | ctor ctorAtom t = scored 2
  -- Well, this looks hopeful!
  | otherwise = scored 1

  where
    scored x = LS x (-sizeTerm s t)

    -- Records and tuples are constructors
    ctor _ AnnValues{} = True
    ctor _ AnnExtend{} = True
    -- Constructors are obviously constructors
    ctor f (AnnAtom _ r)    = f r
    -- Constructors with arguments are obviously constructors
    ctor f (AnnApp _ r _)   = f r
    ctor f (AnnTyApp _ r _) = f r
    -- Walk down the tree. Yay, ANF!
    ctor f (AnnLet _ (One (v, _, e)) r) = ctor f r || (ctor f e && ctor (refEq v) r)
    ctor _ _ = False

    ctorAtom (Ref r _) = VarMap.member (toVar r) (s ^. ctorScope)
    ctorAtom _ = False

    refEq r (Ref r' _) = r == r'
    refEq _ _ = False

-- | Determine if this is a trivially inlinable term.
isTrivialTerm :: AnnTerm b a -> Bool
isTrivialTerm (AnnAtom _ a) = isTrivialAtom a
isTrivialTerm _ = False

-- | Determine if this is a trivially inlinable atom
isTrivialAtom :: Atom a -> Bool
isTrivialAtom Ref{} = True
isTrivialAtom (Lit (Str t)) = T.length t <= 8
isTrivialAtom (Lit _) = True

-- | Determine the "size" of an atom, using a somewhat arbitrary set of values.
sizeAtom :: IsVar a => ReduceScope a -> Atom a -> Int
sizeAtom s (Ref v _) = if toVar v `VarMap.member` (s ^. ctorScope) then 0 else 5
sizeAtom _ (Lit _) = 1

-- | Determine the "size" of an term, using a somewhat arbitrary set of values.
--
-- This is used in order to score terms for inlining and loop breaking.
sizeTerm :: IsVar a => ReduceScope a -> AnnTerm b a  -> Int
sizeTerm s (AnnAtom _ a) = sizeAtom s a
sizeTerm s (AnnApp _ f x) = sizeAtom s f + sizeAtom s x + 2
sizeTerm s (AnnLam _ TypeArgument{} b) = sizeTerm s b
sizeTerm s (AnnLam _ TermArgument{} b) = 1 + sizeTerm s b
sizeTerm s (AnnLet _ (One v) e) = sizeTerm s (thd3 v) + sizeTerm s e
sizeTerm s (AnnLet _ (Many vs) e) = sum (map (sizeTerm s . thd3) vs) + sizeTerm s e
sizeTerm s (AnnMatch _ e bs) = sizeAtom s e + sum (map (sizeTerm s . view armBody) bs)
sizeTerm s (AnnExtend _ e rs) = sizeAtom s e + sum (map (sizeAtom s . thd3) rs)
sizeTerm s (AnnValues _ xs) = sum (map (sizeAtom s) xs)
sizeTerm s (AnnTyApp _ t _) = sizeAtom s t
sizeTerm s (AnnCast _ t _) = sizeAtom s t
