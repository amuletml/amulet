{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
module Core.Optimise.Reduce.Inline
  ( UsedAs(..)
  , loopBreakers
  , isTrivialAtom
  , isTrivialTerm
  , sizeAtom
  , sizeTerm

  , InlineVars
  , InlineSubst
  , gatherInlining
  , inlineSubst
  , shouldInline
  , buildUnknownInline
  , buildKnownInline
  ) where

import Control.Monad.RWS
import Control.Arrow (first, (***))
import Control.Lens

import qualified Data.VarMap as VarMap
import qualified Data.VarSet as VarSet
import qualified Data.Text as T
import Data.Triple
import Data.Graph
import Data.List
import Data.Ord

import Core.Optimise.Reduce.Base
import Core.Optimise
import Core.Types

data UsedAs
  = UsedOther
  | UsedApply
  | UsedMatch
  deriving (Show)

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
             -> [Binder (OccursVar a)] -- ^ The binders we're interested in
             -> VarSet.Set -- ^ The set of loop breaker variables
loopBreakers s = go . map (\b -> (b, extractAnn (thd3 b)))
  where
    buildNode :: VarSet.Set -> (Binder (OccursVar a), VarSet.Set)
              -> ((Binder (OccursVar a), VarSet.Set), CoVar, [CoVar])
    buildNode vs (b, fv) =
      let fv' = VarSet.intersection fv vs
      in ((b, fv'), toVar (fst3 b), VarSet.toList fv')

    go :: [(Binder (OccursVar a), VarSet.Set)] -> VarSet.Set
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
          -> Binder (OccursVar a) -> VarSet.Set
          -> LoopScore
loopScore s (v, _, t) fv
  -- Self recursive definitions are wonderful. We'll never inline them
  -- after all.
  | toVar v `VarSet.member` fv = LS 0 0

  -- Trivial terms will be inlined so they're a terrible choice
  | isTrivialTerm t = scored 4
  -- Similarly, variables used once will /probably/ be inlined, so let's
  -- avoid them.
  | usedWhen v == Once = scored 3
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
--
-- Make sure you update the values in 'canInline' too when changing
-- these.
sizeAtom :: IsVar v => ReduceScope a -> Atom v -> Int
sizeAtom s (Ref v _) = if isCtor v s then 1 else 2
sizeAtom _ (Lit _) = 1

-- | Determine the "size" of an term, using a somewhat arbitrary set of values.
--
-- This is used in order to score terms for inlining and loop breaking.
--
-- Make sure you update the values in 'canInline' too when changing
-- these.
sizeTerm :: IsVar v => ReduceScope a -> AnnTerm b v -> Int

-- Effectively free
sizeTerm s (AnnAtom _ a) = sizeAtom s a
sizeTerm s (AnnTyApp _ t _) = sizeAtom s t
sizeTerm s (AnnCast _ t _ _) = sizeAtom s t

sizeTerm s (AnnApp _ f x) = 2 + sizeAtom s f + sizeAtom s x

sizeTerm s (AnnLam _ TypeArgument{} b) = sizeTerm s b
sizeTerm s (AnnLam _ TermArgument{} b) = 10 + sizeTerm s b

sizeTerm s (AnnLet _ (One v) e) = sizeTerm s (thd3 v) + sizeTerm s e
sizeTerm s (AnnLet _ (Many vs) e) = sum (map (sizeTerm s . thd3) vs) + sizeTerm s e
sizeTerm s (AnnMatch _ e bs) = sizeAtom s e + sum (map (sizeTerm s . view armBody) bs)

sizeTerm s (AnnExtend _ e rs) = 10 + sizeAtom s e + sum (map (sizeAtom s . thd3) rs)
sizeTerm s (AnnValues _ xs) = sum (map (sizeAtom s) xs)

type InlineVars a v b = ([(a, Atom v)], [(a, Type v)], AnnTerm b a)

type InlineSubst a v b = (VarMap.Map (Atom v), VarMap.Map (Type v), AnnTerm b a)

inlineSubst :: IsVar a => InlineVars a v b -> InlineSubst a v b
inlineSubst (vs, ts, term) =
  ( VarMap.fromList (map (first toVar) vs)
  , VarMap.fromList (map (first toVar) ts)
  , term )

shouldInline :: (IsVar v, IsVar u)
             => ReduceScope a
             -> ReduceState a
             -> UsedAs
             -> InlineSubst v u b
             -> Bool
shouldInline s st usage (args, tyargs, rhs)
  | rhsSize <= argSize + 10 = True
  | not usefulUsed && VarSet.null usefulArgs = False
  | otherwise = smallEnough

  where
    rhsSize = sizeTerm s rhs

    -- | The size of this inlined function is less than what is required
    -- to have it unapplied.
    argSize
      -- Include size of LHS and application
      = foldr ((+) . (+4) . sizeAtom s) 0 args
      -- Include size of LHS
      + 2 * length tyargs

    -- | Whether the usage of the inlining's result can be considered
    -- "useful".
    usefulUsed =
      case usage of
        UsedOther -> False
        UsedMatch -> True
        UsedApply -> True

    -- | If this atom has the potential to be useful
    usefulAtom (Ref v _)
      | Nothing <- VarMap.lookup (toVar v) (s ^. varScope)
      , Nothing <- VarMap.lookup (toVar v) (st ^. varSubst)
      , not (isCtor v s)
      = False
    usefulAtom _ = True

    -- | The set of "useful" arguments
    usefulArgs = VarMap.foldrWithKey (\v a c -> if usefulAtom a then VarSet.insert v c else c) mempty args
    usefulArgCount = VarSet.size usefulArgs

    threshold = 100 :: Int

    -- | Determine if this term is small enough to be inlined
    smallEnough =
      let -- The set of interesting variables is the union of interesting args and constructors
          usefulVars = VarMap.foldrWithKey (\v _ -> VarSet.insert (toVar v)) usefulArgs (s ^. ctorScope)
          -- Discount functions whose result is considered "interesting".
          resDis = if usefulUsed && usefulResult usefulVars rhs
                   then 40
                   else 0 :: Int

          argDis = VarSet.size (usefulUsage rhs) * 20
      in rhsSize <= threshold
      || rhsSize - resDis - argDis <= threshold

    -- | Determine if this result looks "interesting"
    --
    -- Namely if we return an "inspectable" value (record, tuple, constructor,
    -- lambda, non-boring argument).

    -- These are guaranteed to be consumed (due to 'interestingUsage')
    usefulResult _ AnnLam{} = True
    usefulResult _ AnnExtend{} = True
    usefulResult _ AnnValues{} = True
    -- Basic usages of interesting variables are interesting
    usefulResult c (AnnAtom _ a) = usefulResultA c a
    usefulResult c (AnnApp _ f _) = usefulResultA c f
    usefulResult c (AnnTyApp _ f _) = usefulResultA c f
    usefulResult c (AnnCast _ f _ _) = usefulResultA c f
    -- Ensure at least one arm is interesting
    usefulResult c (AnnMatch _ _ arms)
      = any (usefulResult c . view armBody) arms
    -- Propagate interesting things onwards
    usefulResult c (AnnLet _ (One (v, _, e)) rest) =
      let c' = if usefulResult c e
               then VarSet.insert (toVar v) c
               else c
      in usefulResult c' rest
    usefulResult c (AnnLet _ Many{} rest) = usefulResult c rest

    usefulResultA c (Ref v _) = VarSet.member (toVar v) c
    usefulResultA _ Lit{} = True

    -- | Find useful arguments which are used in an useful context
    usefulUsage AnnExtend{} = mempty
    usefulUsage AnnValues{} = mempty
    usefulUsage AnnAtom{} = mempty
    usefulUsage AnnCast{} = mempty
    usefulUsage (AnnLam _ _ b) = usefulUsage b
    usefulUsage (AnnLet _ (One (_, _, e)) rest) = usefulUsage e <!> usefulUsage rest
    usefulUsage (AnnLet _ (Many vs) rest) = foldr ((<!>) . usefulUsage . thd3) (usefulUsage rest) vs
    -- Applications or matches are considered interesting
    usefulUsage (AnnMatch _ test arms) = foldr ((<!>) . usefulUsage . view armBody) (usefulUsageA test) arms
    usefulUsage (AnnApp _ f _) = usefulUsageA f
    usefulUsage (AnnTyApp _ f _) = usefulUsageA f

    usefulUsageA (Ref v _)
      | VarSet.member (toVar v) usefulArgs
      = VarSet.singleton (toVar v)
      | otherwise = mempty
    usefulUsageA Lit{} = mempty

    -- Short circuiting version of <>, which will ensure we don't inspect everything
    -- if we've seen all variables
    l <!> r = if VarSet.size l >= usefulArgCount then l else l <> r


-- | Gather information about a potential inlining candidate at this site
gatherInlining :: MonadReduce a m
               => AnnTerm VarSet.Set (OccursVar a)
               -> m (Maybe ( Either (InlineVars (OccursVar a) (OccursVar a) VarSet.Set)
                                    (InlineVars a (OccursVar a) ())
                           , VarSet.Set ))
gatherInlining (AnnApp _ (Ref f _) x) = do
  s <- gets (VarMap.lookup (toVar f) . view varSubst)
  case s of
    Just (SubTodo t) -> do
      t' <- gatherInlining t
      pure $ case t' of
        Just (Left (vs, ts, AnnLam _ (TermArgument v _) bod), rs) ->
          Just ( Left ((v, x):vs, ts, bod)
               , VarSet.insert (toVar f) rs )
        Just (Right (vs, ts, Lam (TermArgument v _) bod), rs) ->
          Just (  Right ((v, x):vs, ts, bod)
               , VarSet.insert (toVar f) rs )
        _ -> Nothing
    _ -> do
      s <- asks (lookupVar (toVar f))
      pure $ case s of
        VarDef { varDef = Just DefInfo { defTerm = Lam (TermArgument v _) bod }
               , varLoopBreak = False }
          -> Just ( Right ([(v, x)], mempty, bod)
                  , mempty )
        _ -> Nothing
gatherInlining (AnnTyApp _ (Ref f _) x) = do
  s <- gets (VarMap.lookup (toVar f) . view varSubst)
  case s of
    Just (SubTodo t) -> do
      t' <- gatherInlining t
      pure $ case t' of
        Just (Left (vs, ts, AnnLam _ (TypeArgument v _) bod), rs) ->
          Just ( Left (vs, (v, x):ts, bod)
               , VarSet.insert (toVar f) rs )
        Just (Right (vs, ts, Lam (TypeArgument v _) bod), rs) ->
          Just ( Right (vs, (v, x):ts, bod)
               , VarSet.insert (toVar f) rs )
        _ -> Nothing
    _ -> do
      s <- asks (lookupVar (toVar f))
      pure $ case s of
        VarDef { varDef = Just DefInfo { defTerm = Lam (TypeArgument v _) bod }
               , varLoopBreak = False }
          -> Just (Right (mempty, [(v, x)], bod), mempty)
        _ -> Nothing
gatherInlining t = pure . Just $ (Left (mempty, mempty, t), mempty)

-- | Build up a set of terms for inlining when the terms have been
-- annotated.
buildKnownInline :: IsVar a
              => InlineVars (OccursVar a) (OccursVar a) VarSet.Set
              -> AnnTerm VarSet.Set (OccursVar a)
buildKnownInline (vs, ts, rhs)
  = substituteInTys (VarMap.fromList . map (first toVar) $ ts)
  . foldr buildLet rhs
  $ vs
  where
    buildLet (v, a) rest =
      let ann = freeInAtom a
      in AnnLet (ann <> VarSet.delete (toVar v) (extractAnn rest))
                (One (v, approximateAtomType a, AnnAtom ann a))
                rest

-- | Build up a set of terms for inlining when the terms have not been
-- annotated.
buildUnknownInline :: IsVar a
                   => InlineVars a (OccursVar a) ()
                   -> Term a
buildUnknownInline (vs, ts, rhs)
  = substituteInTys (VarMap.fromList . map (toVar *** fmap underlying) $ ts)
  . foldr buildLet rhs
  $ vs
  where
    buildLet (v, a) rest =
      let a' = underlying <$> a
      in Let (One (v, approximateAtomType a', Atom a')) rest
