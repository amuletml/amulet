{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE
  ConstraintKinds
, FlexibleContexts
, MultiWayIf
, OverloadedStrings
, ScopedTypeVariables
, TemplateHaskell
, TupleSections
, ViewPatterns #-}
module Core.Optimise.Reduce (reducePass) where

import Control.Monad.Namey
import Control.Monad.RWS
import Control.Lens
import Control.Arrow hiding ((<+>))

import qualified Data.Map.Strict as Map
import qualified Data.VarMap as VarMap
import qualified Data.VarSet as VarSet
import Data.Foldable
import Data.Triple
import Data.Graph
import Data.Maybe

import Core.Optimise.Reduce.Pattern
import Core.Optimise.Reduce.Inline
import Core.Optimise.Reduce.Fold
import Core.Optimise.Reduce.Base
import Core.Optimise
import Core.Builtin
import Core.Types

import Core.Optimise.DeadCode

reducePass :: (IsVar a, MonadNamey m) => OptimiseInfo -> [Stmt a] -> m [Stmt a]
reducePass info =
  runReduceN ( fmap (deadCodePass info)
             . reduceStmts
             . snd
             . tagOccurStmt (const occursSet) OccursVar (exportNames info) ) 4

annotate :: IsVar a => Term a -> AnnTerm VarSet.Set (OccursVar a)
annotate = snd . tagOccurTerm (const occursSet) OccursVar

extendVar :: IsVar a => (a, Type, Term a) -> ReduceScope a -> ReduceScope a
extendVar b@(v, _, e) = (varScope %~ VarMap.insert (toVar v) (basicDef v e))
                      . (ariScope %~ flip extendPureLets [b])

extendVars :: IsVar a => [(a, Type, Term a)] -> ReduceScope a -> ReduceScope a
extendVars vs s = foldr extendVar s vs

extendVarsRec :: IsVar a => [(a, Type, Term a)] -> ReduceScope a -> ReduceScope a
extendVarsRec vs s = foldr extend s vs where
  extend b@(v, _, e) = (varScope %~ VarMap.insert (toVar v) (basicRecDef v e))
                     . (ariScope %~ flip extendPureLets [b])

extendBreakers :: VarSet.Set -> ReduceScope a -> ReduceScope a
extendBreakers vs s = VarSet.foldr extend s vs where
  extend v = varScope %~ VarMap.insert v unknownRecDef

mapVar :: Functor f
       => (AnnTerm b (OccursVar a) -> f (Term a))
       -> (OccursVar a, Type, AnnTerm b (OccursVar a))
       -> f (a, Type, Term a)
mapVar f (v, ty, e) = (underlying v, ty, ) <$> f e

reduceStmts :: MonadReduce a m => [AnnStmt VarSet.Set (OccursVar a)] -> m [Stmt a]
reduceStmts [] = pure []
reduceStmts (Foreign v ty def:ss) = do
  ss' <- local ( (ariScope %~ extendForeign (v, ty) def)
               . (varScope %~ VarMap.insert (toVar v) (foreignDef (underlying v) def)) )
           (reduceStmts ss)
  pure (Foreign (underlying v) ty def:ss')
reduceStmts (StmtLet (One var):ss) = do
  var' <- mapVar reduceTerm' var
  ss' <- local (extendVar var') (reduceStmts ss)
  pure $ StmtLet (One var'):ss'
reduceStmts (StmtLet (Many vs):ss) =
  case stronglyConnComp . map buildNode $ vs of
    [] -> reduceStmts ss
    [CyclicSCC vs] -> local (ariScope %~ flip extendPureLets vs) $ do
      breakers <- asks (`loopBreakers` vs)
      -- We go over the non-loop breakers (which will never be inlined), reduce
      -- them and then visit the loop breakers with these inlinable functions in
      -- scope.
      vsn <- traverse (mapVar reduceTerm') . filter (flip VarSet.notMember breakers . toVar . fst3) $ vs
      local (extendVars vsn . extendBreakers breakers) $ do
        vse <- traverse (mapVar reduceTerm') . filter (flip VarSet.member breakers . toVar . fst3) $ vs
        ss' <- local (extendVarsRec vse) $ reduceStmts ss
        pure (StmtLet (Many (vse ++ vsn)):ss')

    -- If we can split the nodes up into something simpler, do so!
    cs -> do
      cs' <- changed $ foldr ((:) . unwrapNode) ss cs
      reduceStmts cs'

  where
    buildNode n@(v, _, e) = (n, toVar v, VarSet.toList (extractAnn e))
    unwrapNode (AcyclicSCC v) = StmtLet (One v)
    unwrapNode (CyclicSCC vs) = StmtLet (Many vs)

reduceStmts (Type v cases:ss) = do
  let cases' = map (first underlying) cases
  local ( (typeScope %~ VarMap.insert (toVar v) cases')
        . (ctorScope %~ VarMap.union (VarMap.fromList (map buildCtor cases')))
        . (ariScope %~ flip extendPureCtors cases) ) $
    (Type (underlying v) cases':) <$> reduceStmts ss
  where
    buildCtor (def, sig) = (toVar def, (underlying v, sig))

-- | Simplify an atom within the current context
--
-- This doesn't do anything fancy: we just inline trivial variables.
reduceAtom :: MonadReduce a m
           => UsedAs -> Atom -> m Atom
reduceAtom u (Ref v ty) = do
  -- Beta reduction (let case)
  v' <- asks (lookupTerm v)
  case v' of
    Just (Atom d) | isTrivialAtom d -> changed d
    Just _ -> pure basic

    Nothing -> do
      -- If we're not in the scope, maybe try the substitution one?
      s <- gets (VarMap.lookup (toVar v) . view varSubst)
      case s of
        Just (SubTodo t) -> do
          t' <- reduceTerm u t
          case t' of
            Atom a -> do
              -- If this is just an atom, remove it from the substitution scope and
              -- do that
              varSubst %= VarMap.delete (toVar v)
              changed a
            _ -> do
              -- Otherwise put the done substitution in and continue
              varSubst %= VarMap.insert (toVar v) (SubDone t')
              pure basic
        _ -> pure basic
  where basic = Ref (toVar v) ty

reduceAtom _ (Lit l) = pure (Lit l)

-- | Reduce an atom with the default context
reduceAtom' :: MonadReduce a m
            => Atom -> m Atom
reduceAtom' = reduceAtom UsedOther

-- | Simplify a term within the current context
--
-- This will simplify nested terms/atoms as well.
reduceTerm :: forall a m. MonadReduce a m
           => UsedAs -> AnnTerm VarSet.Set (OccursVar a)
           -> m (Term a)

reduceTerm u (AnnAtom _ a) = Atom <$> reduceAtom u a
reduceTerm _ (AnnValues _ vs) = Values <$> traverse reduceAtom' vs

reduceTerm _ (AnnExtend _ e fs) = do
  e' <- reduceAtom' e
  fs' <- traverse reduceRow fs
  s <- ask
  case (e', fs') of
    -- Eliminate empty extensions
    (_, []) -> changed $ Atom e'

    -- If we're updating an existing update, then merge the two.
    (Ref v _, ours)
      | Just (Extend e theirs) <- lookupTerm v s
      ->
        let theirKs = mkMap theirs
            ourKs = mkMap ours
        in
        if foldr (\p@(k,_,_) a -> a && Just p == Map.lookup k theirKs) True ours
        then
          -- If all our keys are identical to the previous one, just inline this
          -- binding. This just prevents us creating entirely duplicate objects.
          changed $ Atom e'
        else
          -- Otherwise just merge the two bindings. This may have the
          -- unfortunate side effect of making variables last for longer, but
          -- should be good enough for now.
          changed . Extend e $ foldr (\x s -> if fst3 x `Map.member` ourKs then s else x:s) ours theirs

    (_, _) -> pure $ Extend e' fs'
  where
    reduceRow (t, ty, e) = (t, ty, ) <$> reduceAtom' e
    mkIdx s@(k, _, _) = (k, s)
    mkMap = Map.fromList . map mkIdx

reduceTerm _ (AnnLam _ arg body) = do
  body' <- reduceTerm' body
  s <- ask
  case (underlying <$> arg, body') of
    -- Eta conversion (function case)
    (TermArgument var _, App r (Ref var' _))
      | toVar var == var', nonBreaker r s -> changed $ Atom r
    (TypeArgument var _, TyApp r (VarTy var'))
      | toVar var == var', nonBreaker r s -> changed $ Atom r

    (arg', _) -> pure $ Lam arg' body'

  where
    -- | Check if a definition is not a loop breaker - we don't want to
    -- eta-reduce those.
    nonBreaker (Ref v _) s = not . varLoopBreak . lookupVar v $ s
    nonBreaker Lit{} _ = True

reduceTerm u (AnnCast _ a to co) = do
  a' <- reduceAtom u a
  let from = approximateAtomType a'
  if from `unifyClosed` to
  then changed $ Atom a'
  else do
    s <- ask
    if
      -- If we point to another cast, either try to merge or eliminate
      -- them.
      | Ref v _ <- a'
      , Just (Cast oa _ oco) <- lookupTerm v s
      -> if approximateAtomType oa `unifyClosed` from
         then changed $ Atom oa
         else let co' = squishCoercion (oco `Trans` co)
              in changed $ Cast oa to co'

      | otherwise ->
        let co' = squishCoercion co
        in pure $ Cast a' to co'

reduceTerm u t@AnnMatch{} = reduceTermK u t pure
reduceTerm u t@AnnLet{}   = reduceTermK u t pure
reduceTerm u t@AnnApp{}   = reduceTermK u t pure
reduceTerm u t@AnnTyApp{} = reduceTermK u t pure

-- | Reduce a term with the default context
reduceTerm' :: MonadReduce a m
            => AnnTerm VarSet.Set (OccursVar a)
            -> m (Term a)
reduceTerm' = reduceTerm UsedOther

-- | Reduce the provided term, running the continuation when reaching the
-- "leaf" node.
--
-- This allows us to implement commuting conversion for lets and matches
-- in a more intuitive manner.
reduceTermK :: forall a m. MonadReduce a m
            => UsedAs
            -> AnnTerm VarSet.Set (OccursVar a)
            -> (Term a -> m (Term a))
            -> m (Term a)

reduceTermK u d@(AnnApp _ f x) cont
  = inlineOr d u cont basic
  where
    basic = do
      f' <- reduceAtom UsedApply f

      s <- ask
      st <- get
      if
        -- Attempt to reduce forced lazy values. We only look in the
        -- substitution set, and so can guarantee that they are only used once.
        | Ref fV _ <- f', vForce == toVar (lookupRawVar fV s)
        , Ref xV _ <- x
        , Just (SubTodo (AnnApp _ (Ref lazyV _) (Ref lamV _))) <- VarMap.lookup (toVar xV) (st ^. varSubst)
        -- Find the `lazy {'a}` application. Ideally we could use
        -- 'lookupRawTerm', but this'll probably be in the application set.
        , Just (SubTodo (AnnTyApp _ (Ref lazyV' _) _)) <- VarMap.lookup (toVar lazyV) (st ^. varSubst)
        , toVar lazyV' == vLAZY
        -- Find the deferred lambda
        , Just (SubTodo (AnnLam lf (TermArgument la lat) lbod)) <- VarMap.lookup (toVar lamV) (st ^. varSubst)
        -> do
          varSubst %= VarMap.delete (toVar lamV) . VarMap.delete (toVar lazyV) . VarMap.delete (toVar xV)
          reduceTermK u (AnnLet lf (One (la, lat, AnnAtom mempty (Lit Unit))) lbod) cont

        | otherwise -> do
            x' <- reduceAtom' x
            s <- ask

            if
              | Ref fv _ <- f'
              , Ref xv _ <- x'
              , VarDef { varDef = ForeignInfo { defForeign = Intrinsic i } }
                <- lookupVar fv s
              , Just (Values xs) <- lookupTerm xv s
              , Just a' <- foldApply i xs
              -> changing $ cont a'

              | otherwise -> cont (App f' x')




reduceTermK _ d@(AnnTyApp _ f t) cont
  = inlineOr d UsedOther cont basic
  where
    basic = do
      f' <- reduceAtom UsedApply f
      cont $ TyApp f' t

reduceTermK u (AnnLet fa (One (va, tya, AnnLet fb bb rb)) ra) cont =
  flip (reduceTermK u) cont $ AnnLet fb bb (AnnLet fa (One (va, tya, rb)) ra)

reduceTermK u (AnnLet _ (One b@(v, ty, e)) rest) cont = do
  s <- ask
  st <- get
  let used = usedWhen v
      pures = isPure (s ^. ariScope) e
      inlines = case e of
        -- Applications are fine in the once case (will not duplicate work or code), as long as they
        -- are pure. We also check they are not constructors, as those can never be inlined and the
        -- pattern matcher will not see deferred definitions.
        AnnApp _ (Ref f _) _   -> used == Once && pures && inlineableFn s st f
        AnnTyApp _ (Ref f _) _ -> used == Once && pures && inlineableFn s st f
        -- Lambdas are fine in the once or "once lambda" case as they'll not duplicate code and will
        -- only be inlined if applied (and so not duplicate work).
        AnnLam{} -> used == Once || used == OnceLambda
        _ -> False

  if
    | used == Dead, pures -> reduceTermK u rest cont
    | inlines -> do
      varSubst %= VarMap.insert (toVar v) (SubTodo e)

      rest' <- local (ariScope %~ flip extendPureLets [b])
                 (reduceTermK u rest cont)

      se <- gets (VarMap.lookup (toVar v) . view varSubst)
      varSubst %= VarMap.delete (toVar v)
      case se of
        Just (SubDone e') -> considerE e' (pure rest')
        -- If it's no longer in the set, then it's either been visited or is now
        -- considered dead.
        _ -> changed rest'
    | otherwise -> reduceTermK UsedOther e $ \e' -> do
      considerE e' (local (extendVar (v', ty, e'))
                     (reduceTermK u rest cont))

  where
    v' = underlying v

    inlineableFn :: ReduceScope a -> ReduceState a -> CoVar -> Bool
    inlineableFn s st f =
      let f' = lookupRawVar f s
      in if
      | f' == vLAZY -> True
      | isCtor f' s -> False
      | Nothing <- VarMap.lookup f' (s ^. varScope)
      , Nothing <- VarMap.lookup f' (st ^. varSubst)
      -> False
      | otherwise -> True

    -- | Examine e and determine whether the remaining information needs to be
    -- preserved
    considerE e' rest' = do
      s <- ask
      case e' of
        -- Let of bottom conversion: we've errored here, so we can skip any
        -- remaining code.
        App (Ref f _) msg
          | lookupRawVar f s == vError
          ->
            let Just ty = approximateType rest -- TODO: Is this valid with our use of cont?
                errTy = ForallTy Irrelevant tyString
            in changed $
              Let (One ( v', errTy ty
                       , TyApp (Ref (fromVar vError) (ForallTy (Relevant tyvarA) StarTy (errTy (VarTy tyvarA)))) ty))
                (App (Ref (toVar v') (errTy ty)) msg)

        _ -> rest' >>= finalise e'

    -- | Generate a binding from e' and rest'
    finalise e' rest' = do
      s <- ask
      if
        -- If we're binding a trivial atom, then we can strip it - we'll have
        -- inlined it elsewhere and so it's dead.
        | Atom a <- e', isTrivialAtom a -> changed rest'

        -- Eta conversion for simple lets
        | Atom (Ref ov _) <- rest', ov == toVar v' -> changed e'

        -- Eta conversion for single constructor types
        | Atom (Lit Unit) <- rest', ty == tyUnit -> changed e'
        | Atom (Ref _ oty) <- rest'
        , ty `unifyClosed` oty
        , Just tyName <- unwrapTy ty
        , Just [_] <- VarMap.lookup (toVar tyName) (s ^. typeScope)
        -> changed e'

        -- Match commuting conversion for multiple arms
        | Match test arms <- e'
        , Just restTy <- approximateType rest'
        -> do
            join <- fresh' ValueVar
            let joinTy = ForallTy Irrelevant ty restTy
                joinVar = Ref join joinTy

                shoveJoinArm :: Arm a -> m (Arm a) = armBody %%~ shoveJoin
                shoveJoin (Let bind body) = Let bind <$> shoveJoin body
                shoveJoin (Match t bs) = Match t <$> traverse shoveJoinArm bs
                shoveJoin (Atom a) = pure (App joinVar a)
                shoveJoin ex = do
                  var <- fresh' ValueVar
                  pure (Let (One (fromVar var, ty, ex)) (App joinVar (Ref var ty)))

            arms' <- traverse shoveJoinArm arms
            changed $ Let (One (fromVar join, joinTy, Lam (TermArgument v' ty) rest')) (Match test arms')

        | otherwise -> pure (Let (One (v', ty, e')) rest')

reduceTermK u (AnnLet f (Many vs) rest) cont =
  case stronglyConnComp . map buildNode $ vs of
    [] -> reduceTermK u rest cont
    [CyclicSCC vs] -> local (ariScope %~ flip extendPureLets vs) $ do
      breakers <- asks (`loopBreakers` vs)
      -- We go over the non-loop breakers (which will never be inlined), reduce
      -- them and then visit the loop breakers with these inlinable functions in
      -- scope.
      vsn <- traverse (mapVar reduceTerm') . filter (flip VarSet.notMember breakers . toVar . fst3) $ vs
      local (extendVars vsn . extendBreakers breakers) $ do
        vse <- traverse (mapVar reduceTerm') . filter (flip VarSet.member breakers . toVar . fst3) $ vs
        rest' <- local (extendVarsRec vse) $ reduceTermK u rest cont
        pure (Let (Many (vse ++ vsn)) rest')

    -- If we can split the nodes up into something simpler, do so!
    cs -> do
      cs' <- changed $ foldr unwrapNode rest cs
      reduceTermK u cs' cont

  where
    buildNode n@(v, _, e) = (n, toVar v, VarSet.toList (extractAnn e))
    unwrapNode (AcyclicSCC v) = AnnLet f (One v)
    unwrapNode (CyclicSCC vs) = AnnLet f (Many vs)

reduceTermK _ (AnnMatch _ test arms) cont = do
  test' <- reduceAtom UsedMatch test
  s <- ask

  -- We prune our pattern list, either removing the match if we can
  -- eliminate all variables or replacing our match with the simplified
  -- list.
  --
  -- If we have only one arm, we can pass our continuation in. Otherwise
  -- we call it on the whole expression.
  --
  -- TODO: Work out a better way of handling continuations on multi-match
  -- arms, as our current handling within the let case means we have to
  -- walk down the entire tree.
  case simplifyArms (underlying<$>) s test' arms of
    Left (arm, subst) -> changing $ view armBody <$> reduceArm cont arm subst
    Right [(arm, subst)] -> Match test' . pure <$> reduceArm cont arm subst
    Right arms' -> do
      arms'' <- reduceArms test' arms' []
      cont $ Match test' arms''
  where
    reduceArms :: Atom -> [(AnnArm VarSet.Set (OccursVar a), Subst a)] -> [Pattern a] -> m [Arm a]
    reduceArms (Ref v _) ((a@Arm { _armPtrn = PatWildcard },subst):_) ps = do
      a' <- local (varScope . at (toVar v) %~ extendNot ps) $ reduceArm pure a subst
      pure [a']
    reduceArms at ((arm,subst):as) ps =
      (:) <$> reduceArm pure arm subst <*> reduceArms at as (fmap underlying (arm^.armPtrn):ps)
    reduceArms _ [] _ = pure []

    extendNot ps def =
      let def' = fromMaybe unknownDef def
      in Just def' { varNotAmong = ps ++ varNotAmong def' }

    -- | Visit an arm with the provided continuation and substitution,
    -- applying them as needed.
    reduceArm :: (Term a -> m (Term a)) -- ^ The continuation function
              -> AnnArm VarSet.Set (OccursVar a) -> Subst a -> m (Arm a)
    reduceArm cont a@Arm { _armTyvars = [], _armBody = body } subst = do
      -- In the trivial case we can do a plain old substitution
      body' <- reduceBody cont subst body
      pure $ (underlying <$> a) & armBody .~ body'
    reduceArm cont a@Arm{ _armVars = vs, _armBody = body } subst = do
      -- Otherwise we look up types and attempt to unify them.
      let Just tySubst = foldr (foldVar (map (first underlying) vs)) (Just mempty) subst
      body' <- reduceBody cont subst body

      pure $ (underlying <$> a)
        & (armVars %~ map (second (substituteInType tySubst)))
        -- Substitute tyvars and remove those which have been remapped
        . (armTyvars %~ map (second (substituteInType tySubst))
        . filter (not . flip VarMap.member tySubst . toVar . fst))
        . (armBody .~ substituteInTys tySubst body')

    reduceBody :: (Term a -> m (Term a)) -> Subst a -> AnnTerm VarSet.Set (OccursVar a) -> m (Term a)
    reduceBody cont subst body = do
      (sub, binds) <- foldrM (\(var, a) (sub, binds) ->
        if isTrivialAtom a
        then pure ( VarMap.insert (toVar var) (basicDef var (Atom a)) sub, binds )
        else do
          let ty = approximateAtomType a
          v <- freshFrom' var
          pure ( VarMap.insert (toVar var) (basicDef var (Atom (Ref (toVar v) ty))) sub
               , Let (One (v, ty, Atom a)) . binds ))
        (mempty, id) subst
      binds <$> local (varScope %~ VarMap.union sub) (reduceTermK UsedOther body cont)

    foldVar :: [(a, Type)] -> (a, Atom) -> Maybe (VarMap.Map Type) -> Maybe (VarMap.Map Type)
    foldVar _ _ Nothing = Nothing
    foldVar vs (v, a) (Just sol) = do
      vty <- snd <$> find ((==v) . fst) vs
      let aty = approximateAtomType a
      unifyWith sol vty aty

reduceTermK u t cont = reduceTerm u t >>= cont

inlineOr :: forall a m. MonadReduce a m
         => AnnTerm VarSet.Set (OccursVar a)
         -> UsedAs
         -> (Term a -> m (Term a))
         -> m (Term a)
         -> m (Term a)
inlineOr t usage cont def = do
  inline <- gatherInlining t
  s <- ask
  st <- get

  case inline of
    Just (Left inl, rs) -> do
      VarSet.foldr (\v -> (*>) (varSubst %= VarMap.delete v)) (pure ()) rs
      changing $ reduceTermK usage (buildKnownInline inl) cont
    Just (Right inl, rs)
      | inl' <- inlineMatches s (inlineSubst inl)
      , shouldInline s st usage inl'
      -> do
          VarSet.foldr (\v -> (*>) (varSubst %= VarMap.delete v)) (pure ()) rs
          rhs' <- refresh $ buildUnknownInline inl
          changing $ reduceTermK usage (annotate rhs') cont
    _ -> def

  where
    -- | Reduce matches when they are the first expression in a function before
    -- determining whether something should be inlined.
    --
    -- This doesn't need to be perfect, it's just a small tweak to catch a
    -- couple of common cases (such as matching on unboxed tuples).
    inlineMatches :: ReduceScope a
                  -> InlineSubst a ()
                  -> InlineSubst a ()
    inlineMatches s (vs, ts, Match test@(Ref v _) arms)
      | Just{} <- VarMap.lookup (toVar v) vs
      = case simplifyArms id s test arms of
          Left (arm, subst) -> inlineMatches
            (s & (varScope %~ (`substScope` subst)))
            (substVars vs subst, ts, arm ^. armBody)
          Right arms' -> (vs, ts, Match test (map fst arms'))
    inlineMatches _ x = x

    substVars :: VarMap.Map Atom -> Subst a -> VarMap.Map Atom
    substVars = foldr (\(v, x) -> VarMap.insert (toVar v) x)

    substScope :: VarMap.Map (VarDef a) -> Subst a -> VarMap.Map (VarDef a)
    substScope = foldr (\(v, x) -> VarMap.insert (toVar v) (basicDef v (Atom x)))
