{-# LANGUAGE
  ConstraintKinds
, FlexibleContexts
, MultiWayIf
, OverloadedStrings
, ScopedTypeVariables
, TemplateHaskell
, ViewPatterns #-}
module Core.Optimise.Reduce (reducePass) where

import Control.Monad.Reader
import Control.Monad.Namey
import Control.Lens
import Control.Arrow hiding ((<+>))

import qualified Data.VarMap as VarMap
import qualified Data.VarSet as VarSet
import qualified Data.Text as T
import Data.Triple
import Data.Graph
import Data.Maybe
import Data.List

import Core.Optimise.Reduce.Pattern
import Core.Optimise.Reduce.Inline
import Core.Optimise.Reduce.Base
import Core.Optimise.Reduce.Fold
import Core.Optimise
import Core.Builtin
import Core.Types
import Core.Free

import Core.Optimise.DeadCode

reducePass :: (IsVar a, MonadNamey m) => [Stmt a] -> m [Stmt a]
reducePass = runReduceN (fmap deadCodePass . reduceStmts . tagFreeSet) 4

extendVar :: IsVar a => (a, Type a, Term a) -> ReduceScope a -> ReduceScope a
extendVar (v, _, e) = varScope %~ VarMap.insert (toVar v) (basicDef v e)

extendVars :: IsVar a => [(a, Type a, Term a)] -> ReduceScope a -> ReduceScope a
extendVars vs s = foldr extendVar s vs where

extendVarsRec :: IsVar a => [(a, Type a, Term a)] -> ReduceScope a -> ReduceScope a
extendVarsRec vs s = foldr extend s vs where
  extend (v, _, e) = varScope %~ VarMap.insert (toVar v) (basicRecDef v e)

reduceStmts :: MonadReduce a m => [AnnStmt VarSet.Set a] -> m [Stmt a]
reduceStmts [] = pure []
reduceStmts (Foreign v ty def:ss) = (Foreign v ty def:) <$> reduceStmts ss
reduceStmts (StmtLet (One var):ss) = do
  var' <- third3A reduceTerm var
  ss' <- local (extendVar var') (reduceStmts ss)
  pure $ StmtLet (One var'):ss'
reduceStmts (StmtLet (Many vs):ss) = do
  breakers <- asks (flip loopBreakers vs)

  vsn   <- traverse (third3A reduceTerm) . filter (flip VarSet.notMember breakers . toVar . fst3) $ vs
  local (extendVars vsn) $ do
    vse <- traverse (third3A reduceTerm) . filter (flip VarSet.member breakers . toVar . fst3) $ vs
    ss' <- local (extendVarsRec vse) $ reduceStmts ss
    pure (StmtLet (Many (vse ++ vsn)):ss')
reduceStmts (Type v cases:ss) =
  local ( (typeScope %~ VarMap.insert (toVar v) cases)
        . (ctorScope %~ VarMap.union (VarMap.fromList (map buildCtor cases))) ) $
    (Type v cases:) <$> reduceStmts ss
  where
    buildCtor (def, sig) = (toVar def, (v, sig))

-- | Simplify an atom within the current context
--
-- This doesn't do anything fancy: we just inline trivial variables.
reduceAtom :: MonadReduce a m => Atom a -> m (Atom a)
reduceAtom a@(Ref v _) = do
  -- Beta reduction (let case)
  v' <- asks (lookupTerm v)
  case v' of
    Just (Atom d) | trivialAtom d -> changed d
    _ -> pure a
reduceAtom a@Lit{} = pure a

-- | Simplify a term within the current context
--
-- This will simplify nested terms/atoms as well.
reduceTerm :: forall a m. MonadReduce a m
           => AnnTerm VarSet.Set a
           -> m (Term a)

reduceTerm (AnnAtom _ a) = Atom <$> reduceAtom a
reduceTerm (AnnValues _ vs) = Values <$> traverse reduceAtom vs

reduceTerm (AnnExtend _ e fs) = do
  e' <- reduceAtom e
  fs' <- traverse (third3A reduceAtom) fs
  case fs' of
    -- Eliminate empty extensions

    -- TODO: Could we do an additional filter which removes redundant fields (for
    -- cases where we can see the parent record and it's the same value).
    [] -> changed $ Atom e'
    _ -> pure $ Extend e' fs'

reduceTerm (AnnLam _ arg body) = do
  body' <- reduceTerm body
  case (arg, body') of
    -- Eta conversion (function case)
    (TermArgument var _, App r (Ref var' _))
      | var == var' -> changed $ Atom r
    (TypeArgument var _, TyApp r (VarTy var'))
      | var == var' -> changed $ Atom r

    _ -> pure $ Lam arg body'

reduceTerm (AnnCast _ a co) = do
  a' <- reduceAtom a
  if redundantCo co
  then changed $ Atom a'
  else do
    let co' = squishCoercion co
    s <- ask
    if
      -- If we point to another cast, and our one reverses theirs then
      -- eliminate it.

      -- TODO: Could we replace this so we chain the coercion, then
      -- detect if it's redundant? This way we can remove /any/ casts
      -- of casts.
      | Ref v _ <- a'
      , Just (Cast oa oco) <- lookupTerm v s
      , Just (l, r) <- relates co'
      , Just (l', r') <- relates oco
      , unifyClosed r l'
      , unifyClosed l r'
      -> changed $ Atom oa

      | otherwise -> pure $ Cast a' co'

reduceTerm t@AnnMatch{} = reduceTermK t pure
reduceTerm t@AnnLet{}   = reduceTermK t pure
reduceTerm t@AnnApp{}   = reduceTermK t pure
reduceTerm t@AnnTyApp{} = reduceTermK t pure

-- | Reduce the provided term, running the continuation when reaching the
-- "leaf" node.
--
-- This allows us to implement commuting conversion for lets and matches
-- in a more intuitive manner.
reduceTermK :: forall a m. MonadReduce a m
            => AnnTerm VarSet.Set a
            -> (Term a -> m (Term a))
            -> m (Term a)

reduceTermK (AnnApp _ f x) cont = do
  f' <- reduceAtom f
  x' <- reduceAtom x

  s <- ask
  if
    -- Constant folding
    | Ref fv _ <- f'
    , Ref xv _ <- x'
    , Just (Values xs) <- lookupTerm xv s
    , Just a' <- foldApply (toVar (lookupRawVar fv s)) xs
    -> changing $ cont a'

    -- Inline
    | Ref fv _ <- f'
    , VarDef { varDef = Just DefInfo { defTerm = Lam (TermArgument v t) b
                                     , defLoopBreak = False } } <- lookupVar fv s
    , sizeTerm s b <= 500 -> do
          b' <- refresh $ Let (One (v, t, Atom x')) b
          changing $ reduceTermK (snd . tagFreeTerm (flip const) const $ b') cont

    -- Nothing to do
    | otherwise -> cont (App f' x')

reduceTermK (AnnTyApp _ f t) cont = do
  f' <- reduceAtom f

  s <- ask
  if
    -- Inline
    | Ref fv _ <- f'
    , VarDef { varDef = Just DefInfo { defTerm = Lam (TypeArgument v _) b
                                     , defLoopBreak = False } } <- lookupVar fv s
    , isLambda b
    , sizeTerm s b <= 500 -> do
        b' <- refresh $ substituteInTys (VarMap.singleton (toVar v) t) b
        changing $ reduceTermK (snd . tagFreeTerm (flip const) const $ b') cont

    | otherwise -> cont $ TyApp f' t

  where
    isLambda (Lam TermArgument{} _) = True
    isLambda (Lam TypeArgument{} b) = isLambda b
    isLambda _ = False


reduceTermK (AnnLet _ (One (v, ty, e)) rest) cont = reduceTermK e $ \e' -> do
  s <- ask
  case e' of
    -- Let of bottom conversion: we've errored here, so we can skip any
    -- remaining code.
    App (Ref f _) msg
      | toVar (lookupRawVar f s) == vError
      ->
        let Just ty = approximateType rest -- TODO: Is this valid with our use of cont?
            errTy = ForallTy Irrelevant (tyString :: Type a)
            na = fromVar tyvarA :: a
        in changed $
          Let (One ( v, errTy ty
                   , TyApp (Ref (fromVar vError) (ForallTy (Relevant na) StarTy (errTy (VarTy na)))) ty))
            (App (Ref v (errTy ty)) msg)

    _ -> do
      rest' <- local (extendVar (v, ty, e')) (reduceTermK rest cont)
      s <- ask
      if
        -- If we're binding a trivial atom, then we can strip it - we'll have
        -- inlined it elsewhere and so it's dead.
        | Atom a <- e', trivialAtom a -> changed rest'

        -- Eta conversion for simple lets
        | Atom (Ref v' _) <- rest', v == v' -> changed e'

        -- Eta conversion for single constructor types
        | Atom (Lit Unit) <- rest', ty == tyUnit -> changed e'
        | Atom (Ref _ ty') <- rest'
        , ty `unifyClosed` ty'
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
                  pure (Let (One (var, ty, ex)) (App joinVar (Ref var ty)))

            arms' <- traverse shoveJoinArm arms
            changed $ Let (One (join, joinTy, Lam (TermArgument v ty) rest')) (Match test arms')

        | otherwise -> pure (Let (One (v, ty, e')) rest')

reduceTermK (AnnLet f (Many vs) rest) cont =
  case stronglyConnComp . map buildNode $ vs of
    [] -> reduceTermK rest cont
    [CyclicSCC vs] -> do
      breakers <- asks (flip loopBreakers vs)

      -- We go over the non-loop breakers (which will never be inlined), reduce
      -- them and then visit the loop breakers with these inlinable functions in
      -- scope.
      vsn   <- traverse (third3A reduceTerm) . filter (flip VarSet.notMember breakers . toVar . fst3) $ vs
      local (extendVars vsn) $ do
        vse <- traverse (third3A reduceTerm) . filter (flip VarSet.member breakers . toVar . fst3) $ vs
        rest' <- local (extendVarsRec vse) $ reduceTermK rest cont
        pure (Let (Many (vse ++ vsn)) rest')

    -- If we can split the nodes up into something simpler, do so!
    cs -> do
      cs' <- changed $ foldr unwrapNode rest cs
      reduceTermK cs' cont

  where
    buildNode n@(v, _, e) = (n, toVar v, VarSet.toList (extractAnn e))
    unwrapNode (AcyclicSCC v) = AnnLet f (One v)
    unwrapNode (CyclicSCC vs) = AnnLet f (Many vs)

reduceTermK (AnnMatch _ test arms) cont = do
  test' <- reduceAtom test
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
  case simplifyArms s test' arms of
    Left (arm, subst) -> changing $ view armBody <$> reduceArm cont arm subst
    Right [(arm, subst)] -> Match test' . pure <$> reduceArm cont arm subst
    Right arms' -> do
      arms'' <- reduceArms test' arms' []
      cont $ Match test' arms''
  where
    reduceArms :: Atom a -> [(AnnArm VarSet.Set a, Subst a)] -> [Pattern a] -> m [Arm a]
    reduceArms (Ref v _) ((a@Arm { _armPtrn = PatWildcard },subst):_) ps = do
      a' <- local (varScope . at (toVar v) %~ extendNot ps) $ reduceArm pure a subst
      pure [a']
    reduceArms at ((arm,subst):as) ps = (:) <$> reduceArm pure arm subst <*> reduceArms at as (arm^.armPtrn:ps)
    reduceArms _ [] _ = pure []

    extendNot ps def =
      let def' = fromMaybe unknownDef def
      in Just def' { varNotAmong = ps ++ (varNotAmong def') }

    -- | Visit an arm with the provided continuation and substitution,
    -- applying them as needed.
    reduceArm :: (Term a -> m (Term a)) -- ^ The continuation function
              -> AnnArm VarSet.Set a -> Subst a -> m (Arm a)
    reduceArm cont a@Arm { _armTyvars = [], _armBody = body } subst = do
      -- In the trivial case we can do a plain old substitution
      body' <- reduceBody cont subst body
      pure $ a & armBody .~ body'
    reduceArm cont a@Arm{ _armVars = vs, _armBody = body } subst = do
      -- Otherwise we look up types and attempt to unify them.
      let Just tySubst = foldr (foldVar vs) (Just mempty) subst
      body' <- reduceBody cont subst body

      pure $ a
        & (armVars %~ map (second (substituteInType tySubst)))
        -- Substitute tyvars and remove those which have been remapped
        . (armTyvars %~ map (second (substituteInType tySubst)) . filter (not . flip VarMap.member tySubst . toVar . fst))
        . (armBody .~ substituteInTys tySubst body')

    reduceBody :: (Term a -> m (Term a)) -> Subst a -> AnnTerm VarSet.Set a -> m (Term a)
    reduceBody cont subst body =
      local (varScope %~ VarMap.union (buildSub subst)) (reduceTermK body cont)

    buildSub :: Subst a -> VarMap.Map (VarDef a)
    buildSub = VarMap.fromList . map (\(var, a) -> (toVar var, basicDef var (Atom a)))

    foldVar :: [(a, Type a)] -> (a, Atom a) -> Maybe (VarMap.Map (Type a)) -> Maybe (VarMap.Map (Type a))
    foldVar _ _ Nothing = Nothing
    foldVar vs (v, a) (Just sol) = do
      vty <- snd <$> find ((==v) . fst) vs
      let aty = approximateAtomType a
      unifyWith sol vty aty

reduceTermK t cont = reduceTerm t >>= cont

redundantCo :: IsVar a => Coercion a -> Bool
redundantCo c
  | Just (a, b) <- relates c = a == b
  | otherwise = False

trivialAtom :: Atom a -> Bool
trivialAtom Ref{} = True
trivialAtom (Lit (Str t)) = T.length t <= 8
trivialAtom (Lit _) = True
