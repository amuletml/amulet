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
import qualified Data.Text as T
import Data.Triple
import Data.List

import Core.Optimise.Reduce.Base
import Core.Optimise.Reduce.Fold
import Core.Optimise.Reduce.Pattern
import Core.Optimise
import Core.Builtin
import Core.Types

reducePass :: (IsVar a, MonadNamey m) => [Stmt a] -> m [Stmt a]
reducePass = runReduceN reduceStmts 4

extendVar :: IsVar a => (a, Type a, Term a) -> ReduceScope a -> ReduceScope a
extendVar (v, _, e) = vars %~ VarMap.insert (toVar v) e

extendVars :: IsVar a => [(a, Type a, Term a)] -> ReduceScope a -> ReduceScope a
extendVars vs s = foldr extendVar s vs

reduceStmts :: MonadReduce a m => [Stmt a] -> m [Stmt a]
reduceStmts [] = pure []
reduceStmts (s@Foreign{}:ss) = (s:) <$> reduceStmts ss
reduceStmts (StmtLet (One var):ss) = do
  var' <- third3A reduceTerm var
  ss' <- local (extendVar var') (reduceStmts ss)
  pure $ StmtLet (One var'):ss'
reduceStmts (StmtLet (Many vs):ss) = do
  vs' <- local (extendVars vs) $ traverse (third3A reduceTerm) vs
  ss' <- local (extendVars vs') $ reduceStmts ss
  pure $ StmtLet (Many vs'):ss'
reduceStmts (s@(Type v cases):ss) =
  local ( (types %~ VarMap.insert (toVar v) cases)
        . (ctors %~ VarMap.union (VarMap.fromList (map (first toVar) cases))) ) $
    (s:) <$> reduceStmts ss

-- | Simplify an atom within the current context
--
-- This doesn't do anything fancy: we just inline trivial variables.
reduceAtom :: MonadReduce a m => Atom a -> m (Atom a)
reduceAtom a@(Ref v _) = do
  -- Beta reduction (let case)
  v' <- asks (lookupBaseVar v)
  case v' of
    Just (Atom d) | trivialAtom d -> changed d
    _ -> pure a
reduceAtom a@Lit{} = pure a

-- | Simplify a term within the current context
--
-- This will simplify nested terms/atoms as well.
reduceTerm :: forall a m. MonadReduce a m => Term a -> m (Term a)

reduceTerm (Atom a) = Atom <$> reduceAtom a
reduceTerm (Values vs) = Values <$> traverse reduceAtom vs
reduceTerm (TyApp a ty) = flip TyApp ty <$> reduceAtom a

reduceTerm (Extend e fs) = do
  e' <- reduceAtom e
  fs' <- traverse (third3A reduceAtom) fs
  case fs' of
    -- Eliminate empty extensions

    -- TODO: Could we do an additional filter which removes redundant fields (for
    -- cases where we can see the parent record and it's the same value).
    [] -> changed $ Atom e'
    _ -> pure $ Extend e' fs'

reduceTerm (Lam arg body) = do
  body' <- reduceTerm body
  case (arg, body) of
    -- Eta conversion (function case)
    (TermArgument var _, App r (Ref var' _))
      | var == var' -> changed $ Atom r
    (TypeArgument var _, TyApp r (VarTy var'))
      | var == var' -> changed $ Atom r

    _ -> pure $ Lam arg body'

reduceTerm (Cast a co) = do
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
      , Just (Cast oa oco) <- lookupVar v s
      , Just (l, r) <- relates co'
      , Just (l', r') <- relates oco
      , unifyClosed r l'
      , unifyClosed l r'
      -> changed $ Atom oa

      | otherwise -> pure $ Cast a' co'

reduceTerm (App f x) = do
  f' <- reduceAtom f
  x' <- reduceAtom x

  s <- ask
  if
    | Ref fv _ <- f'
    , Ref xv _ <- x'
    , Just (Values xs) <- lookupVar xv s
    , Just a' <- foldApply (toVar (lookupRawVar fv s)) xs
    -> changed a'
    | otherwise -> pure (App f' x')

reduceTerm t@Match{} = reduceTermK t pure
reduceTerm t@Let{}   = reduceTermK t pure

-- | Reduce the provided term, running the continuation when reaching the
-- "leaf" node.
--
-- This allows us to implement commuting conversion for lets and matches
-- in a more intuitive manner.
reduceTermK :: forall a m. MonadReduce a m
            => Term a
            -> (Term a -> m (Term a))
            -> m (Term a)

reduceTermK (Let (One (v, ty, e)) rest) cont = reduceTermK e $ \e' -> do
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
        , Just [_] <- VarMap.lookup (toVar tyName) (s ^. types)
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

reduceTermK (Let (Many vs) rest) cont = do
  -- We really can't do a lot with recursive lets sadly, so let's just
  -- leave them for now.
  vs' <- traverse (third3A reduceTerm) vs
  rest' <- reduceTermK rest cont
  pure (Let (Many vs') rest')

reduceTermK (Match test arms) cont = do
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
  case foldCases s test' arms of
    Left (arm, subst) -> changing $ view armBody <$> reduceArm cont arm subst
    Right arms' -> case filterDeadArms (_armPtrn . fst) s arms' of
      [(arm, subst)] -> Match test' . pure <$> reduceArm cont arm subst
      arms' -> do
        arms'' <- traverse (uncurry (reduceArm pure)) arms'
        cont $ Match test' arms''
  where
    foldCases _ _ [] = Right []
    foldCases s t (a@Arm { _armPtrn = p }:as) =
      case reducePattern s t p of
        PatternFail -> foldCases s t as
        PatternUnknown subst -> Right ((a, subst):either pure id (foldCases s t as))
        PatternPartial subst -> Right [(a, subst)]
        PatternComplete subst -> Left ((a, subst))

    -- | Visit an arm with the provided continuation and substitution,
    -- applying them as needed.
    reduceArm :: (Term a -> m (Term a)) -> Arm a -> Subst a -> m (Arm a)
    reduceArm cont a@Arm { _armTyvars = [], _armBody = body } subst = do
      -- In the trivial case we can do a plain old substitution
      body' <- local (vars %~ VarMap.union (VarMap.map Atom subst)) (reduceTermK body cont)
      pure $ a & armBody .~ body'
    reduceArm cont a@Arm{  _armVars = vs, _armBody = body } subst = do
      -- Otherwise we look up types and attempt to unify them.
      let Just tySubst = VarMap.foldrWithKey (foldVar vs . fromVar) (Just mempty) subst
      body' <- local (vars %~ VarMap.union (VarMap.map Atom subst)) (reduceTermK body cont)

      pure $ a
        & (armVars %~ map (second (substituteInType tySubst)))
        -- Substitute tyvars and remove those which have been remapped
        . (armTyvars %~ map (second (substituteInType tySubst)) . filter (not . flip VarMap.member tySubst . toVar . fst))
        . (armBody .~ substituteInTys tySubst body')

    foldVar :: [(a, Type a)] -> a -> Atom a -> Maybe (VarMap.Map (Type a)) -> Maybe (VarMap.Map (Type a))
    foldVar _ _ _ Nothing = Nothing
    foldVar vs v a (Just sol) = do
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
