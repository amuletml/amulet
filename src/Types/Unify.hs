{-# LANGUAGE MultiWayIf, GADTs, FlexibleContexts, ScopedTypeVariables, TemplateHaskell #-}
{-# LANGUAGE TupleSections, OverloadedStrings, ViewPatterns, LambdaCase #-}

-- | This module implements the logic responsible for solving the
-- sequence of @Constraint@s the type-checker generates for a particular
-- binding groups.
module Types.Unify (solve, skolemise, freshSkol) where

import Control.Monad.Except
import Control.Monad.State
import Control.Applicative
import Control.Monad.Infer
import Control.Lens hiding (Empty)

import Types.Infer.Builtin hiding (subsumes, unify)
import Types.Infer.Errors
import Types.Wellformed

import qualified Syntax.Implicits as Imp
import Syntax.Implicits (Obligation(..), Implicit(..), overlap)
import Syntax.Pretty
import Syntax.Subst
import Syntax

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Traversable
import Data.Sequence (Seq ((:<|), Empty))
import Data.Foldable
import Data.Function
import Data.Spanned
import Data.List
import Data.Text (Text)

data SolveScope
  = SolveScope { _bindSkol :: Bool
               , _don'tTouch :: Set.Set (Var Typed)
               }
  deriving (Eq, Show, Ord)

data SolveState
  = SolveState { _solveTySubst :: Subst Typed
               , _solveAssumptions :: Subst Typed
               , _solveCoSubst :: Map.Map (Var Typed) (Wrapper Typed)
               , _solveImplBail :: Set.Set (Var Typed)
               }
  deriving (Eq, Show, Ord)

makeLenses ''SolveState
makeLenses ''SolveScope

type SolveM = NameyT (WriterT [TypeError] (StateT SolveState (ReaderT SolveScope (Except TypeError))))

bind :: Var Typed -> Type Typed -> SolveM (Coercion Typed)
bind var ty
  | occurs var ty = throwError (Occurs var ty)
  | TyVar var == ty = pure (ReflCo ty)
  | TyForall{} <- ty = throwError (Impredicative var ty)
  | TyWithConstraints cs ty <- ty = do
    traverse_ (uncurry unify) cs
    bind var ty
  | otherwise = do
      env <- use solveTySubst
      assum <- use solveAssumptions
      noTouch <- view don'tTouch
      ty <- pure (apply env ty) -- shadowing
      when (occurs var ty) (throwError (Occurs var ty))
      if var `Set.member` noTouch && var `Map.member` assum
         then unify ty (apply env (assum Map.! var))
         else -- Attempt to extend the environment, otherwise unify with existing type
            case Map.lookup var env of
              Nothing -> do
                if | var `Set.notMember` noTouch -> solveTySubst .= env `compose` Map.singleton var ty
                   | var `Set.member` noTouch, TyVar v <- ty, v `Set.notMember` noTouch ->
                     solveTySubst .= (env `compose` Map.singleton v (TyVar var))
                   | otherwise -> throwError (NotEqual (TyVar var) ty)
                pure (ReflCo ty)
              Just ty'
                | ty' == ty -> pure (ReflCo (apply env ty'))
                | otherwise -> unify ty (apply env ty')

-- FOR BOTH UNIFY AND SUBSUME:
--  unify have want
--  subsume k have want
-- i.e. The first argument is the type *we have*.
unify :: Type Typed -> Type Typed -> SolveM (Coercion Typed)
unify (TySkol x) (TySkol y)
  | x == y = pure (ReflCo (TySkol y))
  | otherwise = do
    sub <- use solveAssumptions
    let assumption = sub ^. at (x ^. skolIdent) <|> sub ^. at (y ^. skolIdent)
    case assumption of
      Just{} -> pure (AssumedCo (TySkol x) (TySkol y))
      Nothing -> do
        canWe <- view bindSkol
        if canWe
           then do
             solveAssumptions . at (x ^. skolIdent) ?= TySkol y
             pure (AssumedCo (TySkol x) (TySkol y))
           else throwError $ SkolBinding x (TySkol y)

unify ta@(TyPromotedCon a) tb@(TyPromotedCon b)
  | a == b = pure (ReflCo tb)
  | otherwise = throwError (NotEqual ta tb)

unify (TySkol t@(Skolem sv _ _ _)) b = do
  sub <- use (solveAssumptions . at sv)
  case sub of
    Just ty -> do
      _ <- unify b ty
      pure (AssumedCo (TySkol t) ty)
    Nothing -> case b of
      TyVar v -> bind v (TySkol t)
      _ -> do
        canWe <- view bindSkol
        if canWe
           then if t `Set.notMember` skols b
           then do
             solveAssumptions . at sv ?= b
             pure (AssumedCo (TySkol t) b)
           else throwError (Occurs sv b)
           else throwError $ SkolBinding t b
unify b (TySkol t) = SymCo <$> unify (TySkol t) b

unify (TyVar a) b = bind a b
unify a (TyVar b) = SymCo <$> bind b a

unify (TyArr a b) (TyArr a' b') = ArrCo <$> unify a a' <*> unify b b'

unify (TyPi (Implicit a) b) (TyPi (Implicit a') b') =
  ArrCo <$> unify a a' <*> unify b b' -- Technically cheating but yay desugaring

unify l@(TyApp a b) r@(TyApp a' b') =
  (AppCo <$> unify a a' <*> unify b b')
    `catchError` \case
      NotEqual _ _ -> throwError (NotEqual l r)
      x -> throwError x

unify ta@(TyCon a) tb@(TyCon b)
  | a == b = pure (ReflCo tb)
  | otherwise = throwError (NotEqual ta tb)

unify (TyForall v Nothing ty) (TyForall v' Nothing ty') = do
  fresh <- freshTV
  let (TyVar tv) = fresh

  ForallCo tv (ReflCo TyType) <$>
    unify (apply (Map.singleton v fresh) ty) (apply (Map.singleton v' fresh) ty')

unify (TyForall v (Just k) ty) (TyForall v' (Just k') ty') = do
  c <- unify k k'
  fresh <- freshTV
  let (TyVar tv) = fresh

  ForallCo tv c <$>
    unify (apply (Map.singleton v fresh) ty) (apply (Map.singleton v' fresh) ty')

unify (TyRows rho arow) (TyRows sigma brow)
  | overlaps <- overlap arow brow
  , rhoNew <- deleteFirstsBy ((==) `on` fst) (sortOn fst arow) (sortOn fst brow)
  , sigmaNew <- deleteFirstsBy ((==) `on` fst) (sortOn fst brow) (sortOn fst arow) =
    do
      let mk t rs = if rs /= [] then TyRows t rs else t
      tau <- freshTV
      cs <- traverse unifRow overlaps
      if null sigmaNew && null rhoNew
         then RowsCo <$> unify rho sigma <*> pure cs
         else do
           co <- unify rho (mk tau sigmaNew) -- yes
           _ <- unify sigma (mk tau rhoNew) -- it's backwards
           pure (RowsCo co cs)

unify ta@TyExactRows{} tb@TyRows{} = SymCo <$> unify tb ta

unify tb@(TyRows rho brow) ta@(TyExactRows arow)
  | overlaps <- overlap arow brow
  , rhoNew <- deleteFirstsBy ((==) `on` fst) (sortOn fst arow) (sortOn fst brow)
  = if | length overlaps < length brow -> throwError (NoOverlap tb ta)
       | otherwise -> do
          cs <- traverse unifRow overlaps
          _ <- unify rho (TyExactRows rhoNew)
          pure (SymCo (ProjCo rhoNew cs))

unify ta@(TyExactRows arow) tb@(TyExactRows brow)
  | overlaps <- overlap arow brow
  = do when (length overlaps /= length arow || length overlaps /= length brow)
         $ throwError (NoOverlap ta tb)
       cs <- traverse unifRow overlaps
       pure (ExactRowsCo cs)

unify x tp@TyRows{} = throwError (Note (CanNotInstance tp x) isRec)
unify tp@TyRows{} x = throwError (Note (CanNotInstance tp x) isRec)
unify (TyTuple a b) (TyTuple a' b') =
  ProdCo <$> unify a a' <*> unify b b'

unify TyType TyType = pure (ReflCo TyType)
unify a b = throwError (NotEqual a b)

isRec :: String
isRec = "A record type's hole can only be instanced to another record"


unifRow :: (Text, Type Typed, Type Typed) -> SolveM (Text, Coercion Typed)
unifRow (t, a, b) = do
  co <- unify a b
  pure (t, co)

runSolve :: Var Resolved -> Subst Typed -> SolveM b -> Either TypeError (Var Resolved, (Subst Typed, Map.Map (Var Typed) (Wrapper Typed)))
runSolve i s x = runExcept (fix (runReaderT (runStateT (runWriterT (runNameyT act i)) (SolveState s mempty mempty mempty)) emptyScope)) where
  act = (,) <$> genName <*> x
  fix act = do
    (((_, x), w), s) <- act
    case w of
      [] -> let ss = s ^. solveTySubst
             in pure (x, (fmap (apply ss) ss, s ^. solveCoSubst))
      xs -> throwError (ManyErrors xs)
  emptyScope = SolveScope False mempty

-- | Solve a sequence of constraints, returning either a substitution
-- for both type variables (a 'Subst' 'Typed') and for 'Wrapper'
-- variables.
--
-- The 'Var' 'Resolved' parameter is the first fresh name this
-- particular instance of the solver is allowed to generate from.
solve :: Var Resolved -> Seq.Seq (Constraint Typed) -> Either TypeError (Subst Typed, Map.Map (Var Typed) (Wrapper Typed))
solve i cs = snd <$> runSolve i mempty (doSolve cs)

doSolve :: Seq.Seq (Constraint Typed) -> SolveM ()
doSolve Empty = pure ()
doSolve (ConUnify because v a b :<| xs) = do
  sub <- use solveTySubst

  co <- catchy $ unify (apply sub a) (apply sub b)
  case co of
    Left e -> tell [propagateBlame because e]
    Right co -> solveCoSubst . at v .= Just (Cast co)

  doSolve xs
doSolve (ConSubsume because v scope a b :<| xs) = do
  sub <- use solveTySubst

  let a' = apply sub a
      cont = do
        sub <- use solveTySubst
        co <- catchy $ subsumes scope (apply sub a) (apply sub b)
        case co of
          Left e -> do
            solveImplBail %= Set.union (ftv a' <> ftv (apply sub b))
            tell [propagateBlame because e]
          Right co -> solveCoSubst . at v .= Just co

  case a' of
    TyPi (Implicit _) _ -> do
      doSolve xs
      cont
    _ -> do
      cont
      doSolve xs

doSolve (ConImplies because not cs ts :<| xs) = do
  before <- use solveTySubst
  assump <- use solveAssumptions
  let not' = ftv (apply before not) <> ftv not
      cs' = apply before cs
      ts' = apply before ts
  do
    let go = local (bindSkol .~ True) . local (don'tTouch .~ mempty) $ doSolve cs'
    ((), sub) <- capture go

    solveAssumptions .= (sub ^. solveAssumptions <> sub ^. solveTySubst)

    local (don'tTouch %~ Set.union not') $
      doSolve (fmap (apply (sub ^. solveTySubst)) ts')
        `catchError` \e -> tell [ArisingFrom e because]

    let leaky = Map.filterWithKey (\k _ -> k `Set.notMember` assumptionBound) (sub ^. solveTySubst)
        assumptionBound = not'

    solveTySubst %= Map.union leaky
    solveAssumptions .= assump

    doSolve xs

doSolve (ConImplicit because var scope t inner :<| xs) = do
  doSolve xs
  sub <- use solveTySubst
  abort <- use solveImplBail
  let scope' = Imp.mapTypes (apply sub) scope
      t' = apply sub t
  when (Set.disjoint (ftv t') abort) $ do
    w <- catchy $ solveImplicitConstraint 0 inner scope' t'
    case w of
      Left e -> tell [propagateBlame because e]
      Right w -> solveCoSubst . at var ?= w

doSolve (ConFail a v t :<| cs) = do
  doSolve cs
  sub <- use solveTySubst
  let ex = Hole (unTvName v) (fst a)
  tell [propagateBlame (BecauseOf ex) $ foundHole v (apply sub t) sub]

solveImplicitConstraint :: Int -> Type Typed -> Imp.ImplicitScope Typed -> Type Typed -> SolveM (Wrapper Typed)
solveImplicitConstraint x _ _ tau | x >= 200 = throwError (tooMuchRecursion tau)
solveImplicitConstraint x inner scope t =
  let isUnsolved t = (t ^. Imp.implSolved) == Imp.Unsolved
      freeInHead t = ftv (t ^. Imp.implHead)
      freeInPre t = flip foldMap (t ^. Imp.implPre) $ \case
        Quantifier{} -> Set.empty
        Implication t -> ftv t
      isPolymorphic t = let tv (Quantifier (Invisible v _)) = [v]
                            tv _ = mempty
                            tvs = foldMap tv (t ^. Imp.implPre)
                         in all (`Set.member` freeInHead t) tvs
                         && all (`Set.member` freeInPre t) tvs
                         && not (null tvs)
      pickClosest t xs = groupBy (\a b -> fst a == fst b) (sortOn fst (map (matchDistance t) xs))
      trySolved xs =
        case partition isUnsolved xs of
          ([c], _) -> useImplicit (x + 1) inner scope t c
          _ -> retry xs
      tryPoly xs =
        case partition isPolymorphic xs of
          ([c], _) -> do
            let solvable (Quantifier _) = pure True
                solvable (Implication tau) = do
                  w <- catchy (solveImplicitConstraint (x + 1) inner scope tau)
                  case w of
                    Left _ -> pure False
                    Right _ -> pure True
            weCan <- allM solvable (c ^. Imp.implPre)
            unless weCan (retry xs)
            useImplicit (x + 1) inner scope t c
          _ -> retry xs
      tryMoreSpecific xs =
        case pickClosest t xs of
          ([(_, c)]:_) -> useImplicit (x + 1) inner scope t c
          _ -> throwError (ambiguousImplicits xs t)
      retry :: [Implicit Typed] -> SolveM a
      retry xs = case xs of
        [] -> throwError (noImplicitFound scope t)
        _ -> throwError (ambiguousImplicits xs t)
  in case Imp.lookup t scope of
       [c] -> useImplicit x inner scope t c
       -- [] -> throwError (noImplicitFound scope t)
       xs -> (trySolved xs `orElse` tryMoreSpecific xs) `orElse` tryPoly xs

useImplicit :: Int -> Type Typed -> Imp.ImplicitScope Typed -> Type Typed -> Implicit Typed -> SolveM (Wrapper Typed)
useImplicit x inner scope' ty (ImplChoice hdt oty os s imp) = go where
  go :: SolveM (Wrapper Typed)
  go = do
    (hdt, refresh) <- if s == Imp.Solved then refreshTy hdt else pure (hdt, mempty)
    (cast, view solveTySubst -> sub) <- capture $ unify hdt ty

    let start e = VarRef imp (annotation e, oty)
        mk _ [] = pure (\e -> ExprWrapper (probablyCast cast) e (annotation e, ty))
        mk (TyPi (Invisible v _) t) (Quantifier (Invisible _ _):xs) = do
          let tau = apply sub . apply refresh . TyVar $ v
          let sub' = Map.singleton v tau

          flip (.) (\ex -> ExprWrapper (TypeApp tau) ex (annotation ex, apply sub' t)) <$> mk (apply sub' t) xs
        mk (TyPi (Implicit tau) t) (Implication _:os) = do
          w <- solveImplicitConstraint (x + 1) t scope' tau
          k' <- mk t os
          pure (k' . (\ex -> ExprWrapper w ex (annotation ex, t)))
        mk x (o:_) = error (show x ++ " missing " ++ show o)

    wrap <- mk oty os
    solveTySubst %= compose sub
    let cont ex = ExprWrapper (ExprApp (wrap (start ex))) ex (annotation ex, inner)
    pure (WrapFn (MkWrapCont cont ("implicit value for " ++ show ty)))

matchDistance :: Type Typed -> Implicit Typed -> (Int, Implicit Typed)
matchDistance t im =
  let occm = tyVarOcc (im ^. Imp.implHead) <> foldMap (\case Quantifier{} -> mempty; Implication t -> tyVarOcc t) (im ^. Imp.implPre)
      distance =
        case solve firstName (Seq.singleton (ConUnify (BecauseOf (undefined :: Expr Typed)) (TvName firstName) t (im ^. Imp.implHead))) of
          Left _ -> maxBound
          Right (s, _) ->
            let weight v =
                  case Map.lookup v s of
                    Just _ -> 1
                    Nothing -> 0
             in foldOccMap (\v o x -> x + o * weight v) 0 occm
   in (distance, im)

subsumes', subsumes :: Imp.ImplicitScope Typed -> Type Typed -> Type Typed -> SolveM (Wrapper Typed)
subsumes s a b = do
  x <- use solveTySubst
  subsumes' s (apply x a) (apply x b)

subsumes' s t1 t2@TyPi{} | isSkolemisable t2 = do
  sub <- use solveTySubst
  (c, t2') <- skolemise (BySubsumption (apply sub t1) (apply sub t2)) t2
  (Syntax.:>) c <$> subsumes s t1 t2'

subsumes' s t1@TyPi{} t2 | isSkolemisable t1 = do
  (cont, _, t1') <- instantiate Subsumption t1
  let wrap = maybe IdWrap (WrapFn . flip MkWrapCont "forall <= sigma; instantiation") cont

  flip (Syntax.:>) wrap <$> subsumes s t1' t2

subsumes' s (TyPi (Implicit b) c) (TyPi (Implicit a) d) = do
  wc <- subsumes s c d
  wa <- subsumes s a b
  arg <- TvName <$> genName

  let wrap ex | an <- annotation ex
        = Fun (ImplParam (Capture arg (an, a)))
              (ExprWrapper wc
                (App ex
                     (ExprWrapper wa
                        (VarRef arg (an, a))
                        (an, b))
                     (an, c))
                (an, d))
              (an, TyPi (Implicit a) d)

  pure (WrapFn (MkWrapCont wrap "co/contra subsumption for implicit functions"))

subsumes' s wt@(TyPi (Implicit t) t1) t2 | _TyVar `isn't` t2 = do
  omega <- subsumes s t1 t2

  sub <- use solveTySubst
  w <- solveImplicitConstraint 0 wt s (apply sub t)
  let wrap ex | an <- annotation ex
        = ExprWrapper omega
            (ExprWrapper (TypeAsc t1)
              (ExprWrapper w ex (an, t1)) (an, t1)) (an, t2)
   in pure (WrapFn (MkWrapCont wrap "implicit instantation"))

subsumes' s ot@(TyTuple a b) nt@(TyTuple a' b') = do
  wb <- subsumes s b b'
  wa <- subsumes s a a'
  [elem, elem'] <- fmap TvName <$> replicateM 2 genName
  let cont (Tuple (e:es) (an, _)) =
        Tuple [ ExprWrapper wa e (an, a')
              , case es of
                 [e] -> ExprWrapper wb e (an, b')
                 xs -> ExprWrapper wb (Tuple xs (an, b)) (an, b')
              ]
              (an, nt)
      cont ex
        | an <- annotation ex =
          Match ex [ ( PTuple [ Capture elem (an, a), Capture elem' (an, b) ] (an, ot)
                     , Tuple [ ExprWrapper wa (VarRef elem (an, a)) (an, a')
                             , ExprWrapper wb (VarRef elem' (an, b)) (an, b') ]
                             (an, nt)) ]
                   (an, nt)
  pure (WrapFn (MkWrapCont cont "tuple re-packing"))

subsumes' _ a@(TyApp lazy _) b@(TyApp lazy' _)
  | lazy == lazy', lazy' == tyLazy = probablyCast <$> unify a b

subsumes' _ (TyApp lazy ty') ty | lazy == tyLazy, concretish ty = do
  co <- unify ty' ty
  let wrap ex
        | an <- annotation ex =
          App (ExprWrapper (TypeApp ty) (VarRef forceName (an, forceTy))
                (an, forceTy' ty))
              (ExprWrapper (probablyCast (AppCo (ReflCo tyLazy) co)) ex (an, TyApp lazy ty))
              (an, ty)
  pure (WrapFn (MkWrapCont wrap "automatic forcing"))

subsumes' _ ty' (TyApp lazy ty) | lazy == tyLazy, concretish ty' = do
  co <- unify ty ty'
  let wrap ex
        | an <- annotation ex =
          App (ExprWrapper (TypeApp ty) (VarRef lAZYName (an, lAZYTy))
                (an, lAZYTy' ty))
              (Fun (PatParam (PLiteral LiUnit (an, tyUnit)))
                (ExprWrapper (probablyCast co) ex (an, ty'))
                (an, TyArr tyUnit ty))
              (an, TyApp lazy ty)
  pure (WrapFn (MkWrapCont wrap "automatic thunking"))

subsumes' _ a b = probablyCast <$> unify a b


-- | Shallowly skolemise a type, replacing any @forall@-bound 'TyVar's
-- with fresh 'Skolem' constants.
skolemise :: MonadNamey m => SkolemMotive Typed -> Type Typed -> m (Wrapper Typed, Type Typed)
skolemise motive ty@TyPi{} | Just (tv, k, t) <- isSkolemisableTyBinder ty = do
  sk <- freshSkol motive ty tv
  (wrap, ty) <- skolemise motive (apply (Map.singleton tv sk) t)
  kind <- case k of
    Nothing -> freshTV
    Just x -> pure x
  let getSkol (TySkol s) = s
      getSkol _ = error "not a skolem from freshSkol"
  pure (TypeLam (getSkol sk) kind Syntax.:> wrap, ty)
skolemise _ ty = pure (IdWrap, ty)

isSkolemisableTyBinder :: Type Typed -> Maybe (Var Typed, Maybe (Type Typed), Type Typed)
isSkolemisableTyBinder (TyPi (Invisible v k) c) = Just (v, k, c)
isSkolemisableTyBinder _ = Nothing

-- Which coercions are safe to remove *here*?
isReflexiveCo :: Coercion Typed -> Bool
isReflexiveCo ReflCo{} = True
isReflexiveCo (SymCo c) = isReflexiveCo c
isReflexiveCo (AppCo a b) = isReflexiveCo a && isReflexiveCo b
isReflexiveCo (ArrCo a b) = isReflexiveCo a && isReflexiveCo b
isReflexiveCo (ProdCo a b) = isReflexiveCo a && isReflexiveCo b
isReflexiveCo (ExactRowsCo rs) = all (isReflexiveCo . snd) rs
isReflexiveCo (RowsCo c rs) = isReflexiveCo c && all (isReflexiveCo . snd) rs
isReflexiveCo (ForallCo _ d c) = isReflexiveCo d && isReflexiveCo c
isReflexiveCo AssumedCo{} = False
isReflexiveCo ProjCo{} = False
isReflexiveCo VarCo{} = False

probablyCast :: Coercion Typed -> Wrapper Typed
probablyCast x
  | isReflexiveCo x = IdWrap
  | otherwise = Cast x

-- | Make a fresh 'Skolem' constant, with the given 'SkolemMotive', for
-- a 'Var' in the 'Type'.
--
-- @
--  freshSkol motive scope var
-- @
freshSkol :: MonadNamey m => SkolemMotive Typed -> Type Typed -> Var Typed -> m (Type Typed)
freshSkol m ty u = do
  var <- TvName <$> genName
  pure (TySkol (Skolem var u ty m))

occurs :: Var Typed -> Type Typed -> Bool
occurs _ (TyVar _) = False
occurs x e = x `Set.member` ftv e

capture :: MonadState b m => m a -> m (a, b)
capture m = do
  x <- get
  r <- m
  st <- get
  put x
  pure (r, st)

concretish :: Type Typed -> Bool
concretish TyVar{} = False
concretish (TyApp f x) = concretish f && concretish x
concretish _ = True

catchy :: MonadError e m => m a -> m (Either e a)
catchy x = (Right <$> x) `catchError` (pure . Left)

orElse :: MonadError e m => m a -> m a -> m a
orElse x y = x `catchError` const y

allM :: (Monad m, Foldable t) => (a -> m Bool) -> t a -> m Bool
allM p = foldM (\x b -> (&&) x <$> p b) True

refreshTy :: (Substitutable Typed a, MonadNamey m) => a -> m (a, Subst Typed)
refreshTy ty = do
  vs <- for (Set.toList (ftv ty)) $ \v ->
    (v,) <$> refreshTV v
  pure (apply (Map.fromList vs) ty, Map.fromList vs)
