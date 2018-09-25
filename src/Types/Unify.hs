{-# LANGUAGE MultiWayIf, FlexibleContexts, ScopedTypeVariables,
   TemplateHaskell, TupleSections, ViewPatterns,
   LambdaCase, ConstraintKinds #-}

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

import Syntax.Pretty
import Syntax.Subst
import Syntax

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Set as Set

import Data.List.NonEmpty (NonEmpty(..))
import Data.Traversable
import Data.Semigroup
import Data.Sequence (Seq ((:<|), Empty))
import Data.Typeable
import Data.Foldable
import Data.Function
import Data.Spanned
import Data.Reason
import Data.List
import Data.Text (Text)

import Text.Pretty.Semantic

data SolveScope
  = SolveScope { _bindSkol :: Bool
               , _don'tTouch :: Set.Set (Var Typed)
               }
  deriving (Eq, Show, Ord)

data SolveState
  = SolveState { _solveTySubst :: Subst Typed
               , _solveAssumptions :: Subst Typed
               , _solveCoSubst :: Map.Map (Var Typed) (Wrapper Typed)
               }
  deriving (Eq, Show, Ord)

makeLenses ''SolveState
makeLenses ''SolveScope

type MonadSolve m =
  ( MonadNamey m
  , MonadState SolveState m
  , MonadReader SolveScope m
  , MonadChronicles TypeError m
  )

isRec :: String
isRec = "A record type's hole can only be instanced to another record"

unifRow :: MonadSolve m => (Text, Type Typed, Type Typed) -> m (Text, Coercion Typed)
unifRow (t, a, b) = do
  co <- retcons (flip Note (InField t)) (unify a b)
  pure (t, co)

runSolve :: MonadNamey m
         => Subst Typed
         -> StateT SolveState (ReaderT SolveScope m) b
         -> m (Subst Typed, Map.Map (Var Typed) (Wrapper Typed))
runSolve s x = fix (runReaderT (runStateT act (SolveState s mempty mempty)) emptyScope) where
  act = (,) <$> genName <*> x
  fix act = do
    (_, s) <- act
    let ss = s ^. solveTySubst
    pure (fmap (apply ss) ss, s ^. solveCoSubst)
  emptyScope = SolveScope False mempty

-- | Solve a sequence of constraints, returning either a substitution
-- for both type variables (a 'Subst' 'Typed') and for 'Wrapper'
-- variables.
--
-- The 'Var' 'Resolved' parameter is the first fresh name this
-- particular instance of the solver is allowed to generate from.
solve :: (MonadNamey m, MonadChronicles TypeError m)
      => Seq.Seq (Constraint Typed)
      -> m (Subst Typed, Map.Map (Var Typed) (Wrapper Typed))
solve = runSolve mempty . doSolve

doSolve :: forall m. MonadSolve m => Seq.Seq (Constraint Typed) -> m ()
doSolve Empty = pure ()
doSolve (ConUnify because v a b :<| xs) = do
  sub <- use solveTySubst

  -- traceM (displayS (pretty (ConUnify because v (apply sub a) (apply sub b))))
  co <- memento $ unify (apply sub a) (apply sub b)
  case co of
    Left e -> do
      dictate (reblame because <$> e)
      solveCoSubst . at v ?= IdWrap
    Right co -> solveCoSubst . at v ?= Cast co

  doSolve xs
doSolve (ConSubsume because v a b :<| xs) = do
  sub <- use solveTySubst

  -- traceM (displayS (pretty (ConSubsume because v scope (apply sub a) (apply sub b))))
  let a' = apply sub a
  sub <- use solveTySubst
  co <- memento $ subsumes because a' (apply sub b)
  case co of
    Left e -> do
      dictate e
      solveCoSubst . at v ?= IdWrap
    Right co -> solveCoSubst . at v .= Just co
  doSolve xs

doSolve (ConImplies because not cs ts :<| xs) = do
  before <- use solveTySubst
  assump <- use solveAssumptions
  let not' = ftv (apply before not) <> ftv not
      cs' = apply before cs
      ts' = apply before ts
  ((), sub) <- retcon (fmap DeadBranch) . capture . local (bindSkol .~ True) . local (don'tTouch .~ mempty) $ doSolve cs'

  solveAssumptions .= (sub ^. solveAssumptions <> sub ^. solveTySubst)

  local (don'tTouch %~ Set.union not')
    . retcons (addBlame because) . recover ()
    $ doSolve (fmap (apply (sub ^. solveTySubst)) ts')

  let leaky = Map.filterWithKey (\k _ -> k `Set.notMember` assumptionBound) (sub ^. solveTySubst)
      assumptionBound = not'

  solveTySubst %= Map.union leaky
  solveAssumptions .= assump

  doSolve xs

doSolve (DeferredError e :<| cs) = do
  dictates e
  doSolve cs

doSolve (ConFail a v t :<| cs) = do
  doSolve cs
  sub <- use solveTySubst
  let ex = Hole (unTvName v) (fst a)
  dictates . reblame (BecauseOf ex) $ foundHole v (apply sub t) sub

bind :: MonadSolve m => Var Typed -> Type Typed -> m (Coercion Typed)
bind var ty
  | occurs var ty = confesses (Occurs var ty)
  | TyVar var == ty = pure (ReflCo ty)
  | TyForall{} <- ty = confesses (Impredicative var ty)
  | TyWildcard (Just (TyVar v)) <- ty, v == var = pure (ReflCo ty)
  | otherwise = do
      env <- use solveTySubst
      assum <- use solveAssumptions
      noTouch <- view don'tTouch
      ty <- pure (apply env ty) -- shadowing
      when (occurs var ty) (confesses (Occurs var ty))
      if | var `Set.member` noTouch && var `Map.member` assum ->
            unify ty (apply env (assum Map.! var))
         | TyVar var == ty -> pure (ReflCo ty)
         | otherwise ->
            case Map.lookup var env of
              Nothing -> do
                if | var `Set.notMember` noTouch -> solveTySubst .= env `compose` Map.singleton var ty
                   | var `Set.member` noTouch, TyVar v <- ty, v `Set.notMember` noTouch ->
                     solveTySubst .= (env `compose` Map.singleton v (TyVar var))
                   | otherwise -> confesses =<< unequal (TyVar var) ty
                pure (ReflCo ty)
              Just ty'
                | ty' == ty -> pure (ReflCo (apply env ty'))
                | otherwise -> unify ty (apply env ty')

-- FOR BOTH UNIFY AND SUBSUME:
--  unify have want
--  subsume k have want
-- i.e. The irst argument is the type *we have*.
unify :: MonadSolve m => Type Typed -> Type Typed -> m (Coercion Typed)
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
           else confesses (SkolBinding x (TySkol y))

unify ta@(TyPromotedCon a) tb@(TyPromotedCon b)
  | a == b = pure (ReflCo tb)
  | otherwise = confesses =<< unequal ta tb

unify (TyVar a) b = bind a b
unify a (TyVar b) = SymCo <$> bind b a

unify (TyWildcard (Just a)) b = unify a b
unify a (TyWildcard (Just b)) = SymCo <$> unify b a

unify skt@(TySkol t@(Skolem sv _ _ _)) b = do
  sub <- use (solveAssumptions . at sv)
  case sub of
    Just ty -> do
      _ <- unify b ty
      pure (AssumedCo (TySkol t) ty)
    Nothing -> case b of
      TyVar v -> bind v (TySkol t)
      TyWildcard (Just tau) -> unify skt tau
      _ -> do
        canWe <- view bindSkol
        if canWe
           then if t `Set.notMember` skols b
           then do
             solveAssumptions . at sv ?= b
             pure (AssumedCo (TySkol t) b)
           else confesses (Occurs sv b)
           else confesses (SkolBinding t b)
unify b (TySkol t) = SymCo <$> unify (TySkol t) b

unify (TyArr a b) (TyArr a' b') = ArrCo <$> unify a a' <*> unify b b'

unify l@(TyApp a b) r@(TyApp a' b') =
  (AppCo <$> unify a a' <*> unify b b')
    `catchChronicle` (\e -> confess =<< for e rethrow)
  where
    rethrow NotEqual{} = unequal l r
    rethrow x = pure x

unify ta@(TyCon a) tb@(TyCon b)
  | a == b = pure (ReflCo tb)
  | otherwise = confesses =<< unequal ta tb

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
  | overlaps <- overlap brow arow
  , rhoNew <- deleteFirstsBy ((==) `on` fst) (sortOn fst arow) (sortOn fst brow)
  = if | length overlaps < length brow -> confesses (NoOverlap tb ta)
       | otherwise -> do
          cs <- traverse unifRow overlaps
          _ <- unify rho (TyExactRows rhoNew)
          pure (SymCo (ProjCo rhoNew cs))

unify ta@(TyExactRows arow) tb@(TyExactRows brow)
  | overlaps <- overlap arow brow
  = do when (length overlaps /= length arow || length overlaps /= length brow)
         $ confesses (NoOverlap ta tb)
       cs <- traverse unifRow overlaps
       pure (ExactRowsCo cs)

unify x tp@TyRows{} = confesses (Note (CanNotInstance tp x) isRec)
unify tp@TyRows{} x = confesses (Note (CanNotInstance tp x) isRec)
unify (TyTuple a b) (TyTuple a' b') =
  ProdCo <$> unify a a' <*> unify b b'

unify TyType TyType = pure (ReflCo TyType)
unify a b = confesses =<< unequal a b

subsumes', subsumes :: MonadSolve m => SomeReason -> Type Typed -> Type Typed -> m (Wrapper Typed)
subsumes blame a b = do
  x <- use solveTySubst
  retcons (addBlame blame) $ subsumes' blame (apply x a) (apply x b)

subsumes' b t1 t2@TyPi{} | isSkolemisable t2 = do
  (c, t2') <- skolemise (BySubsumption t1 t2) t2
  (Syntax.:>) c <$> subsumes b t1 t2'

subsumes' b t1@TyPi{} t2 | isSkolemisable t1 = do
  (cont, _, t1') <- instantiate Subsumption t1
  let wrap = maybe IdWrap (WrapFn . flip MkWrapCont "forall <= sigma; instantiation") cont

  flip (Syntax.:>) wrap <$> subsumes b t1' t2

subsumes' r ot@(TyTuple a b) nt@(TyTuple a' b') = do
  -- We must check that..
  (wb, wa) <- retcon Seq.reverse $ do
    -- The second components are related
    wb <- recover IdWrap $ subsumes (secondBlame r) b b'
    -- The first components are related
    wa <- recover IdWrap $ subsumes (firstBlame r) a a'
    pure (wb, wa)
  -- Thus "point-wise" subsumption

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

subsumes' _ (TyApp lazy ty') ty | lazy == tyLazy, lazySubOk ty' ty = do
  co <- unify ty' ty
  -- We have a thunk and want a value
  let wrap ex
        | an <- annotation ex =
          -- ... so force it
          App (ExprWrapper (TypeApp ty) (VarRef forceName (an, forceTy))
                (an, forceTy' ty))
              (ExprWrapper (probablyCast (AppCo (ReflCo tyLazy) co)) ex (an, TyApp lazy ty))
              (an, ty)
  pure (WrapFn (MkWrapCont wrap "automatic forcing"))

subsumes' _ ty' (TyApp lazy ty) | lazy == tyLazy, lazySubOk ty ty' = do
  co <- unify ty ty'
  -- We have a value and want a thunk
  let wrap ex
        | an <- annotation ex =
          App (ExprWrapper (TypeApp ty) (VarRef lAZYName (an, lAZYTy))
                (an, lAZYTy' ty))
              -- So put it in a function to delay evaluation!
              (Fun (PatParam (PLiteral LiUnit (an, tyUnit)))
                (ExprWrapper (probablyCast co) ex (an, ty'))
                (an, TyArr tyUnit ty))
              (an, TyApp lazy ty)
  pure (WrapFn (MkWrapCont wrap "automatic thunking"))

subsumes' r th@(TyExactRows rhas) tw@(TyExactRows rwant) = do
  let matching = overlap rhas rwant

  -- All fields must be present in both records..
  when (length matching /= length rhas || length rhas /= length rwant) $
    confesses (NoOverlap th tw)

  matched <- fmap fold . for matching $ \(key, have, want) -> do
    -- and have to be point-wise subtypes
    wrap <- subsumes' (fieldBlame key r) have want
    pure (Map.singleton key (have, want, wrap))

  exp <- TvName <$> genName

  pure (WrapFn (MkWrapCont (mkRecordWrapper rhas matched tw th tw id exp) "exact→exact record subsumption"))

subsumes' r th@(TyExactRows rhas) tw@(TyRows rho rwant) = do
  let matching = overlap rhas rwant

  -- We need to at *least* match all of the ones we want
  when (length matching < length rwant || length rwant > length rhas) $
    confesses (NoOverlap th tw)

  matched <- fmap fold . for matching $ \(key, have, want) -> do
    -- and make sure that the ones we have are subtypes of the ones we
    -- want
    wrap <- subsumes' (fieldBlame key r) have want
    pure (Map.singleton key (have, want, wrap))

  let diff = filter (not . flip Map.member matched . fst) rhas
      get_t (key, old) = (:) (key, case Map.lookup key matched of
        Just (_, new, _) -> new
        Nothing -> old)

      matched_t = TyExactRows (foldr get_t [] rhas)
      co = ProjCo diff (map (\(key, _, right) -> (key, ReflCo right)) matching)

  -- then produce a coercion to shrink the record.
  _ <- unify rho (TyExactRows diff)
  let cast = probablyCast co
  exp <- TvName <$> genName

  let mkw ex = ExprWrapper cast ex (annotation ex, tw)
  pure (WrapFn (MkWrapCont (mkRecordWrapper rhas matched matched_t th tw mkw exp) "exact→poly record subsumption"))

subsumes' r th@(TyRows rho rhas) tw@(TyRows sigma rwant) = do
  let matching = overlap rhas rwant

  -- We need to at *least* match all of the ones we want
  if length matching < length rwant || length rwant > length rhas
  then do
    _ <- unify th tw
    pure (WrapFn (MkWrapCont (\ex -> Ascription (ExprWrapper (Cast (AssumedCo th tw)) ex (annotation ex, tw)) tw (annotation ex, tw)) "foo"))
  else do
    matched <- fmap fold . for matching $ \(key, have, want) -> do
      -- and make sure that the ones we have are subtypes of the ones we
      -- want
      wrap <- subsumes' (fieldBlame key r) have want
      pure (Map.singleton key (have, want, wrap))

    let diff = filter (not . flip Map.member matched . fst) rhas
        get_t (key, old) = (:) (key, case Map.lookup key matched of
          Just (_, new, _) -> new
          Nothing -> old)
        matched_t = TyExactRows (foldr get_t [] rhas)
        new = case diff of
          [] -> rho
          _ -> TyRows rho diff

    cast <- probablyCast <$> unify sigma new
    exp <- TvName <$> genName

    let mkw ex = Ascription (ExprWrapper cast ex (annotation ex, tw)) tw (annotation ex, tw)
    pure (WrapFn (MkWrapCont (mkRecordWrapper rhas matched matched_t th tw mkw exp) "exact→poly record subsumption"))

subsumes' r a b = probablyCast <$> retcons (reblame r) (unify a b)

-- | Shallowly skolemise a type, replacing any @forall@-bound 'TyVar's
-- with fresh 'Skolem' constants.
skolemise :: MonadNamey m => SkolemMotive Typed -> Type Typed -> m (Wrapper Typed, Type Typed)
skolemise motive ty@(TyPi (Invisible tv k) t) = do
  sk <- freshSkol motive ty tv
  (wrap, ty) <- skolemise motive (apply (Map.singleton tv sk) t)
  kind <- case k of
    Nothing -> freshTV
    Just x -> pure x
  let getSkol (TySkol s) = s
      getSkol _ = error "not a skolem from freshSkol"
  pure (TypeLam (getSkol sk) kind Syntax.:> wrap, ty)
skolemise _ ty = pure (IdWrap, ty)

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
occurs _ (TyWildcard (Just (TyVar _))) = False
occurs v (TyWildcard (Just t)) = occurs v t
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
concretish (TyWildcard t) = maybe False concretish t
concretish _ = True

firstBlame, secondBlame :: SomeReason -> SomeReason
firstBlame (It'sThis (BecauseOfExpr (Tuple (x:_) _) _)) = becauseExp x
firstBlame x = x
secondBlame (It'sThis (BecauseOfExpr (Tuple (_:xs) an) _)) =
  case xs of
    [] -> error "wot"
    [x] -> becauseExp x
    xs@(x:ys) ->
      let len = sconcat (annotation x :| map annotation ys)
       in becauseExp (respan (const len) (Tuple xs an))
secondBlame x = x

fieldBlame :: Text -> SomeReason -> SomeReason
fieldBlame key (It'sThis (BecauseOfExpr (Record rs _) _))
  | Just ex <- select key rs
  = It'sThis $ BecauseOfExpr ex ("field " <> T.unpack key)
fieldBlame key (It'sThis (BecauseOfExpr (RecordExt _ rs _) _))
  | Just ex <- select key rs
  = It'sThis $ BecauseOfExpr ex ("field " <> T.unpack key)
fieldBlame _ e = e

reblame :: SomeReason -> TypeError -> TypeError
reblame (It'sThis (BecauseOfExpr (Record rs _) _)) (Note err note)
  | Just (InField t) <- cast note, Just ex <- select t rs
  = addBlame (It'sThis (BecauseOfExpr ex ("field " <> T.unpack t))) err

reblame (It'sThis (BecauseOfExpr (RecordExt _ rs _) _)) (Note err note)
  | Just (InField t) <- cast note, Just ex <- select t rs
  = addBlame (It'sThis (BecauseOfExpr ex ("field " <> T.unpack t))) err

reblame r e = addBlame r e

unequal :: MonadSolve m => Type Typed -> Type Typed -> m TypeError
unequal a b = do
  x <- use solveTySubst
  pure (NotEqual (apply x a) (apply x b))

select :: Respannable a => Text -> [Field a] -> Maybe (Expr a)
select t = fmap go . find ((== t) . view fName) where
  go (Field _ e s) = respan (const (annotation s)) e

mkRecordWrapper :: [(Text, Type Typed)]
                -> Map.Map Text (Type Typed, Type Typed, Wrapper Typed)
                -> Type Typed
                -> Type Typed -> Type Typed
                -> (Expr Typed -> Expr Typed)
                -> Var Typed -> Expr Typed -> Expr Typed
mkRecordWrapper keys matched matched_t th tw cont exp =
  let ref an = VarRef exp (an, th)
      wrapField (Field k ex (an, old)) =
        case Map.lookup k matched of
          Just (_, new, wrap) -> Field k (ExprWrapper wrap ex (annotation ex, new)) (an, new)
          Nothing -> Field k ex (an, old)

      fakeField :: Ann Resolved -> Expr Typed -> (Text, Type Typed) -> [Field Typed]
      fakeField ant ex (key, have)
        = case Map.lookup key matched of
            Just (_, new, wrap) -> [Field key (ExprWrapper wrap (Access ex key (ant, have)) (ant, new)) (ant, new)]
            Nothing -> [Field key (Access ex key (ant, have)) (ant, have)]

      -- Rebuild the literal in place, or
      wrapEx (Record rs (an, _)) = cont (Record (map wrapField rs) (an, matched_t))
      -- If we have a variable, use it directly, or
      wrapEx ex@(VarRef _ (an, _)) = cont (Record (foldMap (fakeField an ex) keys) (an, matched_t))

      -- Bind the expression to a value so we don't duplicate work and
      -- rebuild the record
      wrapEx ex | an <- annotation ex =
        Let [Binding exp ex (an, th)]
          (cont (Record (foldMap (fakeField an (ref an)) keys) (an, matched_t)))
          (an, tw)
   in wrapEx

lazySubOk :: Type Typed -> Type Typed -> Bool
lazySubOk tlazy tout = concretish tout || head (spine tout) == head (spine tlazy)

overlap :: [(Text, Type p)] -> [(Text, Type p)] -> [(Text, Type p, Type p)]
overlap xs ys
  | inter <- filter ((/=) 1 . length) $ groupBy ((==) `on` fst) (sortOn fst (xs ++ ys))
  = map get inter
  where get [(t, a), (_, b)] = (t, a, b)
        get _ = undefined

newtype InField = InField Text
  deriving Show

instance Pretty InField where
  pretty (InField t) = string "When checking the field" <+> skeyword (text t)
