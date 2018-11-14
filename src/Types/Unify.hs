{-# LANGUAGE MultiWayIf, FlexibleContexts, ScopedTypeVariables,
   TemplateHaskell, TupleSections, ViewPatterns,
   LambdaCase, ConstraintKinds, CPP, TypeFamilies, OverloadedStrings #-}

-- | This module implements the logic responsible for solving the
-- sequence of @Constraint@s the type-checker generates for a particular
-- binding groups.
module Types.Unify
  ( SolveState, emptyState
  , solve, solveImplies
  , skolemise, freshSkol
  , unifyPure
  , applicable
  ) where

import Control.Monad.Except
import Control.Monad.State
import Control.Applicative
import Control.Monad.Infer
import Control.Lens hiding (Empty)

import Types.Infer.Builtin hiding (subsumes, unify)
import Types.Infer.Errors
import Types.Wellformed

import Syntax.Implicits hiding (overlap, spine)
import Syntax.Pretty
import Syntax.Subst
import Syntax

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Set as Set

import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty(..))
import Data.Traversable
import Data.Semigroup hiding (First(..))
import Data.Sequence (Seq ((:<|), Empty))
import Data.Typeable
import Data.Foldable
import Data.Function
import Data.Spanned
import Data.Reason
import Data.Maybe
import Data.These
import Data.Span (internal)
import Data.Text (Text)
import Data.Ord

import Text.Pretty.Semantic

import Prelude hiding (lookup)

#ifdef TRACE_TC
import Debug.Trace
#endif

data SolveScope
  = SolveScope { _bindSkol :: Bool
               , _don'tTouch :: Set.Set (Var Typed)
               , _depth :: [Type Typed]
               }
  deriving (Eq, Show, Ord)

data SolveState
  = SolveState { _solveTySubst :: Subst Typed
               , _solveAssumptions :: Subst Typed
               , _solveCoSubst :: Map.Map (Var Typed) (Wrapper Typed)
               }
  deriving (Eq, Show)

emptyState :: SolveState
emptyState = SolveState mempty mempty mempty

makeLenses ''SolveState
makeLenses ''SolveScope

type MonadSolve m =
  ( MonadNamey m
  , MonadState SolveState m
  , MonadReader SolveScope m
  , MonadChronicles TypeError m
  , MonadWriter [Constraint Typed] m
  )

isRec :: String
isRec = "A record type's hole can only be instanced to another record"

unifyPure :: Type Typed -> Type Typed -> Maybe (Subst Typed)
unifyPure a b = fst . flip runNamey firstName $ do
  x <- runChronicleT $ do
    (sub, _, _) <- solve (pure (ConUnify undefined undefined a b))
    pure sub
  case x of
    These e x | null e -> pure (Just x)
    That x -> pure (Just x)
    _ -> pure Nothing

unifRow :: MonadSolve m => (Text, Type Typed, Type Typed) -> m (Text, Coercion Typed)
unifRow (t, a, b) = do
  co <- retcons (flip Note (InField t)) (unify a b)
  pure (t, co)

runSolve :: MonadNamey m
         => Bool
         -> SolveState
         -> WriterT [Constraint Typed] (StateT SolveState (ReaderT SolveScope m)) a
         -> m ([Constraint Typed], SolveState)
runSolve skol s x = runReaderT (runStateT (execWriterT act) s) emptyScope where
  act = (,) <$> genName <*> x
  emptyScope = SolveScope skol mempty []

  --   let ss = s ^. solveTySubst
    -- pure (fmap (apply ss) ss, s ^. solveCoSubst, cs)

-- | Solve a sequence of constraints, returning either a substitution
-- for both type variables (a 'Subst' 'Typed') and for 'Wrapper'
-- variables.
--
-- The 'Var' 'Desugared' parameter is the first fresh name this
-- particular instance of the solver is allowed to generate from.
solve :: (MonadNamey m, MonadChronicles TypeError m)
      => Seq.Seq (Constraint Typed)
      -> m (Subst Typed, Map.Map (Var Typed) (Wrapper Typed), [Constraint Typed])
solve cs = do
  (cs', s) <- runSolve False emptyState (doSolve cs)
  let ss = s ^. solveTySubst
  pure (fmap (apply ss) ss, s ^. solveCoSubst, cs')

solveImplies :: (MonadNamey m, MonadChronicles TypeError m)
             => SolveState -> Seq.Seq (Constraint Typed)
             -> m (SolveState, [Constraint Typed])
solveImplies s cs = do
  (cs', s') <- runSolve True s . doSolve . apply (s ^. solveTySubst) $ cs
  pure ( s' & solveAssumptions %~ (<>(s ^. solveTySubst))
       , apply (s ^. solveTySubst) cs' )

doSolve :: forall m. MonadSolve m => Seq.Seq (Constraint Typed) -> m ()
doSolve Empty = pure ()
doSolve (ConUnify because v a b :<| xs) = do
  sub <- use solveTySubst

#ifdef TRACE_TC
  traceM (displayS (pretty (ConUnify because v (apply sub a) (apply sub b))))
#endif
  co <- memento $ unify (apply sub a) (apply sub b)
  case co of
    Left e -> do
      dictate (reblame because <$> e)
      solveCoSubst . at v ?= IdWrap
    Right co -> solveCoSubst . at v ?= probablyCast co

  doSolve xs
doSolve (ConSubsume because scope v a b :<| xs) = do
  sub <- use solveTySubst

#ifdef TRACE_TC
  traceM (displayS (shown because <+> pretty because <+> pretty (ConSubsume because scope v (apply sub a) (apply sub b))))
#endif
  let a' = apply sub a
  sub <- use solveTySubst
  co <- memento $ subsumes because scope a' (apply sub b)
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

  ((), sub) <- retcon (fmap DeadBranch) . capture .
    local (bindSkol .~ True) . local (don'tTouch .~ mempty) $
      doSolve cs'

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
  s <- use solveTySubst
  s' <- use solveAssumptions

  let ex :: Expr Desugared = Hole v (fst a)
      sub = s `compose` s'
  dictates . reblame (BecauseOf ex) $ foundHole v t sub

doSolve (ConImplicit _ _ v x :<| cs) | x == tyUnit = do
  doSolve cs
  let wrap ex | an <- annotation ex, ty <- getType ex = App ex (Literal LiUnit (an, tyUnit)) (an, ty)
      wrap :: Expr Typed -> Expr Typed
  solveCoSubst . at v ?= WrapFn (MkWrapCont wrap "unit solution app")

doSolve (ConImplicit why scope v (TyTuple a b) :<| cs) = do
  vara <- genName
  varb <- genName
  solveCoSubst . at v ?= ExprApp
    (Tuple [ ExprWrapper (WrapVar vara)
              (Fun (EvParam (Capture vara (annotation why, a)))
                (VarRef vara (annotation why, a))
                (annotation why, TyArr a a))
              (annotation why, a)
           , ExprWrapper (WrapVar varb)
              (Fun (EvParam (Capture varb (annotation why, b)))
                (VarRef varb (annotation why, b))
                (annotation why, TyArr b b))
              (annotation why, b)]
           (annotation why, TyTuple a b))
  doSolve (ConImplicit why scope vara a :< ConImplicit why scope varb b :<| cs)

doSolve (ohno@(ConImplicit reason scope var con@TyPi{}) :<| cs) = do
  doSolve cs
  sub <- use solveTySubst
  con <- pure (apply sub con)
  scope <- pure (mapTypes (apply sub) scope)

  let (head, _) = splitImplVarType con
  case filter ((/= Superclass) . view implSort) $ lookup head scope of
    xs | allSameHead xs, concreteUnderOne con -> do
      w <- local (depth %~ (con:)) $
        useImplicit reason con scope (pickBestPossible xs)
      solveCoSubst . at var ?= w
    _ -> dictates (addBlame reason (UnsatClassCon reason (apply sub ohno) It'sQuantified))

doSolve (ohno@(ConImplicit reason scope var cons) :<| cs) = do
  doSolve cs
  sub <- use solveTySubst
  cons <- pure (apply sub cons)
  scope <- pure (mapTypes (apply sub) scope)

#ifdef TRACE_TC
  traceM (displayS (shown scope <+> pretty reason <+> pretty (apply sub ohno)))
#endif

  x <- view depth
  if length x >= 25
     then confesses (ClassStackOverflow reason x cons)
     else case lookup cons scope of
           xs | allSameHead xs
              , concreteUnderOne cons || hasExactMatch cons xs
              , applic <- filter (applicable cons scope) xs
              , not (null applic) -> do
             let imp = pickBestPossible applic
             w <- local (depth %~ (cons :)) $
               useImplicit reason cons scope imp
             solveCoSubst . at var ?= w
           _ -> do
#ifdef TRACE_TC
             traceM "  propagated"
#endif
             solveCoSubst . at var ?= ExprApp (VarRef var (annotation reason, cons))
             tell (pure (apply sub ohno))

allSameHead :: [Implicit Typed] -> Bool
allSameHead (x:xs) = all (matches (x ^. implHead) . view implHead) xs
allSameHead [] = True

hasExactMatch :: Type Typed -> [Implicit Typed] -> Bool
hasExactMatch t = any ((== t) . view implHead)

pickBestPossible :: [Implicit Typed] -> Implicit Typed
pickBestPossible [] = error "No choices"
pickBestPossible xs = minimumBy (comparing (length . view implPre)) xs

useImplicit :: forall m. MonadSolve m
            => SomeReason -> Type Typed -> ImplicitScope Typed
            -> Implicit Typed -> m (Wrapper Typed)
useImplicit reason ty scope (ImplChoice _ oty _ imp _ _) = go where
  go :: m (Wrapper Typed)
  go = do
    w <- subsumes' reason scope oty ty
    let start = VarRef imp (annotation reason, oty)
    pure (ExprApp (ExprWrapper w start (annotation reason, ty)))

bind :: MonadSolve m => Var Typed -> Type Typed -> m (Coercion Typed)
bind var ty
  | TyVar var == ty = pure (ReflCo ty)
  -- /\ Var-var deletion
  | TyWildcard (Just (TyVar v)) <- ty, v == var = pure (ReflCo ty)
  -- /\ Var-wildcard deletion (same as above, but with an indirection)
  | TyForall{} <- ty = confesses (Impredicative var ty)
  -- /\ Predicativity checking
  | otherwise = do
      env <- use solveTySubst
      assum <- use solveAssumptions
      noTouch <- view don'tTouch
      ty <- pure (apply (env `compose` assum) ty) -- shadowing

      when (occurs var ty) $
        confesses (Occurs var ty)

      if | TyVar var == ty -> pure (ReflCo ty)
         | otherwise ->
            case Map.lookup var env of
              Nothing -> do
                if | var `Set.notMember` noTouch ->
                     solveTySubst .= env `compose` Map.singleton var ty
                     -- /\ We're allowed to bind the variable, so just
                     -- do it.

                   | var `Set.member` noTouch, TyVar v <- ty, v `Set.notMember` noTouch ->
                     solveTySubst .= (env `compose` Map.singleton v (TyVar var))
                     -- /\ The equality is of the form v1 := v2, where
                     -- v1 is bad but v2 is free. So what we do is add
                     -- v2 := v1 to the environment.

                   | otherwise -> confesses =<< unequal (TyVar var) ty
                   -- No can do.
                pure (ReflCo ty)
              Just ty' -> unify ty (apply env ty')

-- FOR BOTH UNIFY AND SUBSUME:
--  unify have want
--  subsume k have want
-- i.e. The irst argument is the type *we have*.
unify :: MonadSolve m => Type Typed -> Type Typed -> m (Coercion Typed)
unify (TySkol x) (TySkol y)
  | x == y = pure (ReflCo (TySkol y))
  | otherwise = do
    sub <- use solveAssumptions
    let assumption = (Right <$> sub ^. at (x ^. skolIdent)) <|> (Left <$> sub ^. at (y ^. skolIdent))
    case assumption of
      Just assum ->
        case assum of
          Right x -> unify x (TySkol y)
          Left y -> unify (TySkol x) y
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

-- ((->) a b) = a -> b
unify (TyApp (TyApp (TyCon v) l) r) (TyArr l' r')
  | TgName _ (-38) <- v = ArrCo <$> unify l l' <*> unify r r'

unify (TyArr l r) (TyApp (TyApp (TyCon v) l') r')
  | TgName _ (-38) <- v = ArrCo <$> unify l l' <*> unify r r'

unify (TyApp f g) (TyArr l r) = ArrCo <$> unify f (TyApp (TyCon (TgName "->" (-38))) l) <*> unify g r
unify (TyArr l r) (TyApp f g) = ArrCo <$> unify (TyApp (TyCon (TgName "->" (-38))) l) f <*> unify r g

unify (TyArr a b) (TyArr a' b') = ArrCo <$> unify a a' <*> unify b b'
unify (TyPi (Implicit a) b) (TyPi (Implicit a') b') =
  ArrCo <$> unify a a' <*> unify b b' -- Technically cheating but yay desugaring

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
  , rhoNew <- L.deleteFirstsBy ((==) `on` fst) (L.sortOn fst arow) (L.sortOn fst brow)
  , sigmaNew <- L.deleteFirstsBy ((==) `on` fst) (L.sortOn fst brow) (L.sortOn fst arow) =
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
  , rhoNew <- L.deleteFirstsBy ((==) `on` fst) (L.sortOn fst arow) (L.sortOn fst brow)
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

-- ((*) a b) = a * b
unify (TyApp (TyApp (TyCon v) l) r) (TyTuple l' r')
  | TgName _ (-39) <- v = ProdCo <$> unify l l' <*> unify r r'

unify (TyTuple l r) (TyApp (TyApp (TyCon v) l') r')
  | TgName _ (-39) <- v = ProdCo <$> unify l l' <*> unify r r'

unify (TyApp f g) (TyTuple l r) = ProdCo <$> unify f (TyApp (TyCon (TgName "*" (-39))) l) <*> unify g r
unify (TyTuple l r) (TyApp f g) = ProdCo <$> unify (TyApp (TyCon (TgName "*" (-39))) l) f <*> unify r g

unify (TyOperator l v r) (TyTuple l' r')
  | TgName _ (-39) <- v = ProdCo <$> unify l l' <*> unify r r'

unify (TyTuple l r) (TyOperator l' v r')
  | TgName _ (-39) <- v = ProdCo <$> unify l l' <*> unify r r'

unify (TyTuple a b) (TyTuple a' b') =
  ProdCo <$> unify a a' <*> unify b b'

unify (TyOperator l v r) (TyApp f g) = AppCo <$> unify (TyApp (TyCon v) l) f <*> unify g r
unify (TyApp f g) (TyOperator l v r) = AppCo <$> unify f (TyApp (TyCon v) l) <*> unify r g

unify TyType TyType = pure (ReflCo TyType)
unify a b = confesses =<< unequal a b

subsumes', subsumes :: MonadSolve m
                    => SomeReason -> ImplicitScope Typed
                    -> Type Typed -> Type Typed -> m (Wrapper Typed)
subsumes blame scope a b = do
  x <- use solveTySubst
  retcons (addBlame blame) $ subsumes' blame scope (apply x a) (apply x b)

subsumes' b s t1 t2@TyPi{} | isSkolemisable t2 = do
  (c, t2', scope) <- skolemise (BySubsumption t1 t2) t2
  (Syntax.:>) c <$> subsumes b (scope <> s) t1 t2'

subsumes' r s (TyPi (Implicit t) t1) t2
  | prettyConcrete t2 = do
      var <- genName
      omega <- subsumes' r s t1 t2
      let con = ConImplicit r s var t
      doSolve (Seq.singleton con)
      let wrap ex | an <- annotation ex
            = ExprWrapper omega
                (ExprWrapper (TypeAsc t1)
                   (ExprWrapper (WrapVar var) ex (an, t1)) (an, t1)) (an, t2)
       in pure (WrapFn (MkWrapCont wrap ("implicit instantation " ++ show (pretty t))))
  | TyVar{} <- t1, TyVar{} <- t2, show r /= "pattern blame" = do
      var <- genName
      omega <- subsumes' r s t1 t2
      let con = ConImplicit r s var t
      doSolve (Seq.singleton con)
      let wrap ex | an <- annotation ex
            = ExprWrapper omega
                (ExprWrapper (TypeAsc t1)
                   (ExprWrapper (WrapVar var) ex (an, t1)) (an, t1)) (an, t2)
       in pure (WrapFn (MkWrapCont wrap ("implicit instantation " ++ show (pretty t))))

  | otherwise = probablyCast <$> unify (TyPi (Implicit t) t1) t2

subsumes' b s t1@TyPi{} t2 | isSkolemisable t1 = do
  (cont, _, t1') <- instantiate Subsumption t1
  omega <- subsumes b s t1' t2
  let inst = fromMaybe id cont
      wrap ex = ExprWrapper omega (inst ex) (annotation ex, t2)
  pure (WrapFn (MkWrapCont wrap ("instantiation " ++ show t1 ++ " => " ++ show t2)))

subsumes' r scope ot@(TyTuple a b) nt@(TyTuple a' b') = do
  -- We must check that..
  (wb, wa) <- retcon Seq.reverse $ do
    -- The second components are related
    wb <- recover IdWrap $ subsumes (secondBlame r) scope b b'
    -- The first components are related
    wa <- recover IdWrap $ subsumes (firstBlame r) scope a a'
    pure (wb, wa)
  -- Thus "point-wise" subsumption

  ~[elem, elem'] <- replicateM 2 genName
  let cont (Tuple (e:es) (an, _)) =
        Tuple [ ExprWrapper wa e (an, a')
              , case es of
                 [e] -> ExprWrapper wb e (an, b')
                 xs -> ExprWrapper wb (Tuple xs (an, b)) (an, b')
              ]
              (an, nt)
      cont ex | IdWrap <- wa, IdWrap <- wb = ex
      cont ex
        | an <- annotation ex =
          Match ex [ Arm
                     ( PTuple [ Capture elem (an, a), Capture elem' (an, b) ] (an, ot) )
                     Nothing
                     ( Tuple [ ExprWrapper wa (VarRef elem (an, a)) (an, a')
                             , ExprWrapper wb (VarRef elem' (an, b)) (an, b') ]
                             (an, nt)) ]
                   (an, nt)
  pure (WrapFn (MkWrapCont cont "tuple re-packing"))

subsumes' _ _ a@(TyApp lazy _) b@(TyApp lazy' _)
  | lazy == lazy', lazy' == tyLazy = probablyCast <$> unify a b

subsumes' _ _ (TyApp lazy ty') ty | lazy == tyLazy, lazySubOk ty' ty = do
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

subsumes' _ _ ty' (TyApp lazy ty) | lazy == tyLazy, lazySubOk ty ty' = do
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

subsumes' r scope th@(TyExactRows rhas) tw@(TyExactRows rwant) = do
  let matching = overlap rhas rwant

  -- All fields must be present in both records..
  unless (length matching <= length rhas && length rwant <= length matching) $
    confesses (NoOverlap tw th)

  matched <- fmap fold . for matching $ \(key, have, want) -> do
    -- and have to be point-wise subtypes
    wrap <- subsumes' (fieldBlame key r) scope have want
    pure (Map.singleton key (have, want, wrap))

  exp <- genName

  pure (WrapFn (MkWrapCont (mkRecordWrapper rhas matched tw th tw id exp) "exact→exact record subsumption"))

subsumes' r scope th@(TyExactRows rhas) tw@(TyRows rho rwant) = do
  let matching = overlap rhas rwant

  -- We need to at *least* match all of the ones we want
  when (length matching < length rwant || length rwant > length rhas) $
    confesses (NoOverlap th tw)

  matched <- fmap fold . for matching $ \(key, have, want) -> do
    -- and make sure that the ones we have are subtypes of the ones we
    -- want
    wrap <- subsumes' (fieldBlame key r) scope have want
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
  exp <- genName

  let mkw ex = ExprWrapper cast ex (annotation ex, tw)
  pure (WrapFn
          (MkWrapCont (mkRecordWrapper rhas matched matched_t th tw mkw exp)
            "exact→poly record subsumption"))

subsumes' r scope th@(TyRows rho rhas) tw@(TyRows sigma rwant) = do
  let matching = overlap rhas rwant

  -- We need to at *least* match all of the ones we want
  if length matching < length rwant || length rwant > length rhas
  then do
    _ <- unify th tw
    pure (probablyCast (AssumedCo th tw))
  else do
    matched <- fmap fold . for matching $ \(key, have, want) -> do
      -- and make sure that the ones we have are subtypes of the ones we
      -- want
      wrap <- subsumes' (fieldBlame key r) scope have want
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
    exp <- genName

    let mkw ex = ExprWrapper cast ex (annotation ex, tw)
    pure (WrapFn
            (MkWrapCont (mkRecordWrapper rhas matched matched_t th tw mkw exp)
              "exact→poly record subsumption"))

subsumes' r _ a b = probablyCast <$> retcons (reblame r) (unify a b)

-- | Shallowly skolemise a type, replacing any @forall@-bound 'TyVar's
-- with fresh 'Skolem' constants.
skolemise :: MonadNamey m
          => SkolemMotive Typed
          -> Type Typed -> m (Wrapper Typed, Type Typed, ImplicitScope Typed)
skolemise motive ty@(TyPi (Invisible tv k) t) = do
  sk <- freshSkol motive ty tv
  (wrap, ty, scope) <- skolemise motive (apply (Map.singleton tv sk) t)
  kind <- case k of
    Nothing -> freshTV
    Just x -> pure x
  let getSkol (TySkol s) = s
      getSkol _ = error "not a skolem from freshSkol"
  pure (TypeLam (getSkol sk) kind Syntax.:> wrap, ty, scope)

skolemise motive wt@(TyPi (Implicit ity) t) = do
  (omega, ty, scp) <- skolemise motive t
  let go (TyTuple a b) = do
        var <- genName
        (pat, scope) <- go b
        pure (Capture var (internal, a):pat, insert internal LocalAssum var a scope)
      go x = do
        var <- genName
        pure ([Capture var (internal, x)], insert internal LocalAssum var x scp)
  (pat, scope) <- go ity
  let wrap ex | an <- annotation ex =
        Fun (EvParam (PTuple pat (an, ity)))
          (ExprWrapper omega ex (an, ty)) (an, wt)
  pure (WrapFn (MkWrapCont wrap "constraint lambda"), ty, scope)

skolemise _ ty = pure (IdWrap, ty, mempty)

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

applicable :: Type Typed -> ImplicitScope Typed -> Implicit Typed -> Bool
applicable wanted scp (ImplChoice head _ cs _ s _) =
  case s of
    Superclass -> isJust (unifyPure wanted head) && all (entails scp) cs
    _ -> isJust (unifyPure wanted head)
  where
    entails :: ImplicitScope Typed -> Obligation Typed -> Bool
    entails _ (Quantifier (Invisible v _)) = v `Map.member` sub
    entails scp (Implication c) | c <- apply sub c =
      any (applicable c scp) (lookup c scp)
    entails _ _ = False

    sub :: Subst Typed
    Just sub = unifyPure head wanted


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
  var <- genName
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

prettyConcrete :: Type Typed -> Bool
prettyConcrete TyVar{} = False
prettyConcrete (TyWildcard t) = maybe False prettyConcrete t
prettyConcrete _ = True

concreteUnderOne :: Type Typed -> Bool
concreteUnderOne TyVar{} = False
concreteUnderOne (TyApp f x) = prettyConcrete f && concretish (head (spine x))
concreteUnderOne _ = True

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

select :: Respannable (Ann a) => Text -> [Field a] -> Maybe (Expr a)
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

      updateField :: Ann Desugared -> Expr Typed -> (Text, Type Typed) -> [Field Typed]
      updateField ant ex (key, have)
        = case Map.lookup key matched of
            Just (_, _, IdWrap) -> []
            Just (_, new, wrap) ->
              [Field key (ExprWrapper wrap (Access ex key (ant, have)) (ant, new)) (ant, new)]
            Nothing -> []

      recordExt ex [] _ = ex
      recordExt ex rs a = RecordExt ex rs a

      -- Rebuild the literal in place, or
      wrapEx (Record rs (an, _)) = cont (Record (map wrapField rs) (an, matched_t))
      -- If we have a variable, use it directly, or
      wrapEx ex@(VarRef _ (an, _)) = cont (recordExt ex (foldMap (updateField an ex) keys) (an, matched_t))

      -- Update the record in place
      wrapEx ex | an <- annotation ex =
        Let [Binding exp ex (an, th)]
          (cont (recordExt (ref an) (foldMap (updateField an (ref an)) keys) (an, matched_t)))
          (an, tw)
   in wrapEx

lazySubOk :: Type Typed -> Type Typed -> Bool
lazySubOk tlazy tout = concretish tout || head (spine tout) == head (spine tlazy)

overlap :: [(Text, Type p)] -> [(Text, Type p)] -> [(Text, Type p, Type p)]
overlap xs ys
  | inter <- filter ((/=) 1 . length) $ L.groupBy ((==) `on` fst) (L.sortOn fst (xs ++ ys))
  = map get inter
  where get [(t, a), (_, b)] = (t, a, b)
        get _ = undefined

newtype InField = InField Text
  deriving Show

instance Pretty InField where
  pretty (InField t) = string "When checking the field" <+> skeyword (text t)
