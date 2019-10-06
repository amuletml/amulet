{-# LANGUAGE MultiWayIf, FlexibleContexts, ScopedTypeVariables,
   TemplateHaskell, TupleSections, ViewPatterns,
   LambdaCase, ConstraintKinds, CPP, TypeFamilies, OverloadedStrings, RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- | This module implements the logic responsible for solving the
-- sequence of @Constraint@s the type-checker generates for a particular
-- binding groups.
module Types.Unify
  ( SolveState, emptyState
  , typeWithin
  , solve, solveImplies
  , skolemise, freshSkol
  , unifyPure_v, unifyPure
  , applicable, getSolveInfo
  , prettyConcrete
  ) where

import Control.Monad.Except
import Control.Monad.State
import Control.Applicative
import Control.Monad.Infer
import Control.Lens hiding (Empty, (:>))

import Control.Exception (assert)

import Types.Infer.Errors
import Types.Wellformed

import Syntax.Implicits hiding (overlap)
import Syntax.Transform
import Syntax.Builtin
import Syntax.Subst
import Syntax.Types
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

import Types.Unify.Magic

import Prelude hiding (lookup)

#ifdef TRACE_TC

import Text.Show.Pretty (ppShow)
import Syntax.Pretty
import qualified Debug.Trace as Dbg

#endif

type SolverInfo = Map.Map (Var Typed) (Either ClassInfo TySymInfo)

data SolveScope
  = SolveScope { _bindSkol :: Bool
               , _don'tTouch :: Set.Set (Var Typed)
               , _depth :: [Type Typed]
               , _solveInfo :: SolverInfo
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

typeWithin :: Var Typed -> SolveState -> Maybe (Type Typed)
typeWithin v s = s ^. (solveTySubst . at v) <|> s ^. (solveAssumptions . at v)

isRec :: String
isRec = "A record type's hole can only be instanced to another record"

unifyPure :: Type Typed -> Type Typed -> Maybe (Subst Typed)
unifyPure a b = unifyPure_v [(a, b)]

unifyPure_v :: [(Type Typed, Type Typed)] -> Maybe (Subst Typed)
unifyPure_v ts = fst . flip runNamey firstName $ do
  let err_unify_pure = error "unifyPure_v: forced variable substitution in pure unifier should be impossible"
  x <- runChronicleT $ do
    (sub, _, _) <- solve (fmap (uncurry (ConUnify (It'sThis BecauseInternal) mempty err_unify_pure)) (Seq.fromList ts)) mempty
    pure sub
  case x of
    These e x | null e -> pure (Just x)
    That x -> pure (Just x)
    _ -> pure Nothing

unifRow :: MonadSolve m => ImplicitScope ClassInfo Typed -> (Text, Type Typed, Type Typed) -> m (Text, Coercion Typed)
unifRow scope (t, a, b) = do
  co <- retcons (flip Note (InField t)) (unify scope a b)
  pure (t, co)

runSolve :: MonadNamey m
         => Bool
         -> SolverInfo
         -> SolveState
         -> WriterT [Constraint Typed] (StateT SolveState (ReaderT SolveScope m)) a
         -> m ([Constraint Typed], SolveState)
runSolve skol info s x = runReaderT (runStateT (execWriterT act) s) emptyScope where
  act = (,) <$> genName <*> x
  emptyScope = SolveScope skol mempty [] info

getSolveInfo :: MonadReader Env m => m SolverInfo
getSolveInfo = do
  x <- fmap Left <$> view classDecs
  y <- fmap Right <$> view tySyms
  pure (x <> y)

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
      -> SolverInfo
      -> m (Subst Typed, Map.Map (Var Typed) (Wrapper Typed), [Constraint Typed])
solve cs info = do
  (cs', s) <- runSolve False info emptyState (doSolve cs)
  let ss = s ^. solveTySubst
  pure (fmap (apply ss) ss, s ^. solveCoSubst, cs')

solveImplies :: (MonadNamey m, MonadChronicles TypeError m)
             => SolveState -> SolverInfo -> Seq.Seq (Constraint Typed)
             -> m (SolveState, [Constraint Typed])
solveImplies s info cs = do
  (cs', s') <- runSolve True info s . doSolve . apply (s ^. solveTySubst) $ cs
  pure ( s' & solveAssumptions %~ (<>(s ^. solveTySubst))
       , apply (s ^. solveTySubst) cs' )

doSolve :: forall m. MonadSolve m => Seq.Seq (Constraint Typed) -> m () -- {{{
doSolve Empty = pure ()
doSolve (ConUnify because scope v a b :<| xs) = do
  sub <- use solveTySubst
  scope <- pure (apply sub scope)

  traceM (displayS (keyword "[W]:" <+> pretty (ConUnify because scope v (apply sub a) (apply sub b))))

  co <- censor (reblame_con because) $ memento $ unify scope (apply sub a) (apply sub b)
  case co of
    Left e -> do
      dictate (reblame because <$> e)
      solveCoSubst . at v ?= IdWrap
    Right co -> solveCoSubst . at v ?= probablyCast co

  doSolve xs

doSolve (ConSubsume because scope v a b :<| xs) = do
  sub <- use solveTySubst

  traceM (displayS (pretty (ConSubsume because scope v (apply sub a) (apply sub b))))
  let a' = apply sub a
  sub <- use solveTySubst
  co <- censor (reblame_con because) $ memento $ subsumes because scope a' (apply sub b)
  case co of
    Left e -> do
      dictate e
      solveCoSubst . at v ?= IdWrap
    Right co -> solveCoSubst . at v .= Just co
  doSolve xs

doSolve (ConImplies because not cs ts :<| xs) = do -- {{{
  doSolve xs
  before <- use solveTySubst
  assump <- use solveAssumptions
  let not' = ftv (apply before not) <> ftv not
      cs' = apply before cs
      ts' = apply before ts

  traceM (displayS (pretty (ConImplies because not cs' ts')))

  ((), sub) <- retcon (fmap (addBlame because . DeadBranch)) . capture .
    local (bindSkol .~ True) . local (don'tTouch .~ mempty) $
      doSolve cs'

  solveAssumptions .= (sub ^. solveAssumptions <> sub ^. solveTySubst)

  local (don'tTouch %~ Set.union not')
    . retcons (addBlame because) . recover ()
    $ doSolve (fmap (apply (sub ^. solveTySubst)) ts')

  let leaky = Map.filterWithKey (\k _ -> k `Set.notMember` assumptionBound) (sub ^. solveTySubst)
      assumptionBound = not'

  solveTySubst %= Map.union leaky
  solveAssumptions .= assump -- }}}

doSolve (DeferredError e :<| cs) = do
  dictates e
  doSolve cs

doSolve (ConFail env a v t :<| cs) = do
  doSolve cs
  s <- use solveTySubst
  s' <- use solveAssumptions

  let ex :: Expr Desugared = Hole v (fst a)
      sub = s `compose` s'
  dictates . reblame (BecauseOf ex) =<< foundHole (apply sub env) a v t sub

doSolve (ConImplicit _ _ v x :<| cs) | x == tyUnit = do
  doSolve cs
  let wrap ex | an <- annotation ex, ty <- getType ex = App ex (Literal LiUnit (an, tyUnit)) (an, ty)
      wrap :: Expr Typed -> Expr Typed
  solveCoSubst . at v ?= WrapFn (MkWrapCont wrap "unit solution app")

doSolve (ConImplicit why scope v (TyTuple a b) :<| cs) = do -- {{{
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
  doSolve (ConImplicit why scope varb b :< ConImplicit why scope vara a :<| cs) -- }}}

doSolve (ohno@(ConImplicit reason scope var con@TyPi{}) :<| cs) = do -- {{{
  doSolve cs
  sub <- use solveTySubst
  con <- pure (apply sub con)
  scope <- pure (apply sub scope)

  (wrap, head, assum, _) <- skolemise (ByConstraint con) con
  traceM (displayS ("quantified constraint. Solving:" <+> pretty head))
  var' <- genName

  doSolve (Seq.singleton (ConImplicit reason (scope <> assum) var' head))

  x <- use (solveCoSubst . at var')
  case x of
    Nothing -> dictates (addBlame reason (UnsatClassCon reason (apply sub ohno) It'sQuantified))
    Just w -> do
      let wanted = ExprWrapper (wrap :> w) (identity head) (a, con)
          identity t = Fun (EvParam (PType (Capture var (a, t)) t (a, t))) (VarRef var (a, t)) (a, TyArr t t)
          a = annotation reason

      solveCoSubst . at var ?= ExprApp wanted -- }}}

doSolve (ohno@(ConImplicit reason scope var cons) :<| cs) = do -- {{{
  doSolve cs
  sub <- use solveTySubst
  cons <- pure (apply sub cons)
  scope <- pure (apply sub scope)

  traceM (displayS (pretty (apply sub ohno)))

  let possible = lookup cons scope
  traceM ("considering between: " ++ show (map (pretty . view implType) possible))
  -- traceM (ppShow scope)

  ignored <- freshTV

  case possible of
    xs | True, allSameHead xs
       , concreteUnderOne cons || hasExactMatch cons xs
       , applic <- filter (applicable cons scope) xs
       , not (null applic) -> do
      let imp = pickBestPossible applic

      traceM (displayS ("best possible:" <+> pretty var <+> pretty (imp ^. implVar) <+> pretty (imp ^. implType)))

      w <- local (depth %~ (cons :)) $
        useImplicit reason cons scope imp
          `catchChronicle`
            \e -> confesses (usingFor imp cons (headSeq e))
      solveCoSubst . at var ?= w

    -- TODO: see if sound
    (filter ((/= Superclass) . view implSort) -> [imp])
      | not (concreteUnderOne cons), imp ^. implSort == LocalAssum || fundepsAllow imp cons -> do
      traceM (displayS ("only possible:" <+> pretty var <+> pretty (imp ^. implVar) <+> pretty (imp ^. implType)))

      w <- useImplicit reason cons scope imp
          `catchChronicle`
            \e -> confesses (usingFor imp cons (headSeq e))
      solveCoSubst . at var ?= w

    _ | let tup = TyTuple cons ignored, [imp] <- lookup tup scope -> do
      traceM (displayS ("decomposing tuple:" <+> pretty (imp ^. implType)))
      w <- useImplicit reason tup scope imp
            `catchChronicle` \e -> confesses (usingFor imp cons (headSeq e))
      v <- genName

      let pi1 = ExprWrapper w
                  (Fun (PatParam (PTuple [Capture v (an, cons), Wildcard (an, ignored)]
                                    (an, tup)))
                    (VarRef v (an, cons)) (an, tup :-> cons))
                  (an, cons)
          an = annotation reason
      solveCoSubst . at var ?= ExprApp pi1

    _ | let tup = TyTuple ignored cons, [imp] <- lookup tup scope -> do
      traceM (displayS ("decomposing tuple:" <+> pretty (imp ^. implType)))
      w <- useImplicit reason tup scope imp
            `catchChronicle` \e -> confesses (usingFor imp cons (headSeq e))
      v <- genName

      let pi1 = ExprWrapper w
                  (Fun (PatParam (PTuple [Capture v (an, cons), Wildcard (an, ignored)]
                                    (an, tup)))
                    (VarRef v (an, cons)) (an, tup :-> cons))
                  (an, cons)
          an = annotation reason
      solveCoSubst . at var ?= ExprApp pi1

    _ ->
      case head (appsView cons) of
        TyCon v | Just solve <- magicClass v -> do
          (w, cs) <- censor (const mempty) $ listen $ solve reason scope (apply sub cons)
          doSolve (Seq.fromList cs)
            `catchChronicle`
              \e -> confesses (ArisingFrom (UnsatClassCon reason (apply sub ohno) (MagicErrors (toList e))) reason)
          case w of
            Just solution -> solveCoSubst . at var ?= solution
            Nothing -> do
              traceM " => quantifiying over magic class"
              solveCoSubst . at var ?= ExprApp (VarRef var (annotation reason, cons))
              tell (pure (apply sub ohno))
        _ -> do
          solveCoSubst . at var ?= ExprApp (VarRef var (annotation reason, cons))
          tell (pure (apply sub ohno))
--- }}}

-- }}}

allSameHead :: [Implicit ClassInfo Typed] -> Bool
allSameHead (x:xs) = all (matches (x ^. implHead) . view implHead) xs
allSameHead [] = True

hasExactMatch :: Type Typed -> [Implicit ClassInfo Typed] -> Bool
hasExactMatch t = any ((== t) . view implHead)

pickBestPossible :: [Implicit ClassInfo Typed] -> Implicit ClassInfo Typed
pickBestPossible [] = error "No choices"
pickBestPossible xs = head best where
  ~(x:_) = L.groupBy ((==) `on` superclasses) (L.sortOn superclasses xs)
  best = L.sortOn specificity x
  specificity t = countConstructors (t ^. implHead)

  superclasses = length . filter isSuperclass . view implPre

  isSuperclass Implication{} = True
  isSuperclass Quantifier{} = False

usingFor :: Implicit ClassInfo Typed -> Type Typed -> TypeError -> TypeError
usingFor _ _ x@Note{} = x
usingFor _ _ x@(ArisingFrom Note{} _) = x
usingFor _ _ x@CustomTypeError{} = x
usingFor _ _ x@(ArisingFrom CustomTypeError{} _) = x
usingFor i ty e =
  Note e . indent (-4) $
    vsep [ mempty
         , "When considering the instance"
         , indent 2 $ displayType (i ^. implType)
         , "as a solution for the constraint"
         , indent 2 $ displayType ty
         ]

headSeq :: Seq.Seq a -> a
headSeq (Seq.viewl -> (x Seq.:< _)) = x
headSeq _ = undefined

useImplicit :: forall m. MonadSolve m
            => SomeReason -> Type Typed -> ImplicitScope ClassInfo Typed
            -> Implicit ClassInfo Typed -> m (Wrapper Typed)
useImplicit reason ty scope (ImplChoice _ oty _ imp _ _ _) = go where
  go :: m (Wrapper Typed)
  go = do
    guardClassOverflow reason ty

    w <- subsumes' reason scope oty ty
    let start = VarRef imp (annotation reason, oty)
    pure (ExprApp (Ascription (ExprWrapper w start (annotation reason, ty))
                    ty
                    (annotation reason, ty)))

-- 'ImplChoice's for which 'implSort' /= 'InstSort' never have their
-- 'implClass' forced. That means that 'Superclass' and 'LocalAssum'
-- evidence can have 'undefined' implClass'.
fundepsAllow :: Implicit ClassInfo Typed -> Type Typed -> Bool
fundepsAllow impl cons
  | impl ^. implSort /= InstSort || null (impl ^. implClass . ciFundep) = False
  | otherwise = all fine (impl ^. implClass . ciFundep)
  where
    fine (from, _, _) = all concreteP from
    (_:params) = appsView cons

    concreteP = concretish . head . appsView . (params !!)

bind :: MonadSolve m => ImplicitScope ClassInfo Typed -> Var Typed -> Type Typed -> m (Coercion Typed) --- {{{
bind scope var ty
  | TyVar var == ty = pure (ReflCo ty)
  -- /\ Var-var deletion
  | TyWildcard (Just (TyVar v)) <- ty, v == var = pure (ReflCo ty)
  -- /\ Var-wildcard deletion (same as above, but with an indirection)
  | TyPi (Invisible _ _ r) _ <- ty, r /= Req = confesses (Impredicative var ty)
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
              Just ty' -> unify scope ty (apply env ty')
--- }}}

-- FOR BOTH UNIFY AND SUBSUME:
--  unify have want
--  subsume k have want
-- i.e. The irst argument is the type *we have*.
unify :: MonadSolve m => ImplicitScope ClassInfo Typed -> Type Typed -> Type Typed -> m (Coercion Typed) -- {{{

#ifdef TRACE_TC
unify _ a b | trace (displayS (keyword "unify:" <+> pretty a <+> soperator (char '~') <+> pretty b)) False = undefined
#endif

unify scope (TySkol x) (TySkol y) -- {{{
  | x == y = pure (ReflCo (TySkol y))
  | otherwise = do
    sub <- use solveAssumptions
    info <- view solveInfo
    let assumption = (Right <$> sub ^. at (x ^. skolIdent)) <|> (Left <$> sub ^. at (y ^. skolIdent))
    case assumption of
      Just assum ->
        case assum of
          Right x -> unify scope x (TySkol y)
          Left y -> unify scope (TySkol x) y
      Nothing ->
        case lookupEquality info scope sub (TySkol x) (TySkol y) of
          (x:_) -> pure x
          _ -> do
            canWe <- view bindSkol
            if canWe
               then do
                 solveAssumptions . at (x ^. skolIdent) ?= TySkol y
                 pure (AssumedCo (TySkol x) (TySkol y))
               else confesses (SkolBinding x (TySkol y))
-- }}}

unify _ ta@(TyPromotedCon a) tb@(TyPromotedCon b)
  | a == b = pure (ReflCo tb)
  | otherwise = confesses =<< unequal ta tb

unify _ ta@(TyLit a) tb@(TyLit b)
  | a == b = pure (ReflCo tb)
  | otherwise = confesses =<< unequal ta tb

unify scope (TyVar a) b = bind scope a b
unify scope a (TyVar b) = SymCo <$> bind scope b a

unify scope (TyWildcard (Just a)) b = unify scope a b
unify scope a (TyWildcard (Just b)) = SymCo <$> unify scope b a

unify scope skt@(TySkol t@(Skolem sv _ _ _)) b = do -- {{{
  sub <- use (solveAssumptions . at sv)
  subst <- use solveAssumptions
  info <- view solveInfo
  case sub of
    Just ty -> do
      let ty' = apply subst ty
      _ <- unify scope b ty'
      pure (AssumedCo (TySkol t) ty')
    Nothing -> case b of
      TyVar v -> bind scope v (TySkol t)
      TyWildcard (Just tau) -> unify scope skt tau
      TyApps (TyCon v) xs | Just (Right tf) <- Map.lookup v info ->
        SymCo <$> unifyTyFunApp tf scope xs skt
      _ ->
        case lookupEquality info scope subst skt b of
          (x:_) -> pure x
          _ -> do
            canWe <- view bindSkol
            if canWe
               then if t `Set.notMember` skols b
               then do
                 solveAssumptions . at sv ?= b
                 pure (AssumedCo (TySkol t) b)
               else confesses (Occurs sv b)
               else confesses (SkolBinding t b)
unify scope b (TySkol t) = SymCo <$> unify scope (TySkol t) b -- }}}

-- ((->) a b) = a -> b
unify scope (TyApp (TyApp (TyCon v) l) r) (TyArr l' r')
  | v == tyArrowName = ArrCo <$> unify scope l l' <*> unify scope r r'

unify scope (TyArr l r) (TyApp (TyApp (TyCon v) l') r')
  | v == tyArrowName = ArrCo <$> unify scope l l' <*> unify scope r r'

unify scope x@(TyApp f g) y@(TyArr l r) =
  rethrow x y $ ArrCo <$> unify scope f (TyApp (TyCon tyArrowName) l) <*> unify scope g r

unify scope x@(TyArr l r) y@(TyApp f g) =
  rethrow x y $ ArrCo <$> unify scope (TyApp (TyCon tyArrowName) l) f <*> unify scope r g

unify scope (TyArr a b) (TyArr a' b') = ArrCo <$> unify scope a a' <*> unify scope b b'
unify scope (TyPi (Implicit a) b) (TyPi (Implicit a') b') =
  ArrCo <$> unify scope a a' <*> unify scope b b' -- Technically cheating but yay desugaring

unify scope ta@(TyCon a) tb@(TyCon b)
  | a == b = pure (ReflCo tb)
  | otherwise = do
      i <- view solveInfo
      x <- use solveAssumptions
      case lookupEquality i scope x ta tb of
        (x:_) -> pure x
        _ -> confesses =<< unequal ta tb

unify scope (TyForall v Nothing ty) (TyForall v' Nothing ty') = do
  fresh <- freshTV
  let (TyVar tv) = fresh

  ForallCo tv (ReflCo TyType) <$>
    unify scope (apply (Map.singleton v fresh) ty) (apply (Map.singleton v' fresh) ty')

unify scope (TyForall v (Just k) ty) (TyForall v' (Just k') ty') = do
  c <- unify scope k k'
  fresh <- freshTV
  let (TyVar tv) = fresh

  ForallCo tv c <$>
    unify scope (apply (Map.singleton v fresh) ty) (apply (Map.singleton v' fresh) ty')

unify scope (TyRows rho arow) (TyRows sigma brow)
  | overlaps <- overlap arow brow
  , rhoNew <- L.deleteFirstsBy ((==) `on` fst) (L.sortOn fst arow) (L.sortOn fst brow)
  , sigmaNew <- L.deleteFirstsBy ((==) `on` fst) (L.sortOn fst brow) (L.sortOn fst arow) =
    do
      let mk t rs = if rs /= [] then TyRows t rs else t
      tau <- freshTV
      cs <- traverse (unifRow scope) overlaps
      if null sigmaNew && null rhoNew
         then RowsCo <$> unify scope rho sigma <*> pure cs
         else do
           co <- unify scope rho (mk tau sigmaNew) -- yes
           _ <- unify scope sigma (mk tau rhoNew) -- it's backwards
           pure (RowsCo co cs)

unify scope ta@TyExactRows{} tb@TyRows{} = SymCo <$> unify scope tb ta

unify scope tb@(TyRows rho brow) ta@(TyExactRows arow)
  | overlaps <- overlap brow arow
  , rhoNew <- L.deleteFirstsBy ((==) `on` fst) (L.sortOn fst arow) (L.sortOn fst brow)
  = if | length overlaps < length brow -> confesses (NoOverlap tb ta)
       | otherwise -> do
          cs <- traverse (unifRow scope) overlaps
          _ <- unify scope rho (TyExactRows rhoNew)
          pure (SymCo (ProjCo rhoNew cs))

unify scope ta@(TyExactRows arow) tb@(TyExactRows brow)
  | overlaps <- overlap arow brow
  = do when (length overlaps /= length arow || length overlaps /= length brow)
         $ confesses (NoOverlap ta tb)
       cs <- traverse (unifRow scope) overlaps
       pure (ExactRowsCo cs)

unify _ x tp@TyRows{} = confesses (Note (CanNotInstance tp x) isRec)
unify _ tp@TyRows{} x = confesses (Note (CanNotInstance tp x) isRec)

-- ((*) a b) = a * b
unify scope (TyApp (TyApp (TyCon v) l) r) (TyTuple l' r')
  | v == tyTupleName = ProdCo <$> unify scope l l' <*> unify scope r r'

unify scope (TyTuple l r) (TyApp (TyApp (TyCon v) l') r')
  | v == tyTupleName = ProdCo <$> unify scope l l' <*> unify scope r r'

unify scope x@(TyApp f g) y@(TyTuple l r) =
  rethrow x y $ ProdCo <$> unify scope f (TyApp (TyCon tyTupleName) l) <*> unify scope g r
unify scope x@(TyTuple l r) y@(TyApp f g) =
  rethrow x y $ ProdCo <$> unify scope (TyApp (TyCon tyTupleName) l) f <*> unify scope r g

unify scope (TyOperator l v r) (TyTuple l' r')
  | v == tyTupleName = ProdCo <$> unify scope l l' <*> unify scope r r'

unify scope (TyTuple l r) (TyOperator l' v r')
  | v == tyTupleName = ProdCo <$> unify scope l l' <*> unify scope r r'

unify scope (TyTuple a b) (TyTuple a' b') =
  ProdCo <$> unify scope a a' <*> unify scope b b'

unify scope x@(TyOperator l v r) y@(TyApp f g) =
  rethrow x y $ AppCo <$> unify scope (TyApp (TyCon v) l) f <*> unify scope g r
unify scope x@(TyApp f g) y@(TyOperator l v r) =
  rethrow x y $ AppCo <$> unify scope f (TyApp (TyCon v) l) <*> unify scope r g

unify scope (TyOperator l v r) (TyOperator l' v' r')
  | v == v' = AppCo <$> unify scope l l' <*> unify scope r r'

unify scope ta@(TyApps (TyCon v) xs@(_:_)) b = do
  x <- view solveInfo
  case x ^. at v of
    Just (Right tf) -> unifyTyFunApp tf scope xs b
    _ -> case b of
      TyApps f ys | length xs == length ys -> rethrow ta b $ do
        heads <- unify scope (TyCon v) f
        tails <- traverse (uncurry (unify scope)) (zip xs ys)
        pure (foldl AppCo heads tails)
      TyApps f ys | length ys < length xs -> rethrow ta b $ do
        let (xs_a, xs_b) = splitAt (length ys) xs
        heads <- unify scope (TyApps (TyCon v) xs_a) f
        tails <- traverse (uncurry (unify scope)) (zip xs_b ys)
        pure (foldl AppCo heads tails)
      _ ->
        (confesses =<< unequal ta b)
          `catchChronicle` \_ -> fmap SymCo (unify scope b ta)

unify scope a tb@(TyApps (TyCon _) (_:_)) = rethrow a tb $ SymCo <$> unify scope tb a

unify scope (TyApp f x) (TyApp g y) = AppCo <$> unify scope f g <*> unify scope x y


unify _ TyType TyType = pure (ReflCo TyType)
unify _ a b = confesses =<< unequal a b -- }}}

unifyTyFunApp, unifyTyFunApp' :: forall m. MonadSolve m
                              => TySymInfo
                              -> ImplicitScope ClassInfo Typed
                              -> [Type Typed]
                              -> Type Typed
                              -> m (Coercion Typed)
tyFunByEval :: forall m. MonadSolve m
            => TySymInfo
            -> ImplicitScope ClassInfo Typed
            -> [Type Typed]
            -> Type Typed
            -> m (Maybe (Coercion Typed))
#ifdef TRACE_TC
unifyTyFunApp ti _ args tb
  | trace (displayS (keyword "[W] TF:"
                 <+> pretty (TyApps (TyCon (ti ^. tsName)) args)
                 <+> soperator (char '~')
                 <+> pretty tb))
      False = undefined
#endif
unifyTyFunApp TySymInfo{} _ _ _ = undefined
unifyTyFunApp ti@(TyFamInfo tn _ _ _)   scope args tb@(TyApps (TyCon tn') args') | tn == tn' = do
  x <- memento $ foldl AppCo (ReflCo (TyCon tn)) <$> traverse (uncurry (unify scope)) (zip args args')
  case x of
    Left _ -> unifyTyFunApp' ti scope args tb
    Right x -> pure x
unifyTyFunApp ti scope args tb = unifyTyFunApp' ti scope args tb

unifyTyFunApp' info scope args tb = do
  x <- tyFunByEval info scope args tb
  solve_info <- view solveInfo
  case x of
    Just t -> pure t
    Nothing -> case tb of
      TyApps (TyCon tn') their@(_:_) | Just (Right info') <- Map.lookup tn' solve_info -> do
        y <- tyFunByEval info' scope their (TyApps (TyCon (info ^. tsName)) args)
        maybe don't pure y
      _ -> don't
  where
    don't = do
      var <- genName
      tell [ConImplicit (It'sThis BecauseInternal) scope var
              (TyApps tyEq [TyApps (TyCon (info ^. tsName)) args, tb])]
      pure (VarCo var)

tyFunByEval (TyFamInfo tn _ _ _) scope args tb | traceShow args True, Just solve <- magicTyFun tn = solve scope args tb
tyFunByEval (TyFamInfo tn eqs _ _) scope args tb = do
  info <- view solveInfo
  assum <- use solveAssumptions
  case lookupEquality info scope assum (TyApps (TyCon tn) args) tb of
    (x:_) -> pure (Just x)
    _ -> go [] eqs

  where
    go skipped ((declared', result', con):eqs) = do
      info <- view solveInfo
      assum <- use solveAssumptions

      let vars = Set.toList (ftv declared')
      fresh <- fmap Map.fromList . for vars $ \v -> (v,) <$> freshTV
      let (declared, result) = (apply fresh declared', apply fresh result')

      x <- ack (zip declared args)

      case x of
        Just (sub, cos) -> do
          traceM (show (pretty (TyApps (TyCon tn) declared)))

          flat <- flatten assum info (TyApps (TyCon tn) (apply sub declared))
          traceM (show (pretty flat))

          if all (apart flat) skipped

             then do
               traceM (displayS (keyword "[D]:" <+> pretty (apply sub result) <+> "~" <+> pretty tb))
               _ <- unify scope (apply sub result) tb
               pure (Just (InstCo con cos))

             else pure Nothing

        _ -> go (TyApps (TyCon tn) declared:skipped) eqs
    go _ [] = pure Nothing

    apart = (isNothing .) . unifyPure

    ack :: [(Type Typed, Type Typed)] -> m (Maybe (Subst Typed, [Coercion Typed]))
    ack ts = do
      (x, state) <- capture $ memento $ traverse (uncurry (unify scope)) ts
      case x of
        Left _ -> pure Nothing
        Right x -> pure (pure (state ^. solveTySubst, x))

tyFunByEval _ _ _ _ = undefined

lookupEquality :: SolverInfo
               -> ImplicitScope ClassInfo Typed
               -> Map.Map (Var Typed) (Type Typed)
               -> Type Typed
               -> Type Typed
               -> [Coercion Typed] -- {{{
lookupEquality class_info scope assum a b = normal <|> fundepEquality where
  -- Try instances a ~ b (which must be LocalAssum).
  normal =
    let choices = filter ((/= Superclass) . view implSort) $
          find (TyApps tyEq [a, b]) scope <> find (TyApps tyEq [b, a]) scope
     in assert (all ((== LocalAssum) . view implSort) choices) (map makeCo choices)

  makeCo ImplChoice{..} =
    case _implClass of
      Proj1 -> P1 _implVar
      Proj2 -> P2 _implVar
      Var   -> VarCo _implVar

  find t scope = used Var   (find_ffs t scope)
             <|> used Proj1 (find_ffs (TyTuple t a) scope)
             <|> used Proj2 (find_ffs (TyTuple a t) scope)
    where a = TyVar (TgInternal "a")
          a :: Type Typed
          used x = map (implClass .~ x)

  find_ffs t = map snd . filter (matches tau . view implHead . snd) . concatMap splat . Map.toList . keys where
    tau = transformType go t
    splat (x, t) = map (x,) t
    go (TySkol v) | Just x <- assum ^. at (v ^. skolIdent) = x
    go t = t

  fundepEquality =
    let
      all_instances = Map.keys (keys scope)

      inst_pairs =
            [ (x, y) | x@(TyApps (TyCon c) _) <- all_instances
                     , y@(TyApps (TyCon c') _) <- all_instances, c == c', x /= y ]
        <|> [ (x, y) | TyTuple x y <- all_instances
                     , let TyApps (TyCon c) _ = x, let TyApps (TyCon c') _ = y, c == c' ]

      pair_fds =
        [ (xs, tail (appsView y), need, det)
        | (x, y) <- inst_pairs
        , let TyApps (TyCon class_tc) xs = x
        , let Just (Left clss) = class_info ^. at class_tc
        , (need, det, _) <- clss ^. ciFundep
        ]

      implied_eqs =
        [ AssumedCo (ta !! det) (tb !! det)
        -- TODO: We shouldn't blindly assume equalities here, even
        -- though Lint will be happy with that. The principled thing to
        -- do is have each fundep add a coercion axiom with type:
        --   forall 'a 'b 'c 'd. 'a ~ 'b -> c 'a 'c -> c 'a 'd -> 'c ~ 'd
        -- then we use that here.
        | (ta, tb, needs, dets) <- pair_fds
        , flip all needs $ \idx ->
               prettyConcrete (ta !! idx)
            && prettyConcrete (tb !! idx)
            && ta !! idx == tb !! idx
        , det <- dets
        ]
     in implied_eqs
-- }}}

subsumes', subsumes :: MonadSolve m
                    => SomeReason -> ImplicitScope ClassInfo Typed
                    -> Type Typed -> Type Typed -> m (Wrapper Typed) -- {{{
subsumes blame scope a b = do
  x <- use solveTySubst
  retcons (addBlame blame) $ subsumes' blame scope (apply x a) (apply x b)

#ifdef TRACE_TC
subsumes' _ _ t1 t2
  | trace (displayS (hsep [displayType t1, soperator (char '≤'), displayType t2])) False
  = undefined
#endif

subsumes' b s t1 t2@TyPi{} | isSkolemisable t2 = do
  (c, t2', scope, _) <- skolemise (BySubsumption t1 t2) t2
  (Syntax.:>) c <$> subsumes b (scope <> s) t1 t2'

subsumes' r s (TyPi (Implicit t) t1) t2 -- {{{
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
       in pure (WrapFn (MkWrapCont wrap ("implicit instantation " ++ show (pretty t)))) -- }}}

  | otherwise = probablyCast <$> unify s (TyPi (Implicit t) t1) t2

subsumes' b s t1@TyPi{} t2 | isInstantiatable t1 = do
  (cont, _, t1') <- instantiate Strong Subsumption t1
  omega <- subsumes b s t1' t2
  let inst = fromMaybe id cont
      wrap ex = ExprWrapper omega (inst ex) (annotation ex, t2)
  pure (WrapFn (MkWrapCont wrap ("instantiation " ++ show t1 ++ " => " ++ show t2)))

subsumes' r scope ot@(TyTuple a b) nt@(TyTuple a' b') = do -- {{{
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
  pure (WrapFn (MkWrapCont cont "tuple re-packing")) -- }}}

subsumes' _ s a@(TyApp lazy _) b@(TyApp lazy' _)
  | lazy == lazy', lazy' == tyLazy = probablyCast <$> unify s a b

subsumes' r scope (TyApp lazy ty') ty | lazy == tyLazy, lazySubOk ty' ty = do
  co <- subsumes' r scope ty' ty
  -- We have a thunk and want a value
  let wrap ex
        | an <- annotation ex =
          -- ... so force it
          ExprWrapper co
            (App (ExprWrapper (TypeApp ty') (VarRef forceName (an, forceTy)) (an, forceTy' ty'))
              ex
              (an, ty'))
            (an, ty)
  pure (WrapFn (MkWrapCont wrap "automatic forcing"))

subsumes' r scope ty' (TyApp lazy ty) | lazy == tyLazy, lazySubOk ty ty' = do
  wp <- subsumes' r scope ty ty'
  -- We have a value and want a thunk
  let wrap ex
        | an <- annotation ex =
          App (ExprWrapper (TypeApp ty) (VarRef lAZYName (an, lAZYTy))
                (an, lAZYTy' ty))
              -- So put it in a function to delay evaluation!
              (Fun (PatParam (PLiteral LiUnit (an, tyUnit)))
                (ExprWrapper wp ex (an, ty'))
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

subsumes' r scope th@(TyExactRows rhas) tw@(TyRows rho rwant) = do -- {{{
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
  _ <- unify scope rho (TyExactRows diff)
  let cast = probablyCast co
  exp <- genName

  let mkw ex = ExprWrapper cast ex (annotation ex, tw)
  pure (WrapFn
          (MkWrapCont (mkRecordWrapper rhas matched matched_t th tw mkw exp)
            "exact→poly record subsumption")) -- }}}

subsumes' r scope th@(TyRows rho rhas) tw@(TyRows sigma rwant) = do -- {{{
  let matching = overlap rhas rwant

  -- We need to at *least* match all of the ones we want
  if length matching < length rwant || length rwant > length rhas
  then do
    _ <- unify scope th tw
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

    cast <- probablyCast <$> unify scope sigma new
    exp <- genName

    let mkw ex = ExprWrapper cast ex (annotation ex, tw)
    pure (WrapFn
            (MkWrapCont (mkRecordWrapper rhas matched matched_t th tw mkw exp)
              "exact→poly record subsumption"))

subsumes' r s a b = probablyCast <$> retcons (reblame r) (unify s a b) -- }}}

-- }}}

-- | Shallowly skolemise a type, replacing any @forall@-bound 'TyVar's
-- with fresh 'Skolem' constants.
skolemise :: MonadNamey m
          => SkolemMotive Typed
          -> Type Typed
          -> m (Wrapper Typed, Type Typed, ImplicitScope ClassInfo Typed, [(Var Typed, Type Typed)])
skolemise motive ty@(TyPi (Invisible tv k _) t) = do
  sk <- freshSkol motive ty tv
  (wrap, ty, scope, vs) <- skolemise motive (apply (Map.singleton tv sk) t)
  kind <- maybe freshTV pure k
  let getSkol (TySkol s) = s
      getSkol _ = error "not a skolem from freshSkol"
  pure (TypeLam (getSkol sk) kind Syntax.:> wrap, ty, scope, (tv, sk):vs)

skolemise motive wt@(TyPi (Implicit ity) t) = do
  (omega, ty, scp, vs) <- skolemise motive t
  let go (TyTuple a b) = do
        var <- genName
        (pat, scope) <- go b
        pure (Capture var (internal, a):pat, insert internal LocalAssum var a (MagicInfo []) scope)
      go x = do
        var <- genName
        pure ([Capture var (internal, x)], insert internal LocalAssum var x (MagicInfo []) scp)
  (pat, scope) <- go ity
  let wrap ex | an <- annotation ex =
        Fun (EvParam (PTuple pat (an, ity)))
          (ExprWrapper omega ex (an, ty)) (an, wt)
  pure (WrapFn (MkWrapCont wrap "constraint lambda"), ty, scope, vs)

skolemise _ ty = pure (IdWrap, ty, mempty, [])

flatten :: forall m. MonadNamey m => Subst Typed -> SolverInfo -> Type Typed -> m (Type Typed)
flatten assum i = go 0 where
  go :: Int -> Type Typed -> m (Type Typed)
  go l (TyApps (TyCon v) as@(_:_))
    | l > 0, Just (Right _) <- i ^. at v = freshTV
    | otherwise = TyApps (TyCon v) <$> traverse (go (l + 1)) as

  go l (TyApp f x) = TyApp <$> go (l + 1) f <*> go (l + 1) x
  go l (TySkol v)
    | Just x <- assum ^. at (v ^. skolIdent) = go (l + 1) x
    | otherwise = freshTV
  go _ x@TyCon{} = pure x
  go _ x@TyVar{} = pure x
  go _ x@TyPromotedCon{} = pure x
  go _ x@TyLit{} = pure x
  go _ TyWithConstraints{} = error "flatten TyWithConstraints"
  go _ TyType = pure TyType

  go l (TyPi c t) = TyPi <$> go_b c <*> go (l + 1) t where
    go_b (Anon c) = Anon <$> go (l + 1) c
    go_b (Implicit c) = Implicit <$> go (l + 1) c
    go_b (Invisible v k r) = Invisible v <$> traverse (go (l + 1)) k <*> pure r

  go l (TyTuple a b) = TyTuple <$> go (l + 1) a <*> go (l + 1) b
  go l (TyRows t rs) = TyRows <$> go (l + 1) t <*> traverse (_2 %%~ go (l + 1)) rs
  go l (TyExactRows rs) = TyExactRows <$> traverse (_2 %%~ go (l + 1)) rs
  go x (TyOperator l o r) = TyOperator <$> go (x + 1) l <*> pure o <*> go (x + 1) r
  go l (TyWildcard v) = TyWildcard <$> traverse (go (l + 1)) v
  go l (TyParens t) = TyParens <$> go (l + 1) t

-- Which coercions are safe to remove *here*?

applicable :: Type Typed -> ImplicitScope ClassInfo Typed -> Implicit ClassInfo Typed -> Bool
applicable wanted scp (ImplChoice head _ cs _ s _ _) =
  case s of
    Superclass -> isJust (unifyPure wanted head) && all (entails scp) cs
    _ -> isJust (unifyPure wanted head)
  where
    entails :: ImplicitScope ClassInfo Typed -> Obligation Typed -> Bool
    entails _ (Quantifier (Invisible v _ _)) = v `Set.notMember` ftv head || v `Map.member` sub
    entails scp (Implication c) | c <- apply sub c =
      any (applicable c scp) (lookup c scp)
    entails _ _ = False

    sub :: Subst Typed
    Just sub = unifyPure wanted head


probablyCast :: Coercion Typed -> Wrapper Typed
probablyCast = Cast

rethrow :: MonadSolve m => Type Typed -> Type Typed -> m a -> m a
rethrow l r cont =
  condemn cont
    `catchChronicle` \e -> confess =<< for e go
  where
    go NotEqual{} = unequal l r
    go x = pure x

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
concreteUnderOne t = all prettyConcrete (appsView t)

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
        Let [Binding exp ex True (an, th)]
          (cont (recordExt (ref an) (foldMap (updateField an (ref an)) keys) (an, matched_t)))
          (an, tw)
   in wrapEx

lazySubOk :: Type Typed -> Type Typed -> Bool
lazySubOk tlazy tout =
     concretish tout
  || head (appsView tout) == head (appsView tlazy)
  || (record tout && record tlazy)
  where
    record TyRows{} = True
    record TyExactRows{} = True
    record _ = False

overlap :: [(Text, Type p)] -> [(Text, Type p)] -> [(Text, Type p, Type p)]
overlap xs ys
  | inter <- filter ((/=) 1 . length) $ L.groupBy ((==) `on` fst) (L.sortOn fst (xs ++ ys))
  = map get inter
  where get [(t, a), (_, b)] = (t, a, b)
        get _ = undefined

countConstructors :: Type Typed -> Down (Sum Int)
countConstructors TyCon{} = 1
countConstructors TyVar{} = 0
countConstructors (TyApp f x) = countConstructors f + countConstructors x
countConstructors (TyPi (Anon l) r) = 1 + countConstructors l + countConstructors r
countConstructors (TyPi (Implicit l) r) = countConstructors l + countConstructors r
countConstructors (TyPi (Invisible _ k _) r) = foldMap countConstructors k + countConstructors r
countConstructors (TyTuple l r) = 1 + countConstructors l + countConstructors r
countConstructors (TyOperator l _ r) = 1 + countConstructors l + countConstructors r
countConstructors TySkol{} = 1
countConstructors TyType{} = 1
countConstructors TyWildcard{} = 0
countConstructors TyPromotedCon{} = 1
countConstructors (TyParens t) = countConstructors t
countConstructors (TyRows r rs) = countConstructors r + foldMap (countConstructors . snd) rs
countConstructors (TyExactRows rs) = foldMap (countConstructors . snd) rs
countConstructors _ = 0

guardClassOverflow :: MonadSolve m => SomeReason -> Type Typed -> m ()
guardClassOverflow why cons = do
  x <- view depth
  traceM (displayS ("class stack:" <+> vsep (map pretty x)))
  when (length x >= 50) $
    confesses (ClassStackOverflow why x cons)

reblame_con :: SomeReason -> [Constraint p] -> [Constraint p]
reblame_con r = map go where
  go (ConUnify _ a b c d) = ConUnify r a b c d
  go (ConSubsume _ a b c d) = ConSubsume r a b c d
  go (ConImplies _ a b c) = ConImplies r a b c
  go (ConImplicit _ a b c) = ConImplicit r a b c
  go x@ConFail{} = x
  go x@DeferredError{} = x

newtype InField = InField Text
  deriving Show

instance Pretty InField where
  pretty (InField t) = string "When checking the field" <+> skeyword (text t)

data UsedEq = Var | Proj1 | Proj2 deriving (Eq, Show, Ord)

traceM :: Applicative m => String -> m ()
trace :: String -> a -> a
traceShow :: Show a => a -> b -> b
tracePrettyId :: Pretty a => a -> a

#ifdef TRACE_TC

traceM = Dbg.traceM
trace = Dbg.trace
traceShow = Dbg.traceShow
tracePrettyId x = Dbg.trace (displayS (pretty x)) x

#else

traceM _ = pure ()
trace _ x = x
traceShow a x = show a `seq` x
tracePrettyId x = pretty x `seq` x

#endif

-- vim: fdm=marker
