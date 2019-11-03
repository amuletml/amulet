{-# LANGUAGE MultiWayIf, FlexibleContexts, ScopedTypeVariables,
   ViewPatterns, ConstraintKinds, CPP, TypeFamilies, OverloadedStrings,
   DisambiguateRecordFields, TypeApplications #-}

-- | This module implements the logic responsible for solving the
-- sequence of @Constraint@s the type-checker generates for a particular
-- binding groups.
module Types.Unify
  ( SolveState, emptyState
  , typeWithin
  , solve, solveImplies
  , skolemise, freshSkol, skolFreeTy
  , unifyPure_v, unifyPure
  , applicable, getSolveInfo
  , prettyConcrete
  , removeTypeFamApps
  ) where

import Control.Monad.Except
import Control.Applicative
import Control.Monad.Infer
import Control.Lens hiding (Empty, (:>))

import Types.Infer.Errors
import Types.Wellformed

import Syntax.Implicits hiding (overlap)
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
import Data.Either
import Data.Maybe
import Data.Span (internal)
import Data.Text (Text)
import Data.Ord

import Text.Pretty.Semantic

import Types.Unify.Equality
import Types.Unify.Trace
import Types.Unify.Magic

import Prelude hiding (lookup)

import Types.Unify.Base

typeWithin :: Var Typed -> SolveState -> Maybe (Type Typed)
typeWithin v s = s ^. (solveTySubst . at v) <|> s ^. (solveAssumptions . at v)

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
solve cs info = solveWith cs info emptyState

solveWith :: (MonadNamey m, MonadChronicles TypeError m)
      => Seq.Seq (Constraint Typed)
      -> SolverInfo
      -> SolveState
      -> m (Subst Typed, Map.Map (Var Typed) (Wrapper Typed), [Constraint Typed])
solveWith cs info state = do
  (cs', s) <- runSolve False info state (doSolve cs)
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

  traceM TopS (keyword "[W]:" <+> pretty (ConUnify because scope v (apply sub a) (apply sub b)))

  solveFuel .= emptyFuel

  co <- censor (reblame_con because) $ memento $ unify scope (apply sub a) (apply sub b)

  case co of
    Left e -> do
      dictate (reblame because <$> e)
      solveCoSubst . at v ?= IdWrap
    Right co -> solveCoSubst . at v ?= probablyCast co

  doSolve xs

doSolve (ConSubsume because scope v a b :<| xs) = do
  sub <- use solveTySubst

  traceM TopS (keyword "[W]:" <+> pretty (ConSubsume because scope v (apply sub a) (apply sub b)))
  let a' = apply sub a
  sub <- use solveTySubst

  solveFuel .= emptyFuel
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

  traceM TopS (displayS (pretty (ConImplies because not cs' ts')))

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
  traceM TopS ("quantified constraint. Solving:" <+> pretty head)
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
  let scope' = apply sub scope
      old = apply sub cons
  cons <- reduceTyFuns scope' (apply sub cons)

  traceM TopS (keyword "[W]:" <+> pretty (apply sub ohno))

  let possible = lookup cons scope'
  traceM TopS ("considering between:" <#> vsep (map (indent 2 . pretty . view implType) possible))

  ignored <- freshTV
  isMagic <- isMagicClass cons

  case possible of
    xs | allSameHead xs
       , concreteUnderOne cons || hasExactMatch cons xs
       , applic <- filter (applicable cons scope') xs
       , not (null applic)
       , let imp = pickBestPossible applic
       , trace TopS (shown (imp ^. implVar, isSuper imp)) True
       , not isMagic || not (isSuper imp) 
       -> do

      traceM TopS ("best possible:" <+> pretty var <+> pretty (imp ^. implVar) <+> pretty (imp ^. implType))

      w <- local (depth %~ (cons :)) $
        useImplicit reason cons old scope' imp
          `catchChronicle`
            \e -> confesses (usingFor imp cons (headSeq e))
      solveCoSubst . at var ?= w

    -- TODO: see if sound
    (filter ((/= Superclass) . view implSort) -> [imp])
      | not (concreteUnderOne cons), imp ^. implSort == LocalAssum || fundepsAllow imp cons -> do

      traceM TopS ("only possible:" <+> pretty var <+> pretty (imp ^. implVar) <+> pretty (imp ^. implType))

      w <- useImplicit reason cons old scope' imp
          `catchChronicle`
            \e -> confesses (usingFor imp cons (headSeq e))
      solveCoSubst . at var ?= w

    _ | let tup = TyTuple cons ignored, [imp] <- lookup tup scope' -> do

      traceM TopS ("decomposing tuple:" <+> pretty (imp ^. implType))
      w <- useImplicit reason tup (TyTuple old ignored) scope' imp
            `catchChronicle` \e -> confesses (usingFor imp cons (headSeq e))
      v <- genName

      let pi1 = ExprWrapper w
                  (Fun (PatParam (PTuple [Capture v (an, cons), Wildcard (an, ignored)]
                                    (an, tup)))
                    (VarRef v (an, cons)) (an, tup :-> cons))
                  (an, cons)
          an = annotation reason
      solveCoSubst . at var ?= ExprApp pi1

    _ | let tup = TyTuple ignored cons, [imp] <- lookup tup scope' -> do

      traceM TopS ("decomposing tuple:" <+> pretty (imp ^. implType))
      w <- useImplicit reason tup (TyTuple ignored old) scope' imp
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
        TyCon v | Just (solve, report) <- magicClass v -> do
          (w, cs) <- censor (const mempty) $ listen $ solve reason scope' old
          doSolve (Seq.fromList cs) `catchChronicle` report reason scope' old
          case w of
            Just solution -> solveCoSubst . at var ?= solution
            Nothing -> do
              traceM TopS (string " => quantifiying over magic class")
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
pickBestPossible xs | Just i <- find ((== internal) . view implSpan) xs = i
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
            => SomeReason
            -> Type Typed
            -> Type Typed
            -> ImplicitScope ClassInfo Typed
            -> Implicit ClassInfo Typed -> m (Wrapper Typed)
useImplicit reason ty old_ty scope (ImplChoice _ oty _ imp _ _ _) = go where
  go :: m (Wrapper Typed)
  go = do
    guardClassOverflow reason ty

    w <- subsumes' reason scope oty ty
    sub <- use solveTySubst
    (w', _) <- capture $ unify scope ty (apply sub old_ty)
    let start = VarRef imp (annotation reason, oty)

    traceM TopS (string "\x1b[41museImplicit cast:\x1b[0m " <+> pretty w')

    pure . ExprApp . tracePrettyId TopS $
      ExprWrapper (Cast w')
        (Ascription (ExprWrapper w start (annotation reason, ty)) ty (annotation reason, ty))
            (annotation reason, old_ty)

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

subsumes', subsumes :: MonadSolve m
                    => SomeReason -> ImplicitScope ClassInfo Typed
                    -> Type Typed -> Type Typed -> m (Wrapper Typed) -- {{{
subsumes blame scope a b = do
  x <- use solveTySubst
  retcons (addBlame blame) $ subsumes' blame scope (apply x a) (apply x b)

subsumes' _ _ t1 t2
  | trace SubS (hsep [displayType t1, soperator (char '≤'), displayType t2]) False
  = undefined

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
          Let NonRecursive
            [ TypedMatching
                ( PTuple [ Capture elem (an, a), Capture elem' (an, b) ] (an, ot) )
                ex (an, ot)
                [ (elem, a), (elem', b) ] ]
            ( Tuple [ ExprWrapper wa (VarRef elem (an, a)) (an, a')
                    , ExprWrapper wb (VarRef elem' (an, b)) (an, b') ]
              (an, nt))
            (an, nt)
  pure (WrapFn (MkWrapCont cont "tuple re-packing")) -- }}}

subsumes' _ s a@(TyApp lazy _) b@(TyApp lazy' _)
  | lazy == lazy', lazy' == tyLazy = probablyCast <$> polyInstSafe (unify s a b)

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
        pure (Capture var (internal, a):pat, insert internal LocalAssum var a (MagicInfo [] Nothing) scope)
      go x = do
        var <- genName
        pure ([Capture var (internal, x)], insert internal LocalAssum var x (MagicInfo [] Nothing) scp)

  (pat, scope) <- go ity
  traceM TopS (pretty ity)

  let wrap ex | an <- annotation ex =
        Fun (EvParam (PTuple pat (an, ity)))
          (ExprWrapper omega ex (an, ty)) (an, wt)
  pure (WrapFn (MkWrapCont wrap "constraint lambda"), ty, scope, vs)

skolemise _ ty = pure (IdWrap, ty, mempty, [])

skolFreeTy :: MonadNamey m => Set.Set (Var Typed) -> SkolemMotive Typed -> Type Typed -> m (Type Typed, Subst Typed)
skolFreeTy exclude motive ty = do
  vs <- for (Set.toList (ftv ty Set.\\ exclude)) $ \v ->
    (,) v <$> freshSkol motive ty v
  pure (apply (Map.fromList vs) ty, Map.fromList vs)

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
        Let NonRecursive [Binding exp ex True (an, th)]
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
  traceM TopS ("class stack:" <+> vsep (map pretty x))
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

isMagicClass :: forall m. MonadSolve m => Type Typed -> m Bool
isMagicClass (TyApps (TyCon v) _) =
  case magicClass @m v of
    Just{} -> pure True
    _ -> pure False

isMagicClass _ = pure False

isSuper :: Implicit info p -> Bool
isSuper i = (i ^. implSort) == Superclass || any isImplic (i ^. implPre) where
  isImplic Implication{} = True
  isImplic _ = False

-- vim: fdm=marker
