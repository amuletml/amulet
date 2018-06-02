{-# LANGUAGE MultiWayIf, GADTs, FlexibleContexts, ScopedTypeVariables, TemplateHaskell #-}
{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Types.Unify (solve, overlap, bind, skolemise, freshSkol) where

import Control.Monad.Except
import Control.Monad.State
import Control.Applicative
import Control.Monad.Infer
import Control.Lens hiding (Empty)

import Types.Infer.Builtin hiding (subsumes, unify)
import Types.Infer.Errors
import Types.Wellformed

import Syntax.Subst
import Syntax

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Sequence (Seq ((:<|), Empty))
import Data.Spanned
import Data.Foldable
import Data.Function
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
               }
  deriving (Eq, Show, Ord)

makeLenses ''SolveState
makeLenses ''SolveScope

type SolveM = GenT Int (WriterT [TypeError] (StateT SolveState (ReaderT SolveScope (Except TypeError))))

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
           then do
             solveAssumptions . at sv ?= b
             pure (AssumedCo (TySkol t) b)
           else throwError $ SkolBinding t b
unify b (TySkol t) = SymCo <$> unify (TySkol t) b

unify (TyVar a) b = bind a b
unify a (TyVar b) = SymCo <$> bind b a

unify (TyArr a b) (TyArr a' b') = ArrCo <$> unify a a' <*> unify b b'
unify (TyApp a b) (TyApp a' b') = AppCo <$> unify a a' <*> unify b b'

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

unify (TyPi (Explicit v k) ty) (TyPi (Explicit v' k') ty') = do
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
      tau <- freshTV
      cs <- traverse unifRow overlaps
      co <- unify rho (TyRows tau sigmaNew) -- yes
      _ <- unify sigma (TyRows tau rhoNew) -- it's backwards
      pure (RowsCo co cs)

unify ta@TyExactRows{} tb@TyRows{} = unify tb ta

unify tb@(TyRows rho brow) ta@(TyExactRows arow)
  | overlaps <- overlap arow brow
  , rhoNew <- deleteFirstsBy ((==) `on` fst) (sortOn fst arow) (sortOn fst brow)
  = case overlaps of
      [] -> throwError (NoOverlap tb ta)
      xs -> do
        cs <- traverse unifRow xs
        _ <- unify rho (TyExactRows rhoNew)
        pure (ProjCo rhoNew cs)

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

overlap :: Typed ~ p => [(Text, Type p)] -> [(Text, Type p)] -> [(Text, Type p, Type p)]
overlap xs ys
  | inter <- filter ((/=) 1 . length) $ groupBy ((==) `on` fst) (sortOn fst (xs ++ ys))
  = map get inter
  where get [(t, a), (_, b)] = (t, a, b)
        get _ = undefined

unifRow :: (Text, Type Typed, Type Typed) -> SolveM (Text, Coercion Typed)
unifRow (t, a, b) = do
  co <- unify a b
  pure (t, co)

runSolve :: Int -> Subst Typed -> SolveM b -> Either TypeError (Int, (Subst Typed, Map.Map (Var Typed) (Wrapper Typed)))
runSolve i s x = runExcept (fix (runReaderT (runStateT (runWriterT (runGenTFrom i act)) (SolveState s mempty mempty)) emptyScope)) where
  act = (,) <$> gen <*> x
  fix act = do
    (((i, _), w), s) <- act
    case w of
      [] -> let ss = s ^. solveTySubst
             in pure (i, (fmap (apply ss) ss, s ^. solveCoSubst))
      xs -> throwError (ManyErrors xs)
  emptyScope = SolveScope False mempty

solve :: Int -> Seq.Seq (Constraint Typed) -> Either TypeError (Subst Typed, Map.Map (Var Typed) (Wrapper Typed))
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
doSolve (ConSubsume because v a b :<| xs) = do
  sub <- use solveTySubst


  co <- catchy $ subsumes unify (apply sub a) (apply sub b)
  case co of
    Left e -> tell [propagateBlame because e]
    Right co -> solveCoSubst . at v .= Just co

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
doSolve (ConFail a v t :<| cs) = do
  doSolve cs
  sub <- use solveTySubst
  let ex = Hole (unTvName v) (fst a)
  tell [propagateBlame (BecauseOf ex) $ foundHole v (apply sub t) sub]

subsumes :: (Type Typed -> Type Typed -> SolveM (Coercion Typed))
         -> Type Typed -> Type Typed -> SolveM (Wrapper Typed)
subsumes k t1 t2@TyPi{} | isSkolemisable t2 = do
  sub <- use solveTySubst
  (c, t2') <- skolemise (BySubsumption (apply sub t1) (apply sub t2)) t2
  (Syntax.:>) c <$> subsumes k t1 t2'
subsumes k t1@TyPi{} t2 | isSkolemisable t1 = do
  (cont, _, t1') <- instantiate Subsumption t1
  let wrap = maybe IdWrap (WrapFn . flip MkWrapCont "forall <= sigma; instantiation") cont

  flip (Syntax.:>) wrap <$> subsumes k t1' t2

subsumes k ot@(TyTuple a b) nt@(TyTuple a' b') = do
  wa <- subsumes k a a'
  wb <- subsumes k b b'
  [elem, elem'] <- fmap TvName <$> replicateM 2 fresh
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

subsumes k a@(TyApp lazy _) b@(TyApp lazy' _)
  | lazy == lazy', lazy' == tyLazy = probablyCast <$> k a b

subsumes k (TyApp lazy ty') ty | lazy == tyLazy, _TyVar `isn't` ty = do
  co <- k ty' ty
  let wrap ex
        | an <- annotation ex =
          App (ExprWrapper (TypeApp ty) (VarRef forceName (an, forceTy))
                (an, forceTy' ty))
              (ExprWrapper (probablyCast (AppCo (ReflCo tyLazy) co)) ex (an, TyApp lazy ty))
              (an, ty)
  pure (WrapFn (MkWrapCont wrap "automatic forcing"))

subsumes k ty' (TyApp lazy ty) | lazy == tyLazy, _TyVar `isn't` ty' = do
  co <- k ty ty'
  let wrap ex
        | an <- annotation ex =
          App (ExprWrapper (TypeApp ty) (VarRef lAZYName (an, lAZYTy))
                (an, lAZYTy' ty))
              (Fun (PLiteral LiUnit (an, tyUnit))
                (ExprWrapper (probablyCast co) ex (an, ty'))
                (an, TyArr tyUnit ty))
              (an, TyApp lazy ty)
  pure (WrapFn (MkWrapCont wrap "automatic thunking"))


subsumes k a b = probablyCast <$> k a b where


skolemise :: MonadGen Int m => SkolemMotive Typed -> Type Typed -> m (Wrapper Typed, Type Typed)
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
isSkolemisableTyBinder (TyPi (Implicit v k) c) = Just (v, k, c)
isSkolemisableTyBinder (TyPi (Explicit v k) c) = Just (v, Just k, c)
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

freshSkol :: MonadGen Int m => SkolemMotive Typed -> Type Typed -> Var Typed -> m (Type Typed)
freshSkol m ty u = do
  var <- TvName <$> fresh
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

catchy :: MonadError e m => m a -> m (Either e a)
catchy x = (Right <$> x) `catchError` (pure . Left)
