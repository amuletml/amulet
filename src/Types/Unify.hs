{-# Language MultiWayIf, GADTs, FlexibleContexts, ScopedTypeVariables, TemplateHaskell #-}
module Types.Unify (solve, overlap, bind, skolemise, freshSkol) where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Infer
import Control.Lens hiding (Empty)

import Types.Wellformed

import Syntax.Transform
import Syntax.Subst
import Syntax

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Sequence (Seq ((:<|), Empty))
import Data.Foldable
import Data.Function
import Data.List
import Data.Text (Text)

data SolveScope = SolveScope { _bindSkol :: Bool, _don'tTouch :: Set.Set (Var Typed) } deriving (Eq, Show, Ord)
data SolveState = SolveState { _solveTySubst :: Subst Typed, _solveCoSubst :: Map.Map (Var Typed) (Coercion Typed) } deriving (Eq, Show, Ord)
makeLenses ''SolveState
makeLenses ''SolveScope

type SolveM = GenT Int (StateT SolveState (ReaderT SolveScope (Except TypeError)))

bind :: Var Typed -> Type Typed -> SolveM ()
bind var ty
  | occurs var ty = throwError (Occurs var ty)
  | TyVar var == ty = pure ()
  | TyForall{} <- ty = throwError (Impredicative var ty)
  | otherwise = do
      env <- use solveTySubst
      noTouch <- view don'tTouch
      -- Attempt to extend the environment, otherwise unify with existing type
      case Map.lookup var env of
        Nothing ->
          if | var `Set.notMember` noTouch -> solveTySubst .= (env `compose` Map.singleton var (normType ty))
             | var `Set.member` noTouch, TyVar v <- ty, v `Set.notMember` noTouch -> solveTySubst .= (env `compose` Map.singleton v (TyVar var))
             | otherwise -> throwError (NotEqual (TyVar var) ty)
        Just ty'
          | ty' == ty -> pure ()
          | otherwise -> unify (normType ty) (normType ty')
      sub <- use solveTySubst
      solveCoSubst %= fmap (apply sub)

unify :: Type Typed -> Type Typed -> SolveM ()
unify (TySkol x) (TySkol y)
  | x == y = pure ()
unify (TySkol t@(Skolem sv _ _ _)) b = do
  sub <- use (solveTySubst . at sv)
  case sub of
    Just t -> unify t b
    Nothing -> case b of
      TyVar v -> bind v (TySkol t)
      _ -> do
        canWe <- view bindSkol
        if canWe
           then bind sv b
           else throwError $ SkolBinding t b
unify b (TySkol t) = unify (TySkol t) b

unify (TyVar a) b = bind a b
unify a (TyVar b) = bind b a


unify (TyWithConstraints cs a) t = traverse_ (uncurry unify) cs *> unify a t

unify t x@TyWithConstraints{} = unify x t

unify (TyArr a b) (TyArr a' b') = unify a a' *> unify b b'
unify (TyApp a b) (TyApp a' b') = unify a a' *> unify b b'
unify ta@(TyCon a) tb@(TyCon b)
  | a == b = pure ()
  | otherwise = throwError (NotEqual ta tb)
unify t@(TyForall vs ty) t'@(TyForall vs' ty')
  | length vs /= length vs' = throwError (NotEqual t t')
  | otherwise = do
    fvs <- replicateM (length vs) freshTV
    let subst = Map.fromList . flip zip fvs
    unify (apply (subst vs) ty) (apply (subst vs') ty')
unify (TyRows rho arow) (TyRows sigma brow)
  | overlaps <- overlap arow brow
  , rhoNew <- deleteFirstsBy ((==) `on` fst) (sortOn fst arow) (sortOn fst brow)
  , sigmaNew <- deleteFirstsBy ((==) `on` fst) (sortOn fst brow) (sortOn fst arow) =
    do
      tau <- freshTV
      traverse_ (uncurry unify) overlaps
      unify rho (TyRows tau sigmaNew) -- yes
      unify sigma (TyRows tau rhoNew) -- it's backwards
      pure ()
unify ta@TyExactRows{} tb@TyRows{} = unify tb ta
unify tb@(TyRows rho brow) ta@(TyExactRows arow)
  | overlaps <- overlap arow brow
  , rhoNew <- deleteFirstsBy ((==) `on` fst) (sortOn fst arow) (sortOn fst brow)
  = case overlaps of
      [] -> throwError (NoOverlap tb ta)
      xs -> do
        traverse_ (uncurry unify) xs
        unify rho (TyExactRows rhoNew)
unify ta@(TyExactRows arow) tb@(TyExactRows brow)
  | overlaps <- overlap arow brow
  = do when (length overlaps /= length arow || length overlaps /= length brow)
         $ throwError (NoOverlap ta tb)
       traverse_ (uncurry unify) overlaps

unify x tp@TyRows{} = throwError (Note (CanNotInstance tp x) isRec)
unify tp@TyRows{} x = throwError (Note (CanNotInstance tp x) isRec)
unify (TyTuple a b) (TyTuple a' b') = do
  unify a a'
  unify b b'

unify a b = throwError (NotEqual a b)

isRec :: String
isRec = "A record type's hole can only be instanced to another record"

overlap :: Typed ~ p => [(Text, Type p)] -> [(Text, Type p)] -> [(Type p, Type p)]
overlap xs ys
  | inter <- filter ((/=) 1 . length) $ groupBy ((==) `on` fst) (sortOn fst (xs ++ ys))
  = map get inter
  where get [(_, a), (_, b)] = (a, b)
        get _ = undefined

runSolve :: Int -> Subst Typed -> SolveM b -> Either TypeError (Int, (Subst Typed, Map.Map (Var Typed) (Coercion Typed)))
runSolve i s x = runExcept (fix (runReaderT (runStateT (runGenTFrom i act) (SolveState s mempty)) (SolveScope False mempty))) where
  act = (,) <$> gen <*> x
  fix act = do
    ((i, _), s) <- act
    pure (i, (s ^. solveTySubst, s ^. solveCoSubst))

solve :: Int -> Seq.Seq (Constraint Typed) -> Either TypeError (Subst Typed, Map.Map (Var Typed) (Coercion Typed))
solve i cs = snd <$> runSolve i mempty (doSolve cs)

doSolve :: Seq.Seq (Constraint Typed) -> SolveM ()
doSolve Empty = pure ()
doSolve (ConUnify because v a b :<| xs) = do
  sub <- use solveTySubst
  unify (apply sub a) (apply sub b)
    `catchError` \e -> throwError (ArisingFrom e because)

  solveCoSubst . at v .= Just (coercionOf (apply sub a) (apply sub b))
  doSolve xs
doSolve (ConSubsume because v a b :<| xs) = do
  sub <- use solveTySubst
  subsumes unify (apply sub a) (apply sub b)
    `catchError` \e -> throwError (ArisingFrom e because)

  solveCoSubst . at v .= Just (coercionOf (apply sub b) (apply sub a))
  doSolve xs
doSolve (ConImplies because not cs ts :<| xs) = do
  before <- use solveTySubst
  let not' = ftv (apply before not)
      cs' = apply before cs
      ts' = apply before ts

      leak t (TyVar k) m
        | t `Set.member` not' = Map.insert k (TyVar t) m
      leak _ _ m = m
  do
    let go = local (bindSkol .~ True) $ doSolve cs'

    ((), sub) <- local (don'tTouch .~ mempty) . capture $ go

    let sub' = sub ^. solveTySubst
        after = Map.foldrWithKey leak before sub'

    local (don'tTouch %~ Set.union not') $ do
      solveTySubst .= sub'
      doSolve (fmap (apply sub') ts')
        `catchError` \e -> throwError (ArisingFrom e because)
      solveTySubst .= after

    doSolve xs
doSolve (ConFail v t :<| cs) = do
  doSolve cs
  sub <- use solveTySubst
  let unskolemise x@(TySkol v) = case sub ^. at (v ^. skolVar) of
        Just t | t == TySkol v -> TyVar (v ^. skolVar)
        _ -> x
      unskolemise x = x
      go :: Type Typed -> Type Typed
      go = transformType unskolemise
  throwError (FoundHole v (go (apply sub t)))

subsumes :: (Type Typed -> Type Typed -> SolveM b)
         -> Type Typed -> Type Typed -> SolveM b
subsumes k t1 t2@TyForall{} = do
  sub <- use solveTySubst
  t2' <- skolemise (BySubsumption (apply sub t1) (apply sub t2)) t2
  subsumes k t1 t2'
subsumes k t1@TyForall{} t2 = do
  (_, _, t1') <- instantiate t1
  subsumes k t1' t2
subsumes k a b = k a b

skolemise :: MonadGen Int m => SkolemMotive Typed -> Type Typed -> m (Type Typed)
skolemise motive ty@(TyForall tvs t) = do
  sks <- traverse (freshSkol motive ty) tvs
  skolemise motive (apply (Map.fromList (zip tvs sks)) t)
skolemise motive (TyArr c d) = TyArr c <$> skolemise motive d
skolemise _ ty = pure ty

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

coercionOf :: Type Typed -> Type Typed -> Coercion Typed
coercionOf (TyWithConstraints cs a) b =
  foldr CompCo (coercionOf a b) (map (uncurry ReflCo) cs)
coercionOf b (TyWithConstraints cs a) =
  foldr CompCo (coercionOf b a) (map (uncurry ReflCo) cs)
coercionOf a b = ReflCo a b
