{-# Language MultiWayIf, GADTs, FlexibleContexts, ScopedTypeVariables #-}
module Types.Unify (solve, overlap, bind, skolemise, freshSkol) where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Infer
import Control.Lens

import Types.Wellformed

import Syntax.Subst
import Syntax

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Foldable
import Data.Function
import Data.Generics
import Data.List
import Data.Text (Text)

type SolveM = GenT Int (StateT (Subst Typed) (ReaderT (Set.Set (Var Typed)) (Except TypeError)))

bind :: Var Typed -> Type Typed -> SolveM ()
bind var ty
  | occurs var ty = throwError (Occurs var ty)
  | TyVar var == ty = pure ()
  | TyForall{} <- ty = throwError (Impredicative var ty)
  | otherwise = do
      env <- get
      noTouch <- ask
      -- Attempt to extend the environment, otherwise unify with existing type
      case Map.lookup var env of
        Nothing ->
          if | var `Set.notMember` noTouch -> put (env `compose` Map.singleton var (normType ty))
             | var `Set.member` noTouch, TyVar v <- ty, v `Set.notMember` noTouch -> put (env `compose` Map.singleton v (TyVar var))
             | otherwise -> throwError (NotEqual (TyVar var) ty)
        Just ty'
          | ty' == ty -> pure ()
          | otherwise -> unify (normType ty) (normType ty')

unify :: Type Typed -> Type Typed -> SolveM ()
unify (TySkol x) (TySkol y)
  | x == y = pure ()
unify (TySkol t@(Skolem sv _ _ _)) b = do
  sub <- use (at sv)
  case sub of
    Just t -> unify t b
    Nothing -> case b of
      TyVar v -> bind v (TySkol t)
      _ -> throwError $ SkolBinding t b
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

runSolve :: Int -> Subst Typed -> SolveM b -> Either TypeError (Int, Subst Typed)
runSolve i s x = runExcept (fix (runReaderT (runStateT (runGenTFrom i act) s) mempty)) where
  act = (,) <$> gen <*> x
  fix act = do
    ((i, _), s) <- act
    pure (i, s)

solve :: Int -> [Constraint Typed] -> Either TypeError (Subst Typed)
solve i cs = snd <$> runSolve i mempty (doSolve cs)

doSolve :: [Constraint Typed] -> SolveM ()
doSolve [] = pure ()
doSolve (ConUnify because a b:xs) = do
  sub <- get
  unify (apply sub a) (apply sub b)
    `catchError` \e -> throwError (ArisingFrom e because)
  doSolve xs
doSolve (ConSubsume because a b:xs) = do
  sub <- get
  subsumes unify (apply sub a) (apply sub b)
    `catchError` \e -> throwError (ArisingFrom e because)
  doSolve xs
doSolve (ConImplies because not cs ts:xs) = do
  before <- get
  let not' = ftv (apply before not)
      cs' = apply before cs
      ts' = apply before ts

      leak t (TyVar k) m
        | t `Set.member` not' = Map.insert k (TyVar t) m
      leak _ _ m = m
  do
    let go = doSolve cs'
              `catchError` \e -> case realErr e of
              SkolBinding (Skolem v _ _ _) t -> unify (TyVar v) t
              _ -> throwError (ArisingFrom e because)
        go :: SolveM ()

    ((), sub) <- local (const mempty) . capture $ go

    let after = Map.foldrWithKey leak before sub

    local (Set.union not') $ do
      put sub
      doSolve (map (apply sub) ts')
        `catchError` \e -> throwError (ArisingFrom e because)
      put after

    doSolve xs
doSolve (ConFail v t:cs) = do
  doSolve cs
  sub <- get
  let unskolemise x@(TySkol v) = case sub ^. at (v ^. skolVar) of
        Just t | t == TySkol v -> TyVar (v ^. skolVar)
        _ -> x
      unskolemise x = x
      go :: Type Typed -> Type Typed
      go = everywhere (mkT unskolemise)
  throwError (FoundHole v (go (apply sub t)))

subsumes :: (Type Typed -> Type Typed -> SolveM b)
         -> Type Typed -> Type Typed -> SolveM b
subsumes k t1 t2@TyForall{} = do
  sub <- get
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

realErr :: TypeError -> TypeError
realErr (ArisingFrom e _) = realErr e
realErr t = t
