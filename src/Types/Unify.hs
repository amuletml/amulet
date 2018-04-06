{-# LANGUAGE MultiWayIf, GADTs, FlexibleContexts, ScopedTypeVariables, TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module Types.Unify (solve, overlap, bind, skolemise, freshSkol) where

import Control.Monad.Except
import Control.Monad.State
import Control.Applicative
import Control.Monad.Infer
import Control.Lens hiding (Empty)

import Types.Infer.Errors
import Types.Wellformed

import Syntax.Subst
import Syntax

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Traversable
import Data.Sequence (Seq ((:<|), Empty))
import Data.Foldable
import Data.Function
import Data.List
import Data.Text (Text)

data SolveScope
  = SolveScope { _bindSkol :: Bool
               , _don'tTouch :: Set.Set (Var Typed)
               , _don'tCoerce :: Set.Set (Var Typed) }
  deriving (Eq, Show, Ord)

data SolveState
  = SolveState { _solveTySubst :: Subst Typed
               , _solveCoSubst :: Map.Map (Var Typed) (Wrapper Typed)
               }
  deriving (Eq, Show, Ord)

makeLenses ''SolveState
makeLenses ''SolveScope

type SolveM = GenT Int (StateT SolveState (ReaderT SolveScope (Except TypeError)))

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
      noTouch <- view don'tTouch
      -- Attempt to extend the environment, otherwise unify with existing type
      case Map.lookup var env of
        Nothing -> do
          if | var `Set.notMember` noTouch -> solveTySubst .= (env `compose` Map.singleton var (normType ty))
             | var `Set.member` noTouch, TyVar v <- ty, v `Set.notMember` noTouch -> solveTySubst .= (env `compose` Map.singleton v (TyVar var))
             | otherwise -> throwError (NotEqual (TyVar var) ty)
          pure (AssumedCo (TyVar var) ty)
        Just ty'
          | ty' == ty -> pure (ReflCo ty')
          | otherwise -> unify (normType ty) (normType ty')

unify :: Type Typed -> Type Typed -> SolveM (Coercion Typed)
unify (TySkol x) (TySkol y)
  | x == y = pure (ReflCo (TySkol y))
  | otherwise = do
    sub <- use solveTySubst
    let assumption = sub ^. at (x ^. skolIdent) <|> sub ^. at (y ^. skolIdent)
    case assumption of
      Just{} -> pure (AssumedCo (TySkol x) (TySkol y))
      Nothing -> do
        canWe <- view bindSkol
        if canWe
           then bind (x ^. skolIdent) (TySkol y)
           else throwError $ SkolBinding x (TySkol y)

unify (TySkol t@(Skolem sv _ _ _)) b = do
  sub <- use (solveTySubst . at sv)
  case sub of
    Just ty -> do
      _ <- unify b ty
      pure (AssumedCo ty (TySkol t))
    Nothing -> case b of
      TyVar v -> bind v (TySkol t)
      _ -> do
        canWe <- view bindSkol
        if canWe
           then bind sv b
           else throwError $ SkolBinding t b
unify b (TySkol t) = SymCo <$> unify (TySkol t) b

unify (TyVar a) b = bind a b
unify a (TyVar b) = SymCo <$> bind b a

unify (TyWithConstraints cs a) t = do
  traverse_ (uncurry unify) cs
  unify a t

unify t (TyWithConstraints cs ty) = do
  traverse_ (uncurry unify) cs
  unify t ty

unify (TyArr a b) (TyArr a' b') = ArrCo <$> unify a a' <*> unify b b'
unify (TyPi (Dependent v k) b) (TyPi (Dependent v' k') b') = do
  fresh <- freshTV
  let TyVar tv = fresh
  _ <- unify k k'
  ForallCo tv <$> unify (apply (Map.singleton v fresh) b) (apply (Map.singleton v' fresh) b')
unify (TyApp a b) (TyApp a' b') = AppCo <$> unify a a' <*> unify b b'

unify ta@(TyCon a) tb@(TyCon b)
  | a == b = pure (ReflCo tb)
  | otherwise = throwError (NotEqual ta tb)

unify (TyForall v Nothing ty) (TyForall v' Nothing ty') = do
  fresh <- freshTV
  let (TyVar tv) = fresh
  ForallCo tv <$>
    unify (apply (Map.singleton v fresh) ty) (apply (Map.singleton v' fresh) ty')

unify (TyForall v (Just k) ty) (TyForall v' (Just k') ty') =
  unify k k' *> unify (TyForall v Nothing ty) (TyForall v' Nothing ty')

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
        co <- unify rho (TyExactRows rhoNew)
        pure (RowsCo co cs)

unify ta@(TyExactRows arow) tb@(TyExactRows brow)
  | overlaps <- overlap arow brow
  = do when (length overlaps /= length arow || length overlaps /= length brow)
         $ throwError (NoOverlap ta tb)
       cs <- traverse unifRow overlaps
       pure (ExactRowsCo cs)

unify x tp@TyRows{} = throwError (Note (CanNotInstance tp x) isRec)
unify tp@TyRows{} x = throwError (Note (CanNotInstance tp x) isRec)
unify (TyTuple a b) (TyTuple a' b') = do
  ProdCo <$> unify a a' <*> unify b b'

unify (TyTerm a) (TyTerm b) = unifyTerm a b
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
runSolve i s x = runExcept (fix (runReaderT (runStateT (runGenTFrom i act) (SolveState s mempty)) emptyScope)) where
  act = (,) <$> gen <*> x
  fix act = do
    ((i, _), s) <- act
    let ss = s ^. solveTySubst
     in pure (i, (fmap (apply ss) ss, s ^. solveCoSubst))
  emptyScope = SolveScope False mempty mempty

solve :: Int -> Seq.Seq (Constraint Typed) -> Either TypeError (Subst Typed, Map.Map (Var Typed) (Wrapper Typed))
solve i cs = snd <$> runSolve i mempty (doSolve cs)

doSolve :: Seq.Seq (Constraint Typed) -> SolveM ()
doSolve Empty = pure ()
doSolve (ConUnify because v a b :<| xs) = do
  sub <- use solveTySubst

  co <- unify (apply sub a) (apply sub b)
    `catchError` \e -> throwError (ArisingFrom e because)
  solveCoSubst . at v .= Just (Cast co)

  doSolve xs
doSolve (ConSubsume because v a b :<| xs) = do
  sub <- use solveTySubst

  co <- subsumes unify (apply sub a) (apply sub b)
    `catchError` \e -> throwError (ArisingFrom e because)
  solveCoSubst . at v .= Just co

  doSolve xs
doSolve (ConImplies because not cs ts :<| xs) = do
  before <- use solveTySubst
  coercions <- use solveCoSubst
  let not' = ftv (apply before not)
      cs' = apply before cs
      ts' = apply before ts

      leak ass v ty m
        | null (ftv ty `Set.intersection` ass)
        , v `Set.notMember` ass
        = Map.insert v ty m
        | otherwise = m

  do
    let go = local (bindSkol .~ True) $ doSolve cs'

    ((), sub) <- local (don'tTouch .~ mempty) . capture $ go

    let sub' = sub ^. solveTySubst
        assum = Set.fromList (Map.keys sub')
        -- after = Map.foldrWithKey leak before sub'
    solveCoSubst .= coercions

    local (don'tTouch %~ Set.union not') . local (don'tCoerce %~ Set.union assum) $ do
      solveTySubst .= sub'
      doSolve (fmap (apply sub') ts')
        `catchError` \e -> throwError (ArisingFrom e because)

      con <- use solveTySubst

      let after = Map.foldrWithKey (leak assum) before con
      solveTySubst .= after

    doSolve xs
doSolve (ConFail v t :<| cs) = do
  doSolve cs
  sub <- use solveTySubst
  throwError (foundHole v (apply sub t) sub)

subsumes :: (Type Typed -> Type Typed -> SolveM (Coercion Typed))
         -> Type Typed -> Type Typed -> SolveM (Wrapper Typed)
subsumes k t1 t2@TyForall{} = do
  sub <- use solveTySubst
  (c, t2') <- skolemise (BySubsumption (apply sub t1) (apply sub t2)) t2
  (Syntax.:>) c <$> subsumes k t1 t2'
subsumes k t1@TyForall{} t2 = do
  (_, _, t1') <- instantiate t1
  subsumes k t1' t2
subsumes k a b = Cast <$> k a b

skolemise :: MonadGen Int m => SkolemMotive Typed -> Type Typed -> m (Wrapper Typed, Type Typed)
skolemise motive ty@(TyForall tv _ t) = do
  sk <- freshSkol motive ty tv
  (wrap, ty) <- skolemise motive (apply (Map.singleton tv sk) t)
  pure (TypeLam tv Syntax.:> wrap, ty)
skolemise motive (TyArr c d) = do
  (wrap, ty) <- skolemise motive d
  pure (wrap, TyArr c ty)
skolemise _ ty = pure (IdWrap, ty)

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

unifyTerm :: Expr Typed -> Expr Typed -> SolveM (Coercion Typed)
unifyTerm a@(VarRef v _) (VarRef v' _)
  | v == v' = pure (ReflCo (TyTerm a))
unifyTerm x@(Literal l _) (Literal l' _)
  | l == l' = pure (ReflCo (TyTerm x))

unifyTerm (App f x _) (App f' x' _) = AppCo <$> unifyTerm f f' <*> unifyTerm x x'

unifyTerm (Tuple [x] _) (Tuple [x'] _) = unifyTerm x x'
unifyTerm (Tuple (x:xs) a) (Tuple (y:ys) b) = do
  cx <- unifyTerm (Tuple xs a) (Tuple ys b)
  ProdCo <$> unifyTerm x y <*> pure cx

unifyTerm (ExprWrapper (TypeApp x) f _) (ExprWrapper (TypeApp x') f' _) = AppCo <$> unifyTerm f f' <*> unify x x'

unifyTerm x@(Record rs _) y@(Record rs' _)
  | rs <- sortOn fst rs, rs' <- sortOn fst rs' = do
    when (length rs /= length rs') $
      throwError (NotEqual (TyTerm x) (TyTerm y))
    fmap ExactRowsCo . for (zip rs rs') $ \((ra, a), (rb, b)) -> do
      when (ra /= rb) $ 
        throwError (NotEqual (TyTerm x) (TyTerm y))
      (ra,) <$> unifyTerm a b

unifyTerm x@(RecordExt a rs _) y@(RecordExt b rs' _)
  | rs <- sortOn fst rs, rs' <- sortOn fst rs' = do
    c <- unifyTerm a b
    when (length rs /= length rs') $
      throwError (NotEqual (TyTerm x) (TyTerm y))
    fmap (RowsCo c) . for (zip rs rs') $ \((ra, a), (rb, b)) -> do
      when (ra /= rb) $ 
        throwError (NotEqual (TyTerm x) (TyTerm y))
      (ra,) <$> unifyTerm a b

unifyTerm (ExprWrapper Cast{} e _) (ExprWrapper Cast{} e' _) = unifyTerm e e'
unifyTerm a b = throwError (NotEqual (TyTerm a) (TyTerm b))

