{-# Language MultiWayIf, GADTs, FlexibleContexts #-}
module Types.Unify (solve, overlap, bind, skolemise, freshSkol) where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Infer

import Types.Wellformed

import Syntax.Subst
import Syntax

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Data.Foldable
import Data.Function
import Data.List

import Data.Text (Text)

type SolveM = GenT Int (StateT (Subst Typed) (Except TypeError))

bind :: Var Typed -> Type Typed -> SolveM ()
bind var ty
  | occurs var ty = throwError (Occurs var ty)
  | TyVar var == ty = pure ()
  | TyForall{} <- ty = throwError (Impredicative var ty)
  | otherwise = do
      env <- get
      -- Attempt to extend the environment, otherwise unify with existing type
      case Map.lookup var env of
        Nothing -> put (env `compose` Map.singleton var (normType ty))
        Just ty'
          | ty' == ty -> pure ()
          | otherwise -> unify (normType ty) (normType ty')

unify :: Type Typed -> Type Typed -> SolveM ()
unify (TyVar a) b = bind a b
unify a (TyVar b) = bind b a

unify (TySkol x) (TySkol y)
  | x == y = pure ()
unify (TySkol t) b = throwError $ SkolBinding t b
unify b (TySkol t) = throwError $ SkolBinding t b

unify (TyWithConstraints cs a) t = do
  for_ cs $ \(a, b) -> unify a b
  unify a t
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
runSolve i s x = runExcept (fix (runStateT (runGenTFrom i act) s)) where
  act = (,) <$> gen <*> x
  fix act = do
    ((i, _), s) <- act
    pure (i, s)

solve :: Int -> Subst Typed -> [Constraint Typed] -> Either TypeError (Subst Typed)
solve _ s [] = pure s
solve i s (ConUnify e a t:xs) = do
  case wellformed t of
    Left err -> Left (Note (ArisingFrom err e) wellform)
    Right () -> Right ()
  case wellformed a of
    Left err -> Left (Note (ArisingFrom err e) wellform)
    Right () -> Right ()

  case runSolve i s (unify (normType a) (normType t)) of
    Left err -> Left (ArisingFrom err e)
    Right (i', s') -> solve i' s' (apply s' xs)
solve i s (ConSubsume e a b:xs) =
  case runSolve i s (subsumes unify (normType a) (normType b)) of
    Left err -> Left (ArisingFrom err e)
    Right (i', s') -> solve i' s' (apply s' xs)
solve i s (ConImplies reason cs ts:_) =
  case solve i s cs of
    Left e -> Left (ArisingFrom e reason)
    Right ss -> case solve i ss ts of
      Left e -> Left (ArisingFrom e reason)
      Right ss' -> Right $
        let strip = ftv cs
         in Map.filterWithKey (\k _ -> k `Set.notMember` strip) ss'


subsumes :: (MonadGen Int m, MonadError TypeError m)
         => (Type Typed -> Type Typed -> m b)
         -> Type Typed -> Type Typed -> m b
subsumes k t1 t2@TyForall{} = do
  t2' <- skolemise t2
  subsumes k t1 t2'
subsumes k t1@TyForall{} t2 = do
  (_, _, t1') <- instantiate t1
  subsumes k t1' t2
subsumes k a b = k a b

skolemise :: MonadGen Int m => Type Typed -> m (Type Typed)
skolemise ty@(TyForall tvs t) = do
  sks <- traverse (freshSkol ty) tvs
  skolemise (apply (Map.fromList (zip tvs sks)) t)
skolemise (TyArr c d) = TyArr c <$> skolemise d
skolemise ty = pure ty

freshSkol :: MonadGen Int m => Type Typed -> Var Typed -> m (Type Typed)
freshSkol ty u = do
  var <- TvName <$> fresh
  pure (TySkol (Skolem var u ty))

occurs :: Var Typed -> Type Typed -> Bool
occurs _ (TyVar _) = False
occurs x e = x `Set.member` ftv e

wellform :: String
wellform = "The type was rejected in the wellformedness check;\n         It is possible this is a bug."
