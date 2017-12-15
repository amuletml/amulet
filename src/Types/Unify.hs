{-# Language MultiWayIf, GADTs #-}
module Types.Unify (solve, overlap, bind) where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Infer

import Types.Wellformed

import Syntax.Subst
import Syntax

import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Data.Function
import Data.List

import Data.Text (Text)

type SolveM = GenT Int (StateT (Subst Typed) (Except TypeError))

bind :: Var Typed -> Type Typed -> SolveM ()
bind var ty
  | ty == TyVar var = return ()
  | occurs var ty = throwError (Occurs var ty)
  | otherwise = do
      env <- get
      -- Attempt to extend the environment, otherwise unify with existing type
      case M.lookup var env of
        Nothing -> put (M.singleton var ty `compose` env)
        Just ty'
          | ty == ty -> pure ()
          | otherwise -> unify ty ty'

unify :: Type Typed -> Type Typed -> SolveM ()
unify (TyVar a) b = bind a b
unify a (TyVar b) = bind b a
unify (TyArr a b) (TyArr a' b') = unify a a' *> unify b b'
unify (TyApp a b) (TyApp a' b') = unify a a' *> unify b b'
unify ta@(TyCon a) tb@(TyCon b)
  | a == b = pure ()
  | otherwise = throwError (NotEqual ta tb)
unify t@(TyForall vs ty) t'@(TyForall vs' ty')
  | length vs /= length vs' = throwError (NotEqual t t')
  -- TODO: Technically we should make fresh variables and do ty[vs/f] ~ ty'[vs'/f]
  | otherwise = unify ty (apply (M.fromList (zip vs' (map TyVar vs))) ty')
unify (TyRows rho arow) (TyRows sigma brow)
  | overlaps <- overlap arow brow
  , new <- unionBy ((==) `on` fst) arow brow
  = do mapM_ (uncurry unify) overlaps
       tau <- freshTV
       unify rho (TyRows tau new)
       unify sigma (TyRows tau new)
       if length overlaps >= length new
          then error ("overlaps " ++ show (length overlaps) ++ " new " ++ show (length new))
          else pure ()
-- TODO: This is a bit hacky. We have a different type for "closed
-- records" (literals) and "open records" (parameters), and must check
-- that they line up manually here.
unify ta@(TyExactRows arow) tb@(TyRows _ brow)
  | overlaps <- overlap arow brow
  = case overlaps of
      [] -> throwError (NoOverlap ta tb)
      xs -> mapM_ (uncurry unify) xs
unify tb@(TyRows _ brow) ta@(TyExactRows arow)
  | overlaps <- overlap arow brow
  = case overlaps of
      [] -> throwError (NoOverlap ta tb)
      xs -> mapM_ (uncurry unify) xs
unify ta@(TyExactRows arow) tb@(TyExactRows brow)
  | overlaps <- overlap arow brow
  = do when (length overlaps /= length arow || length overlaps /= length brow)
         $ throwError (NoOverlap ta tb)
       mapM_ (uncurry unify) overlaps

unify x tp@TyRows{} = throwError (Note (CanNotInstance tp x) isRec)
unify tp@TyRows{} x = throwError (Note (CanNotInstance tp x) isRec)
unify (TyTuple a b) (TyTuple a' b') = do
  unify a a'
  unify b b'

unify (TyCons cs t) t' = do
  forM_ cs $ \(Equal a b _) -> unify a b
  unify t t'
unify t' (TyCons cs t) = do
  forM_ cs $ \(Equal a b _) -> unify a b
  unify t t'

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
    Left err -> Left (Note (ArisingFrom err e) "The type was rejected in the wellformedness check;\n         It is possible this is a bug.")
    Right () -> Right ()
  case wellformed a of
    Left err -> Left (Note (ArisingFrom err e) "The type was rejected in the wellformedness check;\n         It is possible this is a bug.")
    Right () -> Right ()
  case runSolve i s (unify a t) of
    Left err -> Left (ArisingFrom err e)
    Right (i', s') -> solve i' (s' `compose` s) (apply s' xs)

occurs :: Var Typed -> Type Typed -> Bool
occurs _ (TyVar _) = False
occurs x e = x `S.member` ftv e
