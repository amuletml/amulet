{-# Language MultiWayIf #-}
module Types.Unify (solve, smush) where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Infer

import Syntax.Subst
import Syntax

import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Data.Span

type SolveM = StateT Subst (Except TypeError)

bind :: Var Typed -> Type Typed -> SolveM ()
bind var ty | raiseT id (const internal) ty == TyVar var internal = return ()
            | occurs var ty = throwError (Occurs var ty)
            | otherwise = modify (M.singleton var ty `compose`)

unify :: Type Typed -> Type Typed -> SolveM ()
unify (TyVar a _) b = bind a b
unify a (TyVar b _) = bind b a
unify (TyArr a b _) (TyArr a' b' _) = do
  unify a a'
  unify b b'
unify (TyApp a b _) (TyApp a' b' _) = do
  unify a a'
  unify b b'
unify ta@(TyCon a _) tb@(TyCon b _)
  | smush a == smush b = pure ()
  | otherwise = throwError (NotEqual ta tb)
unify t@(TyForall vs _ ty _) t'@(TyForall vs' _ ty' _)
  | length vs /= length vs' = throwError (NotEqual t t')
  -- TODO: Technically we should make fresh variables and do ty[vs/f] ~ ty'[vs'/f]
  | otherwise = unify ty (apply (M.fromList (zip vs' (map (flip TyVar internal) vs))) ty')
unify a b = throwError (NotEqual a b)

smush :: Var Typed -> Var Typed
smush (TvName v _) = TvName v internalTyVar
smush (TvRefresh v k) = TvRefresh (smush v) k

runSolve :: Subst -> SolveM b -> Either TypeError Subst
runSolve s x = runExcept (execStateT x s)

solve :: Subst -> [Constraint Typed] -> Either TypeError Subst
solve s [] = pure s
solve s (ConUnify e a t:xs) =
  case runSolve s (unify a t) of
    Left err -> Left (ArisingFrom err e)
    Right s' -> solve (s' `compose` s) (apply s' xs)

occurs :: Var Typed -> Type Typed -> Bool
occurs _ (TyVar _ _) = False
occurs x e = x `S.member` ftv e
