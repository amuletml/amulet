module Types.Unify where

import Control.Monad.Except
import Control.Monad.Infer

import Syntax.Subst
import Syntax

import qualified Data.Map.Strict as M
import qualified Data.Set as S

unify :: Type -> Type -> InferM ()
unify (TyVar a) b = tell [ConInstance a b]
unify a (TyVar b) = tell [ConInstance b a]
unify (TyArr a b) (TyArr a' b') = do
  unify a a'
  unify b b'
unify (TyApp a b) (TyApp a' b') = do
  unify a a'
  unify b b'
unify ta@(TyCon a) tb@(TyCon b)
  | a == b = pure ()
  | otherwise = throwError (NotEqual ta tb)
unify a b = throwError (NotEqual a b)

solve :: Subst -> [Constraint] -> Either TypeError Subst
solve m (ConEquality a b:xs)
  | a == b = solve m xs
  | otherwise = throwError (NotEqual a b)
solve m (ConInstance a t:xs)
  | occurs a t = throwError (Occurs a t)
  | otherwise =
    case M.lookup a m of
      Just TyVar{} -> solve (M.insert a t m) xs
      Just t' | t == t' -> solve m xs
              | TyVar _ <- t -> solve (M.insert a t m) xs
              | otherwise -> throwError (NotEqual t t')
      Nothing -> solve (M.insert a t m) xs
solve m [] = pure m

occurs :: String -> Type -> Bool
occurs _ (TyVar _) = False
occurs x e = x `S.member` ftv e
