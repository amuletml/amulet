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
unify a b = throwError (NotEqual a b)

solve :: [Constraint] -> Either TypeError Subst
solve (ConEquality a b:xs)
  | a == b = solve xs
  | otherwise = throwError (NotEqual a b)
solve (ConInstance a t:xs)
  | occurs a t = throwError (Occurs a t)
  | otherwise = M.insert a t <$> solve xs
solve [] = pure M.empty

occurs :: String -> Type -> Bool
occurs _ (TyVar _) = False
occurs x e = x `S.member` ftv e
