{-# Language MultiWayIf #-}
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

unify (TyForall vs _ ty) ot = do
  xs <- forM vs $ \_ -> TyVar <$> fresh
  tell (zipWith ConInstance vs xs)
  let x = apply (M.fromList (zip vs xs)) ty
  unify x ot
unify ot (TyForall vs _ ty) = do
  xs <- forM vs $ \_ -> TyVar <$> fresh
  tell (zipWith ConInstance vs xs)
  let x = apply (M.fromList (zip vs xs)) ty
  unify x ot

unify a b = throwError (NotEqual a b)

solve :: Subst -> [Constraint] -> Either TypeError Subst
solve m (ConEquality a b:xs)
  | a == b = solve m xs
  | otherwise = throwError (NotEqual a b)
solve m (ConInstance a t:xs)
  | occurs a t = throwError (Occurs a t)
  | otherwise =
    case M.lookup a m of
      Just TyVar{} -> solve (M.insert a t m) (substCons a t xs)
      Just lt ->
        let t' = apply (M.insert a t m) lt
         in if | t == t' -> solve m (substCons a t xs)
               | TyVar _ <- t -> solve (M.insert a t m) (substCons a t xs)
               | otherwise -> solve m (substCons a t (xs ++ [ConEquality t t']))
      Nothing -> solve (M.insert a t m) (substCons a t xs)
solve m [] = pure m

substCons :: String -> Type -> [Constraint] -> [Constraint]
substCons n t (ConEquality a b:xs) = ConEquality (apply s a) (apply s b):substCons n t xs where
  s = M.singleton n t
substCons n t (ConInstance v g:xs) = ConInstance v (apply s g):substCons n t xs where
  s = M.singleton n t
substCons _ _ [] = []

occurs :: String -> Type -> Bool
occurs _ (TyVar _) = False
occurs x e = x `S.member` ftv e
