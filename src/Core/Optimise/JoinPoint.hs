{-# LANGUAGE FlexibleContexts #-}
module Core.Optimise.JoinPoint
  ( joinPointPass
  ) where

import Control.Monad.Gen

import Data.VarSet (IsVar(..))
import Data.Triple

import Core.Optimise
import Core.Types

joinPointPass :: (MonadGen Int m, IsVar a) => [Stmt a] -> m [Stmt a]
joinPointPass = traverse transS where
  transS (StmtLet vars) = StmtLet <$> traverse (third3A transT) vars
  transS s = pure s

  transA t@Ref{} = pure t
  transA t@Lit{} = pure t
  transA (Lam arg b) = Lam arg <$> transT b

  transT (Atom a) = Atom <$> transA a
  transT (App f a) = App <$> transA f <*> transA a
  transT (TyApp f t) = flip TyApp t <$> transA f
  transT (Cast f t) = flip Cast t <$> transA f
  transT (Let (One BindValue (arg, argTy, Match t bs)) r) = do
    join <- fromVar <$> fresh
    let Just resTy = approximateType r
    let joinTy = ForallTy Irrelevant argTy resTy

    bs' <- traverse (pushBody (Ref join joinTy) joinTy) bs
    transT (Let (One BindJoin (join, joinTy, Atom (Lam (TermArgument arg argTy) r)))
                (Match t bs'))

      where pushJoin j ty (Let bind r) = Let bind <$> pushJoin j ty r
            pushJoin j ty (Match t bs) = Match t  <$> traverse (pushBody j ty) bs
            pushJoin j _   (Atom a) = pure $ App j a
            pushJoin j ty e = do
              v <- fromVar <$> fresh
              pure $ Let (One BindValue (v, ty, e)) (App j (Ref v ty))

            pushBody j ty a = (\x -> a { armBody = x }) <$> pushJoin j ty (armBody a)
  transT (Let (One k var) r) = do
    var' <- third3A transT var
    Let (One k var') <$> transT r
  transT (Let (Many k vs) r) = do
    vs' <- traverse (third3A transT) vs
    Let (Many k vs') <$> transT r
  transT (Extend t rs) = Extend <$> transA t <*> traverse (third3A transA) rs
  transT (Match t bs) = Match <$> transA t <*> traverse goArm bs where
    goArm x@Arm { armBody = bd } = (\it -> x { armBody = it }) <$> transT bd
