{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, TupleSections #-}
module Core.Optimise.Joinify where

import Control.Monad.Namey
import Control.Lens

import Data.Triple

import Core.Optimise
import Core.Types

matchJoinPass :: forall m a. (MonadNamey m, IsVar a) => [Stmt a] -> m [Stmt a]
matchJoinPass = traverse transS where
  transS (StmtLet (Many vars)) = StmtLet . Many <$> traverse (third3A transT) vars
  transS (StmtLet (One (v, t, e))) = StmtLet . One . (v, t, ) <$> transT e
  transS s = pure s

  transA t@Ref{} = pure t
  transA t@Lit{} = pure t
  transA (Lam arg b) = Lam arg <$> transT b

  transT (Atom a) = Atom <$> transA a
  transT (App f a) = App <$> transA f <*> transA a
  transT (TyApp f t) = flip TyApp t <$> transA f
  transT (Cast f t) = flip Cast t <$> transA f

  transT (Let (One (name, nameTy, Match sc as)) cont) = do
    join <- fromVar <$> fresh ValueVar
    let Just res = approximateType cont
        joinTy = ForallTy Irrelevant nameTy res

        shoveJoinArm :: Atom a -> Type a -> Arm a -> m (Arm a)
        shoveJoinArm j ty = armBody %%~ shoveJoin j ty

        shoveJoin :: Atom a -> Type a -> Term a -> m (Term a)
        shoveJoin j ty (Let bind body) = Let bind <$> shoveJoin j ty body
        shoveJoin j ty (Match t bs) = Match t <$> traverse (shoveJoinArm j ty) bs
        shoveJoin j _  (Atom a) = pure (App j a)

        shoveJoin j (ForallTy Irrelevant ty _) ex = do
          var <- fromVar <$> fresh ValueVar
          pure (Let (One (var, ty, ex)) (App j (Ref var ty)))
        shoveJoin j t ex = error ("What: shoveJoin " ++ show j ++ " " ++ show t ++ " " ++ show ex)

    as <- traverse (shoveJoinArm (Ref join joinTy) joinTy) as
    transT (Let (One (join, joinTy, Atom (Lam (TermArgument name nameTy) cont)))
              (Match sc as))

  transT (Let (One var) r) = do
    var' <- third3A transT var
    Let (One var') <$> transT r

  transT (Let (Many vs) r) = do
    vs' <- traverse (third3A transT) vs
    Let (Many vs') <$> transT r

  transT (Extend t rs) = Extend <$> transA t <*> traverse (third3A transA) rs
  transT (Match t bs) = Match <$> transA t <*> traverse (armBody %%~ transT) bs
