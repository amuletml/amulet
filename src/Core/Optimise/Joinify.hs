{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, TupleSections #-}

{-| This module emulates join point conversion/match conversion using
  normal lambdas.

  Effectively we take a statement like so:

  > let a = match A with ... -> ...
  > match a with ...

  and convert it into

  > let t = \a -> match a with ...
  > match A with ... -> t (...)

  The idea here is that trivial "t"s will be inlined and so facilitate
  match-of-match conversion.

  This pass does introduce a reasonable amount of code duplication due to
  later inlining, so it would be ideal to perform some further
  evaluations to determine if such an optimisation is useful.
-}
module Core.Optimise.Joinify where

import Control.Monad.Namey
import Control.Lens

import Data.Triple

import Core.Optimise
import Core.Types

-- | Perform the match to join-point conversion pass.
matchJoinPass :: forall m a. (MonadNamey m, IsVar a) => [Stmt a] -> m [Stmt a]
matchJoinPass = traverse transS where
  transS (StmtLet (Many vars)) = StmtLet . Many <$> traverse (third3A transT) vars
  transS (StmtLet (One (v, t, e))) = StmtLet . One . (v, t, ) <$> transT e
  transS s = pure s

  transT t@Atom{} = pure t
  transT t@App{} = pure t
  transT t@TyApp{} = pure t
  transT t@Cast{} = pure t
  transT t@Extend{} = pure t
  transT t@Values{} = pure t

  transT (Lam arg b) = Lam arg <$> transT b
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
    transT (Let (One (join, joinTy, Lam (TermArgument name nameTy) cont))
              (Match sc as))

  transT (Let (One var) r) = do
    var' <- third3A transT var
    Let (One var') <$> transT r

  transT (Let (Many vs) r) = do
    vs' <- traverse (third3A transT) vs
    Let (Many vs') <$> transT r

  transT (Match t bs) = Match t <$> traverse (armBody %%~ transT) bs
