{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}
module Core.Lower.TypeRepr (getTypeRepr) where

import qualified Data.VarSet as VarSet

import Control.Monad.Namey
import Control.Monad

import Core.Lower.Basic
import Core.Optimise

import Syntax (Constructor(..))
import Syntax.Var (Typed)

getTypeRepr :: MonadNamey m
            => CoVar -> Maybe [Constructor Typed]
            -> m ([Stmt CoVar], TypeRepr)
getTypeRepr var Nothing = pure ([Type var []], OpaqueTy)
getTypeRepr var (Just ctors) =
  let ctors' = map (\case
                       UnitCon _ p (_, t)  -> (mkCon p, lowerType t)
                       ArgCon _ p _ (_, t) -> (mkCon p, lowerType t)
                       GadtCon _ p t _     -> (mkCon p, lowerType t)) ctors
  in case ctors' of
    [(ctor, ty)] | Just nt@(Spine _ dom cod) <- isNewtype ty -> do
      let CoVar name id _ = ctor

      wrapper <- newtypeWorker nt
      pure ( [ Type var [], StmtLet (One (CoVar name id ValueVar, ty, wrapper))]
           , WrapperTy ctor dom cod )

    _ -> pure ( [ Type var ctors' ]
              , SumTy (VarSet.fromList (map fst ctors')) )

isNewtype :: Type -> Maybe Spine
isNewtype (ForallTy Irrelevant _ ForallTy{}) = Nothing -- Cannot have multiple relevant arguments
isNewtype (ForallTy Irrelevant from to) =
  pure (Spine [(Irrelevant, from)] from to)
isNewtype (ForallTy (Relevant var) k rest) = do
  (Spine tys from to) <- isNewtype rest
  guard (var `occursInTy` to)
  pure (Spine ((Relevant var, k):tys) from to)
isNewtype _ = Nothing

data Spine = Spine [(BoundTv, Type)] Type Type
  deriving (Eq, Show, Ord)

newtypeWorker :: forall a m. (IsVar a, MonadNamey m)
              => Spine -> m (Term a)
newtypeWorker (Spine tys dom cod) = do
  let wrap :: [(BoundTv, Type)] -> (CoVar -> Type -> Term a) -> m (Term a)
      wrap ((Relevant v, c):ts) ex = Lam (TypeArgument (fromVar v) c) <$> wrap ts ex
      wrap [(Irrelevant, c)] ex = do
        v <- fresh ValueVar
        pure (Lam (TermArgument (fromVar v) c) (ex v c))
      wrap _ _ = undefined

      work :: CoVar -> Type -> Term a
      work var ty = Cast (Ref var ty) cod (SameRepr dom cod)

  wrap tys work
