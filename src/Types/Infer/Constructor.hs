{-# LANGUAGE FlexibleContexts #-}
module Types.Infer.Constructor (inferCon) where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Either

import Control.Monad.Infer

import Types.Infer.Builtin
import Types.Infer.Errors
import Types.Unify
import Types.Kinds

import Syntax.Subst
import Syntax

inferCon :: MonadInfer Typed m
         => Type Typed
         -> Constructor Resolved
         -> m ( (Var Typed, Type Typed)
              , Constructor Typed)
inferCon ret (ArgCon nm t ann) = do
  (ty', _) <- resolveKind t
  let res = closeOver $ TyArr ty' ret
  pure ((TvName nm, res), ArgCon (TvName nm) ty' (ann, res))

inferCon ret' (UnitCon nm ann) =
  let ret = closeOver ret'
   in pure ((TvName nm, ret), UnitCon (TvName nm) (ann, ret))

inferCon ret c@(GeneralisedCon nm cty ann) = do

  (cty, _) <- resolveKind cty
  var <- TvName <$> fresh
  case solve 1 (Seq.singleton (ConUnify (BecauseOf c) var (gadtConResult cty) ret)) of
    Left e -> throwError (gadtConShape (cty, ret) (gadtConResult cty) e)
    Right _ -> pure ()

  let generalise (TyForall v t) = TyForall v <$> generalise t
      generalise (TyArr a t) = TyArr a <$> generalise t
      generalise ty = case solve 1 (Seq.singleton (ConUnify (BecauseOf c) var ret ty)) of
        Right (x, _) -> do
          tell (map (\(x, y) -> (TyVar x, y)) (Map.toAscList x))
          pure ret
        Left e -> throwError e

  (cty, cons) <- runWriterT (generalise cty)
  let (sub, keep) = partitionEithers (map (uncurry simplify) cons)
      overall = closeOverGadt keep (apply (mconcat sub) cty)
   in pure ((TvName nm, overall), GeneralisedCon (TvName nm) overall (ann, overall))

closeOverGadt :: Ord (Var p) => [(Type p, Type p)] -> Type p -> Type p
closeOverGadt cons cty =
  let fv = ftv cty `Set.union` foldMap (\(x, y) -> ftv x `Set.union` ftv y) cons
      pushCons [] t = t
      pushCons c (TyForall v t) = TyForall v (pushCons c t)
      pushCons c t = TyWithConstraints c t

      forall xs t = foldr TyPi t (map Implicit xs)
   in if Set.null fv
         then pushCons cons cty
         else pushCons cons (forall (Set.toList fv) cty)


-- A bit of a hack for better aesthetics
simplify :: Type Typed -> Type Typed -> Either (Map.Map (Var Typed) (Type Typed)) (Type Typed, Type Typed)
simplify (TyVar v@(TvName (TgName x _))) b@(TyVar (TvName (TgName y _)))
  | x == y = Left (Map.singleton v b)
simplify x y = Right (x, y)
