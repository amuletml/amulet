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
inferCon ret con@(ArgCon nm t ann) = do
  ty' <- resolveKind (BecauseOf con) t
  res <- closeOver (BecauseOf con) $ TyArr ty' ret
  pure ((TvName nm, res), ArgCon (TvName nm) ty' (ann, res))

inferCon ret' con@(UnitCon nm ann) = do
  ret <- closeOver (BecauseOf con) ret'
  pure ((TvName nm, ret), UnitCon (TvName nm) (ann, ret))

inferCon ret c@(GeneralisedCon nm cty ann) = do

  cty <- resolveKind (BecauseOf c) cty
  var <- TvName <$> fresh
  case solve 1 (Seq.singleton (ConUnify (BecauseOf c) var (gadtConResult cty) ret)) of
    Left e -> throwError (gadtConShape (cty, ret) (gadtConResult cty) e)
    Right _ -> pure ()

  let generalise (TyForall v k t) = TyForall v k <$> generalise t
      generalise (TyArr a t) = TyArr a <$> generalise t
      generalise ty = case solve 1 (Seq.singleton (ConUnify (BecauseOf c) var ret ty)) of
        Right (x, _) -> do
          tell (map (\(x, y) -> (TyVar x, y)) (Map.toAscList x))
          pure ret
        Left e -> throwError e

  (cty, cons) <- runWriterT (generalise cty)
  let (sub, keep) = partitionEithers (map (uncurry simplify) cons)
  overall <- closeOverGadt (BecauseOf c) keep (apply (mconcat sub) cty)
  pure ((TvName nm, overall), GeneralisedCon (TvName nm) overall (ann, overall))

closeOverGadt :: MonadInfer Typed m => SomeReason -> [(Type Typed, Type Typed)] -> Type Typed -> m (Type Typed)
closeOverGadt r cons cty =
  let fv = ftv cty `Set.union` foldMap (\(x, y) -> ftv x `Set.union` ftv y) cons
      pushCons [] t = t
      pushCons c (TyForall v k t) = TyForall v k (pushCons c t)
      pushCons c t = TyWithConstraints c t

      forall [] t = t
      forall xs t = foldr (TyPi . flip Implicit Nothing) t xs
   in annotateKind r $ if Set.null fv
         then pushCons cons cty
         else pushCons cons (forall (Set.toList fv) cty)


-- A bit of a hack for better aesthetics
simplify :: Type Typed -> Type Typed -> Either (Map.Map (Var Typed) (Type Typed)) (Type Typed, Type Typed)
simplify (TyVar v@(TvName (TgName x _))) b@(TyVar (TvName (TgName y _)))
  | x == y = Left (Map.singleton v b)
simplify x y = Right (x, y)
