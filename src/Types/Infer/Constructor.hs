{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
module Types.Infer.Constructor (inferCon) where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Either

import Control.Monad.Infer
import Control.Lens

import Types.Kinds
import Types.Infer.Builtin
import Types.Infer.Errors
import Types.Unify

import Syntax.Subst
import Syntax.Types
import Syntax.Var
import Syntax

inferCon :: MonadInfer Typed m
         => Set.Set (Var Typed)
         -> Type Typed
         -> Constructor Desugared
         -> m ( (Var Typed, Type Typed)
              , Constructor Typed)

inferCon vars ret con@(ArgCon ac nm t ann) = do
  checkWildcard con t
  ty' <- expandType =<< resolveKind (BecauseOf con) t
  res <- closeOver' vars (BecauseOf con) $ TyArr ty' ret
  pure ((nm, res), ArgCon ac nm ty' (ann, res))

inferCon vars ret' con@(UnitCon ac nm ann) = do
  ret <- closeOver' vars (BecauseOf con) ret'
  pure ((nm, ret), UnitCon ac nm (ann, ret))

inferCon vars ret c@(GadtCon ac nm cty ann) = do
  checkWildcard c cty
  cty <- condemn $ resolveKind (BecauseOf c) cty
  var <- genName
  when (countAnon cty > 1) $ dictates (gadtConManyArgs c)

  let generalise (TyPi q t) = TyPi q <$> generalise t
      generalise ty = do
        ~(sub, _, []) <- condemn $ solve (Seq.singleton (ConUnify (BecauseOf c) mempty var ret ty)) =<< view classDecs
        tell (map (\(x, y) -> (TyVar x, y)) (Map.toAscList sub))
        pure ret

  (cty, cons) <- runWriterT (generalise cty)
  let (sub, keep) = partitionEithers (map (uncurry simplify) cons)
  overall <- expandType =<< closeOverGadt vars (BecauseOf c) keep (apply (mconcat sub) cty)
  pure ((nm, overall), GadtCon ac nm overall (ann, overall))

closeOverGadt :: MonadInfer Typed m => Set.Set (Var Typed) -> SomeReason -> [(Type Typed, Type Typed)] -> Type Typed -> m (Type Typed)
closeOverGadt cbv r cons cty = do
  let fv = ftv cty `Set.union` foldMap (\(x, y) -> ftv x `Set.union` ftv y) cons
      fv :: Set.Set (Var Typed)
      vars = cbv `Set.union` ftv cty

      pushCons [] t = t
      pushCons c (TyPi (Invisible v k vis) t) = TyPi (Invisible v k vis) (pushCons c t)
      pushCons c t = TyWithConstraints c t

      forall [] t = t
      forall xs t = foldr (\var -> TyPi (Invisible var Nothing (vis var))) t xs

      vis v
        | v `Set.member` vars = Spec
        | otherwise = Infer

  ty <- annotateKind r $
    if Set.null fv
       then pushCons cons cty
       else pushCons cons (forall (Set.toList fv) cty)

  let fv = ftv ty
      forall [] t = t
      forall xs t = foldr (\var -> TyPi (Invisible var (Just TyType) (vis var))) t xs
  pure (forall (Set.toList fv) ty)


-- A bit of a hack for better aesthetics
simplify :: Type Typed -> Type Typed -> Either (Map.Map (Var Typed) (Type Typed)) (Type Typed, Type Typed)
simplify (TyVar v@(TgName x _)) b@(TyVar (TgName y _))
  | x == y = Left (Map.singleton v b)
simplify x y = Right (x, y)

countAnon :: Type p -> Int
countAnon (TyPi Anon{} t) = 1 + countAnon t
countAnon (TyPi _ t) = countAnon t
countAnon _ = 0
