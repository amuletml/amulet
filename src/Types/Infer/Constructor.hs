{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
module Types.Infer.Constructor (inferCon) where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Either

import Control.Monad.Infer

import Types.Kinds
import Types.Infer.Builtin
import Types.Infer.Errors
import Types.Unify

import Syntax.Subst
import Syntax.Var
import Syntax

inferCon :: MonadInfer Typed m
         => Type Typed
         -> Constructor Desugared
         -> m ( (Var Typed, Type Typed)
              , Constructor Typed)

inferCon ret con@(ArgCon ac nm t ann) = do
  checkWildcard con t
  ty' <- resolveKind (BecauseOf con) t
  res <- closeOver (BecauseOf con) $ TyArr ty' ret
  pure ((nm, res), ArgCon ac nm ty' (ann, res))

inferCon ret' con@(UnitCon ac nm ann) = do
  ret <- closeOver (BecauseOf con) ret'
  pure ((nm, ret), UnitCon ac nm (ann, ret))

inferCon ret c@(GadtCon ac nm cty ann) = do
  checkWildcard c cty
  cty <- condemn $ resolveKind (BecauseOf c) cty
  var <- genName
  _ <- condemn . retcons (gadtConShape (cty, ret) (gadtConResult cty)) $
    solve (Seq.singleton (ConUnify (BecauseOf c) var (gadtConResult cty) ret))

  let generalise (TyPi q t) = TyPi q <$> generalise t
      generalise ty = do
        ~(sub, _, []) <- condemn $ solve (Seq.singleton (ConUnify (BecauseOf c) var ret ty))
        tell (map (\(x, y) -> (TyVar x, y)) (Map.toAscList sub))
        pure ret

  (cty, cons) <- runWriterT (generalise cty)
  let (sub, keep) = partitionEithers (map (uncurry simplify) cons)
  overall <- closeOverGadt (BecauseOf c) keep (apply (mconcat sub) cty)
  pure ((nm, overall), GadtCon ac nm overall (ann, overall))

closeOverGadt :: MonadInfer Typed m => SomeReason -> [(Type Typed, Type Typed)] -> Type Typed -> m (Type Typed)
closeOverGadt r cons cty =
  let fv = ftv cty `Set.union` foldMap (\(x, y) -> ftv x `Set.union` ftv y) cons
      pushCons [] t = t
      pushCons c (TyForall v k t) = TyForall v k (pushCons c t)
      pushCons c t = TyWithConstraints c t

      forall [] t = t
      forall xs t = foldr (TyPi . flip Invisible Nothing) t xs
   in annotateKind r $ if Set.null fv
         then pushCons cons cty
         else pushCons cons (forall (Set.toList fv) cty)


-- A bit of a hack for better aesthetics
simplify :: Type Typed -> Type Typed -> Either (Map.Map (Var Typed) (Type Typed)) (Type Typed, Type Typed)
simplify (TyVar v@(TgName x _)) b@(TyVar (TgName y _))
  | x == y = Left (Map.singleton v b)
simplify x y = Right (x, y)
