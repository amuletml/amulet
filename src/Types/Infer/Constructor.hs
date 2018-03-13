{-# LANGUAGE FlexibleContexts, BangPatterns #-}
module Types.Infer.Constructor (inferCon) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Control.Monad.Infer

import Types.Infer.Builtin
import Types.Unify
import Types.Kinds

import Syntax.Subst
import Syntax

import Pretty

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
  let result (TyForall _ t) = result t
      result (TyArr _ t) = t
      result t = t

  (cty, _) <- resolveKind cty
  case solve 1 mempty [ConUnify (BecauseOf c) (result cty) ret] of
    Left e -> throwError (gadtConShape (cty, ret) (result cty) e)
    Right _ -> pure ()

  let generalise (TyForall v t) = TyForall v <$> generalise t
      generalise (TyArr a t) = TyArr a <$> generalise t
      generalise ty = case solve 1 mempty [ConUnify (BecauseOf c) ret ty] of
        Right x -> do
          tell (map (\(x, y) -> (TyVar x, y)) (Map.toAscList x))
          pure ret
        Left e -> throwError e



  (cty, cons) <- runWriterT (generalise cty)
  let overall = closeOverGadt cons cty
   in pure ((TvName nm, overall), GeneralisedCon (TvName nm) overall (ann, overall))

closeOverGadt :: Ord (Var p) => [(Type p, Type p)] -> Type p -> Type p
closeOverGadt cons cty =
  let fv = ftv cty `Set.union` foldMap (\(x, y) -> ftv x `Set.union` ftv y) cons
      pushCons [] t = t
      pushCons c (TyForall v t) = TyForall v (pushCons c t)
      pushCons c t = TyWithConstraints c t
   in if Set.null fv
         then pushCons cons cty
         else pushCons cons (TyForall (Set.toList fv) cty)

gadtConShape :: (Type Typed, Type Typed) -> Type Typed -> TypeError -> TypeError
gadtConShape (t, _) (TyArr c d) = fix . flip Note (string "Generalised constructors can not be curried") . getErr where
  fix = case t of
    TyArr x _ -> flip Suggestion (string "Perhaps use a tuple:" <+> verbatim (TyArr (TyTuple x c) d))
    TyForall vs (TyArr x _) -> flip Suggestion (string "Perhaps use a tuple:" <+> verbatim (TyForall vs (TyArr (TyTuple x c) d)))
    _ -> id
gadtConShape (_, t) ty = flip Note msg' . flip Note msg . getErr where
  msg = string "The type"
    <+> verbatim ty
    <+> string "is not an instance of the type being declared"
  msg' = case t of
    TyApp{} -> string "It must end in an application like" <+> verbatim t
    TyCon{} -> string "It must end in a reference to" <+> pretty t
    _ -> error "malformed"

getErr :: TypeError -> TypeError
getErr (ArisingFrom e _) = getErr e
getErr x = x
