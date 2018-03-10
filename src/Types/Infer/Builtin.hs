{-# LANGUAGE OverloadedStrings, FlexibleContexts, RankNTypes #-}
module Types.Infer.Builtin where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Spanned

import Control.Monad.Infer
import Control.Lens

import Types.Wellformed

import Syntax.Subst
import Syntax

tyUnit, tyBool, tyInt, tyString, tyFloat :: Type Typed
tyInt = TyCon (TvName (TgInternal "int"))
tyString = TyCon (TvName (TgInternal "string"))
tyBool = TyCon (TvName (TgInternal "bool"))
tyUnit = TyCon (TvName (TgInternal "unit"))
tyFloat = TyCon (TvName (TgInternal "float"))

builtinsEnv :: Env
builtinsEnv = Env (Map.fromList ops) (Map.fromList tps) where
  op :: T.Text -> Type Typed -> (Var Resolved, Type Typed)
  op x t = (TgInternal x, t)
  tp :: T.Text -> (Var Resolved, Kind Typed)
  tp x = (TgInternal x, KiStar)

  boolOp = tyBool `TyArr` (tyBool `TyArr` tyBool)
  intOp = tyInt `TyArr` (tyInt `TyArr` tyInt)
  floatOp = tyFloat `TyArr` (tyFloat `TyArr` tyFloat)
  stringOp = tyString `TyArr` (tyString `TyArr` tyString)
  intCmp = tyInt `TyArr` (tyInt `TyArr` tyBool)
  floatCmp = tyInt `TyArr` (tyInt `TyArr` tyBool)

  cmp = TyForall [name] $ TyVar name `TyArr` (TyVar name `TyArr` tyBool)
    where name = TvName (TgInternal "a")-- TODO: This should use TvName/TvFresh instead
  ops = [ op "+" intOp, op "-" intOp, op "*" intOp, op "/" intOp, op "**" intOp
        , op "+." floatOp, op "-." floatOp, op "*." floatOp, op "/." floatOp, op "**." floatOp
        , op "^" stringOp
        , op "<" intCmp, op ">" intCmp, op ">=" intCmp, op "<=" intCmp
        , op "<." floatCmp, op ">." floatCmp, op ">=." floatCmp, op "<=." floatCmp
        , op "==" cmp, op "<>" cmp
        , op "||" boolOp, op "&&" boolOp ]
  tps :: [(Var Resolved, Kind Typed)]
  tps = [ tp "int", tp "string", tp "bool", tp "unit" ]

unify, subsumes :: ( Reasonable f p
                   , MonadInfer Typed m )
                => f p
                -> Type Typed
                -> Type Typed -> m (Type Typed)
unify e a b = do
  tell [ConUnify (BecauseOf e) a b]
  pure b
subsumes e a b = do
  tell [ConSubsume (BecauseOf e) a b]
  pure b

implies :: ( Reasonable f p
           , MonadInfer Typed m
           )
        => f p
        -> [(Type Typed, Type Typed)]
        -> m a
        -> m a
implies _ [] k = k
implies e cs k =
  let eqToCon (a, b) = ConUnify (BecauseOf e) a b
      eqToCons = map eqToCon
      wrap :: [Constraint Typed] -> [Constraint Typed]
      wrap = (:[]) . ConImplies (BecauseOf e) (eqToCons cs)
   in censor wrap k

leakEqualities :: ( Reasonable f p
                  , MonadInfer Typed m
                  )
               => f p
               -> [(Type Typed, Type Typed)]
               -> m ()
leakEqualities r ((a, b):xs) = unify r a b *> leakEqualities r xs
leakEqualities _ [] = pure ()

decompose :: ( Reasonable f p
             , MonadInfer Typed m )
          => f p
          -> Prism' (Type Typed) (Type Typed, Type Typed)
          -> Type Typed
          -> m (Type Typed, Type Typed, Expr Typed -> Expr Typed)
decompose r p ty@TyForall{} = do
  (s, _, t) <- instantiate ty
  (a, b, k) <- decompose r p t
  let new (TyForall (x:xs) t) = new (TyForall xs t) . 
        \e -> TypeApp e (s Map.! x) (annotation r, normType t)
      new (TyForall [] _) = id
      new _ = error "impossible instantiation in definitely-polymorphic decomposition"
  pure (a, b, new ty . k)
decompose r p t =
  case t ^? p of
    Just (a, b) -> pure (a, b, id)
    Nothing -> do
      (a, b) <- (,) <$> freshTV <*> freshTV
      _ <- subsumes r t (p # (a, b))
      pure (a, b, id)

closeOver :: Type Typed -> Type Typed
closeOver a = normType $ forall (fv a) a where
  fv = Set.toList . ftv
  forall :: [Var p] -> Type p -> Type p
  forall [] a = a
  forall vs a = TyForall vs a
