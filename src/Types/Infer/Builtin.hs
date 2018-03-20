{-# LANGUAGE OverloadedStrings, FlexibleContexts, RankNTypes, StandaloneDeriving, GADTs, UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Types.Infer.Builtin where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Semigroup
import Data.Spanned
import Data.Data

import Control.Monad.Infer.Error
import Control.Monad.Infer
import Control.Lens

import Types.Wellformed

import Syntax.Subst
import Syntax

import Pretty

tyUnit, tyBool, tyInt, tyString, tyFloat :: Type Typed
tyInt = TyCon (TvName (TgInternal "int"))
tyString = TyCon (TvName (TgInternal "string"))
tyBool = TyCon (TvName (TgInternal "bool"))
tyUnit = TyCon (TvName (TgInternal "unit"))
tyFloat = TyCon (TvName (TgInternal "float"))

builtinsEnv :: Env
builtinsEnv = Env (Map.fromList ops) (Map.fromList tps) mempty where
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
                -> Type Typed -> m (Type Typed, Coercion Typed)
unify e a b = do
  x <- TvName <$> fresh
  tell [ConUnify (BecauseOf e) x a b]
  pure (b, VarCo x)
subsumes e a b = do
  x <- TvName <$> fresh
  tell [ConSubsume (BecauseOf e) x a b]
  pure (b, VarCo x)

implies :: ( Reasonable f p
           , MonadInfer Typed m
           )
        => f p
        -> Type Typed -> [(Type Typed, Type Typed)]
        -> m a
        -> m a
implies _ _ [] k = k
implies e t cs k = do
  vs <- replicateM (length cs) fresh
  let eqToCon v (a, b) = ConUnify (BecauseOf e) (TvName v) a b
      eqToCons = zipWith eqToCon vs

      wrap :: [Constraint Typed] -> [Constraint Typed]
      wrap = (:[]) . ConImplies (BecauseOf e) t (eqToCons cs)
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

-- A representation of an individual 'match' arm, for blaming type
-- errors on:

data Arm p
  = Arm { armPat :: Pattern p
        , armExp :: Expr p
        }

deriving instance (Eq (Var p), Eq (Ann p)) => Eq (Arm p)
deriving instance (Show (Var p), Show (Ann p)) => Show (Arm p)
deriving instance (Ord (Var p), Ord (Ann p)) => Ord (Arm p)
deriving instance (Data p, Typeable p, Data (Var p), Data (Ann p)) => Data (Arm p)

instance (Spanned (Expr p), Spanned (Pattern p)) => Spanned (Arm p) where
  annotation (Arm p e) = annotation p <> annotation e

instance Pretty (Var p) => Pretty (Arm p) where
  pretty (Arm p e) = pipe <+> pretty p <+> arrow <+> pretty e

instance (Pretty (Var p), Reasonable Pattern p, Reasonable Expr p) => Reasonable Arm p where
  blame _ = string "the pattern-matching clause"
