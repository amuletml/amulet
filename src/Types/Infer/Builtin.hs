{-# LANGUAGE OverloadedStrings, FlexibleContexts, RankNTypes, StandaloneDeriving, GADTs, UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Types.Infer.Builtin where

import qualified Data.Sequence as Seq
import qualified Data.Text as T
import Data.Spanned
import Data.Reason
import Data.Maybe
import Data.Data

import Control.Monad.Infer
import Control.Lens

import Syntax.Types
import Syntax

import Text.Pretty.Semantic

tyUnit, tyBool, tyInt, tyString, tyFloat, tyLazy :: Type Typed
tyInt = TyCon (TvName (TgInternal "int"))
tyString = TyCon (TvName (TgInternal "string"))
tyBool = TyCon (TvName (TgInternal "bool"))
tyUnit = TyCon (TvName (TgInternal "unit"))
tyFloat = TyCon (TvName (TgInternal "float"))
tyLazy = TyCon (TvName (TgName "lazy" (-34)))

builtinsEnv :: Env
builtinsEnv = envOf (scopeFromList builtin) where
  op :: T.Text -> Type Typed -> (Var Resolved, Type Typed)
  op x t = (TgInternal x, t)
  tp :: T.Text -> (Var Resolved, Type Typed)
  tp x = (TgInternal x, TyType)

  boolOp = tyBool `TyArr` (tyBool `TyArr` tyBool)
  intOp = tyInt `TyArr` (tyInt `TyArr` tyInt)
  floatOp = tyFloat `TyArr` (tyFloat `TyArr` tyFloat)
  stringOp = tyString `TyArr` (tyString `TyArr` tyString)
  intCmp = tyInt `TyArr` (tyInt `TyArr` tyBool)
  floatCmp = tyInt `TyArr` (tyInt `TyArr` tyBool)

  cmp = TyForall name (Just TyType) $ TyVar name `TyArr` (TyVar name `TyArr` tyBool)
    where name = TvName (TgInternal "a")-- TODO: This should use TvName/TvFresh instead
  builtin
    = [ op "+" intOp, op "-" intOp, op "*" intOp, op "/" intOp, op "**" intOp
      , op "+." floatOp, op "-." floatOp, op "*." floatOp, op "/." floatOp, op "**." floatOp
      , op "^" stringOp
      , op "<" intCmp, op ">" intCmp, op ">=" intCmp, op "<=" intCmp
      , op "<." floatCmp, op ">." floatCmp, op ">=." floatCmp, op "<=." floatCmp
      , op "==" cmp, op "<>" cmp
      , op "||" boolOp, op "&&" boolOp
      , (TgInternal "lazy", TyForall a (Just TyType) $ (tyUnit `TyArr` TyVar a) `TyArr` (TyApp tyLazy (TyVar a)))
      , (TgInternal "force", TyForall a (Just TyType) $ (TyApp tyLazy (TyVar a)) `TyArr` TyVar a)
      , tp "int", tp "string", tp "bool", tp "unit", tp "float", (TgName "lazy" (-34), TyArr TyType TyType) 
      ]
    where a = TvName (TgInternal "a")

unify, subsumes :: ( Reasonable f p
                   , MonadInfer Typed m )
                => f p
                -> Type Typed
                -> Type Typed -> m (Type Typed, Wrapper Typed)
unify e a b = do
  x <- TvName <$> fresh
  tell (Seq.singleton (ConUnify (BecauseOf e) x a b))
  pure (b, WrapVar x)
subsumes e a b = do
  x <- TvName <$> fresh
  tell (Seq.singleton (ConSubsume (BecauseOf e) x a b))
  pure (b, WrapVar x)

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

      wrap :: Seq.Seq (Constraint Typed) -> Seq.Seq (Constraint Typed)
      wrap = Seq.singleton . ConImplies (BecauseOf e) t (Seq.fromList (eqToCons cs))
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
  (k', _, t) <- instantiate Expression ty
  (a, b, k) <- decompose r p t
  let cont = fromMaybe id k'
  pure (a, b, cont . k)
decompose r p t =
  case t ^? p of
    Just (a, b) -> pure (a, b, id)
    Nothing -> do
      (a, b) <- (,) <$> freshTV <*> freshTV
      (_, k) <- subsumes r t (p # (a, b))
      pure (a, b, \x -> ExprWrapper k x (annotation x, p # (a, b)))

quantifier :: (Reasonable f p, MonadInfer Typed m)
           => f p
           -> Type Typed
           -> m (TyBinder Typed, Type Typed, Expr Typed -> Expr Typed)
quantifier r ty@TyForall{} = do
  (k', _, t) <- instantiate Expression ty
  (a, b, k) <- quantifier r t
  pure (a, b, fromMaybe id k' . k)
quantifier _ (TyPi x b) = pure (x, b, id)
quantifier r t = do
  (a, b) <- (,) <$> freshTV <*> freshTV
  (_, k) <- subsumes r t (TyPi (Anon a) b)
  pure (Anon a, b, \x -> ExprWrapper k x (annotation x, TyPi (Anon a) b))

discharge :: (Reasonable f p, MonadInfer Typed m)
          => f p
          -> Type Typed
          -> m (Type Typed)
discharge r (TyWithConstraints cs tau) = leakEqualities r cs *> discharge r tau
discharge _ t = pure t

litTy :: Lit -> Type Typed
litTy LiInt{} = tyInt
litTy LiStr{} = tyString
litTy LiBool{} = tyBool
litTy LiUnit{} = tyUnit
litTy LiFloat{} = tyFloat

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

gadtConResult :: Type p -> Type p
gadtConResult (TyForall _ _ t) = gadtConResult t
gadtConResult (TyPi Explicit{} t) = gadtConResult t
gadtConResult (TyArr _ t) = t
gadtConResult t = t

forceName, lAZYName :: Var Typed
lAZYName = TvName (TgName "lazy" (-35))
forceName = TvName (TgName "force" (-36))

forceTy, lAZYTy :: Type Typed
forceTy = TyForall (TvName (TgName "a" (-30))) (Just TyType) (forceTy' (TyVar (TvName (TgName "a" (-30)))))
lAZYTy = TyForall (TvName (TgName "a" (-30))) (Just TyType) (lAZYTy' (TyVar (TvName (TgName "a" (-30)))))

forceTy', lAZYTy' :: Type Typed -> Type Typed
forceTy' x = TyArr (TyApp tyLazy x) x
lAZYTy' x = TyArr (TyArr tyUnit x) (TyApp tyLazy x)
