{-# LANGUAGE OverloadedStrings, FlexibleContexts, RankNTypes,
   StandaloneDeriving, GADTs, UndecidableInstances,
   DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Types.Infer.Builtin where

import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Set as Set
import Data.Spanned
import Data.Reason
import Data.Maybe
import Data.Data

import Control.Monad.Infer
import Control.Lens

import Syntax.Transform
import Syntax.Types
import Syntax.Var
import Syntax

import Text.Pretty.Semantic

tyUnit, tyBool, tyInt, tyString, tyFloat, tyLazy :: Type Typed
tyInt = TyCon (TvName (TgInternal "int"))
tyString = TyCon (TvName (TgInternal "string"))
tyBool = TyCon (TvName (TgInternal "bool"))
tyUnit = TyCon (TvName (TgInternal "unit"))
tyFloat = TyCon (TvName (TgInternal "float"))
tyLazy = TyCon (TvName (TgName "lazy" (-34)))

builtinNames :: Set.Set (Var Typed)
builtinNames = Set.fromList . map TvName $ namesInScope (builtinsEnv ^. names)

builtinsEnv :: Env
builtinsEnv = envOf (scopeFromList builtin) where
  op :: T.Text -> Type Typed -> (Var Resolved, Type Typed)
  op x t = (TgInternal x, t)
  tp :: T.Text -> (Var Resolved, Type Typed)
  tp x = (TgInternal x, TyType)

  intOp = tyInt `TyArr` (tyInt `TyArr` tyInt)
  floatOp = tyFloat `TyArr` (tyFloat `TyArr` tyFloat)
  stringOp = tyString `TyArr` (tyString `TyArr` tyString)
  intCmp = tyInt `TyArr` (tyInt `TyArr` tyBool)
  floatCmp = tyInt `TyArr` (tyInt `TyArr` tyBool)

  cmp = TyForall name (Just TyType) $ TyVar name `TyArr` (TyVar name `TyArr` tyBool)
    where name = TvName (TgInternal "a")-- TODO: This should use TvName/TvFresh instead
  builtin
    = [ op "+" intOp, op "-" intOp, op "*" intOp, op "/" (tyInt `TyArr` (tyInt `TyArr` tyFloat)), op "**" intOp
      , op "+." floatOp, op "-." floatOp, op "*." floatOp, op "/." floatOp, op "**." floatOp
      , op "^" stringOp
      , op "<" intCmp, op ">" intCmp, op ">=" intCmp, op "<=" intCmp
      , op "<." floatCmp, op ">." floatCmp, op ">=." floatCmp, op "<=." floatCmp
      , op "==" cmp, op "<>" cmp
      , op "@@" (TyForall a (Just TyType) $ TyForall b (Just TyType) $ (TyVar a `TyArr` TyVar b) `TyArr` (TyVar a `TyArr` TyVar b))
      , (TgInternal "lazy", TyForall a (Just TyType) $ (tyUnit `TyArr` TyVar a) `TyArr` TyApp tyLazy (TyVar a))
      , (TgInternal "force", TyForall a (Just TyType) $ TyApp tyLazy (TyVar a) `TyArr` TyVar a)
      , tp "int", tp "string", tp "bool", tp "unit", tp "float", (TgName "lazy" (-34), TyArr TyType TyType)
      ]
    where a = TvName (TgInternal "a")
          b = TvName (TgInternal "b")

unify, subsumes :: ( MonadInfer Typed m )
                => SomeReason
                -> Type Typed
                -> Type Typed -> m (Wrapper Typed)
unify e a b = do
  x <- TvName <$> genName
  tell (Seq.singleton (ConUnify e x a b))
  pure (WrapVar x)

subsumes e a b = do
  x <- TvName <$> genName
  tell (Seq.singleton (ConSubsume e x a b))
  pure (WrapVar x)

implies :: ( Reasonable f p
           , MonadInfer Typed m
           )
        => f p
        -> Type Typed -> [(Type Typed, Type Typed)]
        -> m a
        -> m a
implies _ _ [] k = k
implies e t cs k = do
  vs <- replicateM (length cs) genName
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
leakEqualities r ((a, b):xs) = unify (BecauseOf r) a b *> leakEqualities r xs
leakEqualities _ [] = pure ()

decompose :: MonadInfer Typed m
          => SomeReason
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
      k <- subsumes r t (p # (a, b))
      pure (a, b, \x -> ExprWrapper k x (annotation x, p # (a, b)))

-- | Get the first /visible/ 'TyBinder' in this 'Type', possibly
-- instantiating 'TyForall's and discharging 'Implicit' binders.
quantifier :: MonadInfer Typed m
           => SomeReason
           -> SkipImplicit
           -> Type Typed
           -> m (TyBinder Typed, Type Typed, Expr Typed -> Expr Typed)
quantifier r s ty@TyForall{} = do
  (k', _, t) <- instantiate Expression ty
  (a, b, k) <- quantifier r s t
  pure (a, b, fromMaybe id k' . k)
quantifier _ _ (TyPi x b) = pure (x, b, id)
quantifier r _ t = do
  (a, b) <- (,) <$> freshTV <*> freshTV
  k <- subsumes r t (TyPi (Anon a) b)
  pure (Anon a, b, \x -> ExprWrapper k x (annotation x, TyPi (Anon a) b))

discharge :: (Reasonable f p, MonadInfer Typed m)
          => f p
          -> Type Typed
          -> m (Type Typed, Expr Typed -> Expr Typed)
discharge r (TyWithConstraints cs tau) = leakEqualities r cs *> discharge r tau
discharge _ t = pure (t, id)

rereason :: SomeReason -> Seq.Seq (Constraint p) -> Seq.Seq (Constraint p)
rereason because = fmap go where
  go (ConUnify _ v l r) = ConUnify because v l r
  go (ConSubsume _ v l r) = ConSubsume because v l r
  go (ConImplies _ u b a) = ConImplies because u b a
  go x@ConFail{} = x
  go x@DeferredError{} = x

litTy :: Lit -> Type Typed
litTy LiInt{} = tyInt
litTy LiStr{} = tyString
litTy LiBool{} = tyBool
litTy LiUnit{} = tyUnit
litTy LiFloat{} = tyFloat

killWildcard :: Type Typed -> Type Typed
killWildcard = transformType go where
  go (TyWildcard (Just tau)) = killWildcard tau
  go x = x

-- A representation of an individual 'match' arm, for blaming type
-- errors on:

data SkipImplicit = DoSkip | Don'tSkip
  deriving (Eq, Show, Ord)

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

getHead :: Type p -> Type p
getHead t@TyVar{} = t
getHead t@TyCon{} = t
getHead t@TyPromotedCon{} = t
getHead t@TyApp{} = t
getHead x@(TyPi b t) = case b of
  Invisible{} -> getHead t
  _ -> x
getHead t@TyRows{} = t
getHead t@TyExactRows{} = t
getHead t@TyTuple{} = t
getHead t@TySkol{} = t
getHead t@TyWithConstraints{} = t
getHead t@TyType = t
getHead t@TyWildcard{} = t

spine :: Type p -> [Type p]
spine (TyApp f x) = spine f `snoc` x
spine t = [t]
