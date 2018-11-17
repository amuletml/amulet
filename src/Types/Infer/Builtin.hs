{-# LANGUAGE FlexibleContexts, RankNTypes,
   GADTs, UndecidableInstances, FlexibleInstances,
   MultiParamTypeClasses #-}
module Types.Infer.Builtin where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Spanned
import Data.Reason

import Control.Monad.Infer
import Control.Lens

import Syntax.Transform
import Syntax.Builtin
import Syntax.Types
import Syntax.Subst
import Syntax.Var
import Syntax

builtinNames :: Set.Set (Var Typed)
builtinNames = Set.fromList $ namesInScope (builtinEnv ^. names)

unify, subsumes :: ( MonadInfer Typed m )
                => SomeReason
                -> Type Typed
                -> Type Typed -> m (Wrapper Typed)
unify e a b = do
  x <- genName
  tell (Seq.singleton (ConUnify e x a b))
  pure (WrapVar x)

subsumes e a b = do
  x <- genName
  r <- view classes
  tell (Seq.singleton (ConSubsume e r x a b))
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
  let eqToCon v (a, b) = ConUnify (BecauseOf e) v a b
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
decompose r p (TyForall v _ rest) = do
  var <- refreshTV v
  let map = Map.singleton v var
      exp ex | an <- annotation ex =
        ExprWrapper (TypeApp var) ex (an, apply map rest)
  (a, b, k) <- decompose r p rest
  pure (a, b, k . exp)

decompose r p ty@TyWithConstraints{} = do
  (rest, k) <- discharge (Const r) ty
  (a, b, k') <- decompose r p rest
  pure (a, b, k' . k)

decompose r p ty@(TyPi (Implicit cls) rest) = do
  x <- genName
  i <- view classes
  tell (pure (ConImplicit r i x cls))

  (a, b, k) <- decompose r p rest
  let wrap ex = ExprWrapper (WrapVar x) (ExprWrapper (TypeAsc ty) ex (annotation ex, ty)) (annotation ex, rest)
  pure (a, b, k . wrap)
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
           -> Type Typed
           -> m (TyBinder Typed, Type Typed, Expr Typed -> Expr Typed)

quantifier r (TyForall v _ rest) = do
  var <- refreshTV v
  let map = Map.singleton v var
      exp ex | an <- annotation ex =
        ExprWrapper (TypeApp var) ex (an, apply map rest)
  (a, b, k) <- quantifier r (apply map rest)
  pure (a, b, k . exp)

quantifier r ty@TyWithConstraints{} = do
  (rest, k) <- discharge (Const r) ty
  (a, b, k') <- quantifier r rest
  pure (a, b, k' . k)

quantifier r wty@(TyPi (Implicit tau) sigma) = do
  x <- genName
  i <- view classes
  tell (Seq.singleton (ConImplicit r i x tau))

  (dom, cod, k) <- quantifier r sigma
  let wrap ex = ExprWrapper (WrapVar x) (ExprWrapper (TypeAsc wty) ex (annotation ex, wty)) (annotation ex, sigma)
  pure (dom, cod, k . wrap)

quantifier _ (TyPi x b) = pure (x, b, id)
quantifier _ (TyApp (TyApp (TyCon (TgName _ (-38))) l) r) = pure (Anon l, r, id)
quantifier r t = do
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
  go (ConSubsume _ s v l r) = ConSubsume because s v l r
  go (ConImplies _ u b a) = ConImplies because u b a
  go (ConImplicit _ s v t) = ConImplicit because s v t
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

data SkipImplicit = DoSkip | Don'tSkip
  deriving (Eq, Show, Ord)

gadtConResult :: Type p -> Type p
gadtConResult (TyForall _ _ t) = gadtConResult t
gadtConResult (TyArr _ t) = t
gadtConResult t = t

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
getHead t@TyOperator{} = t
getHead (TyParens t) = getHead t

spine :: Type p -> [Type p]
spine (TyApp f x) = spine f `snoc` x
spine t = [t]
