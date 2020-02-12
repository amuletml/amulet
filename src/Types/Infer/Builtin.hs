{-# LANGUAGE FlexibleContexts, RankNTypes,
   GADTs, UndecidableInstances, FlexibleInstances,
   MultiParamTypeClasses #-}
module Types.Infer.Builtin where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Foldable
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
  a <- expandType a
  b <- expandType b

  x <- genName
  r <- view classes
  tell (Seq.singleton (ConUnify e r x a b))
  pure (WrapVar x)

subsumes e a b = do
  a <- expandType a
  b <- expandType b

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
  c <- view classes
  vs <- replicateM (length cs) genName
  let eqToCon v (a, b) = ConUnify (BecauseOf e) c v a b
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
leakEqualities r ((a, b):xs) = do
  x <- genName
  i <- view classes
  tell (Seq.singleton (ConImplicit (BecauseOf r) i x (TyApps tyEq [a, b])))
  leakEqualities r xs
leakEqualities _ [] = pure ()

decompose :: MonadInfer Typed m
          => SomeReason
          -> Prism' (Type Typed) (Type Typed, Type Typed)
          -> Type Typed
          -> m (Type Typed, Type Typed, Expr Typed -> Expr Typed)
decompose r p (TyForall v _ rest) = do
  var <- refreshTV v
  let map = Map.singleton v var
      exp ex | an <- spanOf ex =
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
  let wrap ex = ExprWrapper (WrapVar x) (ExprWrapper (TypeAsc ty) ex (spanOf ex, ty)) (spanOf ex, rest)
  pure (a, b, k . wrap)
decompose r p t =
  case t ^? p of
    Just (a, b) -> pure (a, b, id)
    Nothing -> do
      (a, b) <- (,) <$> freshTV <*> freshTV
      k <- subsumes r t (p # (a, b))
      pure (a, b, \x -> ExprWrapper k x (spanOf x, p # (a, b)))

-- | Get the first /visible/ 'TyBinder' in this 'Type', possibly
-- instantiating 'TyForall's and discharging 'Implicit' binders.
quantifier :: MonadInfer Typed m
           => SomeReason
           -> (Specificity -> Bool) -- The quantifiers we can skip over.
           -> Type Typed
           -> m (TyBinder Typed, Type Typed, Expr Typed -> Expr Typed)

quantifier r pred (TyPi (Invisible v _ req) rest) | pred req = do
  var <- refreshTV v
  let map = Map.singleton v var
      exp ex | an <- spanOf ex =
        ExprWrapper (TypeApp var) ex (an, apply map rest)
  (a, b, k) <- quantifier r pred (apply map rest)
  pure (a, b, k . exp)

quantifier r pred ty@TyWithConstraints{} = do
  (rest, k) <- discharge (Const r) ty
  (a, b, k') <- quantifier r pred rest
  pure (a, b, k' . k)

quantifier r pred wty@(TyPi (Implicit tau) sigma) = do
  x <- genName
  i <- view classes
  tell (Seq.singleton (ConImplicit r i x tau))

  (dom, cod, k) <- quantifier r pred sigma
  let wrap ex = ExprWrapper (WrapVar x) (ExprWrapper (TypeAsc wty) ex (spanOf ex, wty)) (spanOf ex, sigma)
  pure (dom, cod, k . wrap)

quantifier _ _ (TyPi x b) = pure (x, b, id)
quantifier _ _ (TyApp (TyApp (TyCon n ()) l) r) | n == tyArrowName = pure (Anon l, r, id)
quantifier r _ t = do
  (a, b) <- (,) <$> freshTV <*> freshTV
  k <- subsumes r t (TyPi (Anon a) b)
  pure (Anon a, b, \x -> ExprWrapper k x (spanOf x, TyPi (Anon a) b))

discharge :: (Reasonable f p, MonadInfer Typed m)
          => f p
          -> Type Typed
          -> m (Type Typed, Expr Typed -> Expr Typed)
discharge r (TyWithConstraints cs tau) = leakEqualities r cs *> discharge r tau
discharge _ t = pure (t, id)

rereason :: SomeReason -> Seq.Seq (Constraint p) -> Seq.Seq (Constraint p)
rereason because = fmap go where
  go (ConUnify _ c v l r) = ConUnify because c v l r
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

checkWildcard :: MonadChronicles TypeError m => Reasonable f p => f p -> Type p -> m ()
checkWildcard _ TyType{} = pure ()
checkWildcard _ TyLit{} = pure ()
checkWildcard _ TyCon{} = pure ()
checkWildcard _ TyVar{} = pure ()
checkWildcard _ TyPromotedCon{} = pure ()
checkWildcard _ TySkol{} = pure ()
checkWildcard _ TyWithConstraints{} = pure ()
checkWildcard e TyWildcard{} = confesses (WildcardNotAllowed (BecauseOf e))
checkWildcard e (TyApp f x) = checkWildcard e f *> checkWildcard e x
checkWildcard e (TyPi b x) = flip (*>) (checkWildcard e x) $
  case b of
    Anon t -> checkWildcard e t
    Implicit t -> checkWildcard e t
    Invisible _ t _ -> traverse_ (checkWildcard e) t
checkWildcard e (TyRows t rs) = checkWildcard e t *> traverse_ (checkWildcard e . snd) rs
checkWildcard e (TyExactRows rs) = traverse_ (checkWildcard e . snd) rs
checkWildcard e (TyTuple a b) = checkWildcard e a *> checkWildcard e b
checkWildcard e (TyTupleL a b) = checkWildcard e a *> checkWildcard e b
checkWildcard e (TyOperator l _ r) = checkWildcard e l *> checkWildcard e r
checkWildcard e (TyParens t) = checkWildcard e t

data SkipImplicit = DoSkip | Don'tSkip
  deriving (Eq, Show, Ord)

gadtConResult :: Type p -> Type p
gadtConResult (TyPi _ t) = gadtConResult t
gadtConResult t = t

getHead :: Type p -> Type p
getHead t@TyVar{} = t
getHead t@TyCon{} = t
getHead t@TyLit{} = t
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
getHead t@TyTupleL{} = t
getHead (TyParens t) = getHead t

expandTypeWith :: TySyms -> Type Typed -> Type Typed
expandTypeWith syms t@TyApp{}
  | (TyCon v ():xs) <- appsView t =
    case syms ^. at v of
      Just t@TySymInfo{} ->
        let exp = map (expandTypeWith syms) xs
            sub = Map.fromList $ zip (t ^. tsArgs) exp
            rest = drop (length (t ^. tsArgs)) exp
         in foldl TyApp (expandTypeWith syms (apply sub (t ^?! tsExpansion))) rest
      _ -> foldl TyApp (TyCon v ()) (map (expandTypeWith syms) xs)
  | (x:xs) <- appsView t = foldl TyApp (expandTypeWith syms x) (map (expandTypeWith syms) xs)
  | otherwise = undefined
expandTypeWith syms (TyCon v ()) =
  case syms ^. at v of
    Just t@TySymInfo{} -> expandTypeWith syms (t ^?! tsExpansion)
    _ -> TyCon v ()

expandTypeWith syms (TyOperator l o r) = expandTypeWith syms (TyApp (TyApp o l) r)

expandTypeWith _ x@TyVar{} = x
expandTypeWith _ x@TyPromotedCon{} = x
expandTypeWith _ x@TySkol{} = x
expandTypeWith _ x@TyLit{} = x

expandTypeWith s (TyTuple a b) = TyTuple (expandTypeWith s a) (expandTypeWith s b)
expandTypeWith s (TyTupleL a b) = TyTupleL (expandTypeWith s a) (expandTypeWith s b)

expandTypeWith s (TyPi b r) = TyPi b' (expandTypeWith s r) where
  b' = case b of
    Anon t -> Anon (expandTypeWith s t)
    Implicit t -> Implicit (expandTypeWith s t)
    Invisible v k vis -> Invisible v (expandTypeWith s <$> k) vis
expandTypeWith s (TyRows t ts) = TyRows (expandTypeWith s t) (map (_2 %~ expandTypeWith s) ts)
expandTypeWith s (TyExactRows ts) = TyExactRows (map (_2 %~ expandTypeWith s) ts)
expandTypeWith s (TyWildcard t) = TyWildcard (expandTypeWith s <$> t)
expandTypeWith s (TyParens t) = TyParens (expandTypeWith s t)
expandTypeWith s (TyWithConstraints cs t) =
  TyWithConstraints (map ((_1 %~ expandTypeWith s) . (_2 %~ expandTypeWith s)) cs) (expandTypeWith s t)
expandTypeWith _ TyType = TyType

expandType :: MonadReader Env m => Type Typed -> m (Type Typed)
expandType t = expandTypeWith <$> view tySyms <*> pure t
