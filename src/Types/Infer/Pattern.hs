{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, ViewPatterns, TupleSections, TypeFamilies #-}
module Types.Infer.Pattern where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

import Control.Monad.Infer
import Control.Lens

import Data.Traversable
import Data.Foldable
import Data.List hiding (insert)

import Types.Kinds
import Types.Infer.Builtin
import Types.Wellformed
import Types.Unify (freshSkol)

import Syntax.Implicits
import Syntax.Builtin
import Syntax.Types
import Syntax.Subst
import Syntax.Var
import Syntax

inferPattern :: MonadInfer Typed m
             => Pattern Desugared
             -> m ( Pattern Typed -- the pattern
                  , Type Typed -- type of what the pattern matches
                  , Telescope Typed -- captures
                  , [(Type Typed, Type Typed)] -- constraints
                  , ImplicitScope ClassInfo Typed
                  )
inferPattern pat@(PType p t ann) = do
  (p', pt, vs, cs, is) <- inferPattern p
  t' <- resolveKind (becausePat pat) t
  let keepPat ConImplicit{} = False
      keepPat _ = True

  _ <- censor (Seq.filter keepPat) $
    subsumes (becausePat pat) t' pt
  case p' of
    Capture v _ -> pure (PType p' t' (ann, t'), t', one v t', cs, is)
    _ -> pure (PType p' t' (ann, t'), t', vs, cs, is)
inferPattern (PRecord rows ann) = do
  rho <- freshTV
  (rowps, rowts, caps, cons, iss) <- unzip5 <$> for rows (\(var, pat) -> do
    (p', t, caps, cs, iss) <- inferPattern pat
    pure ((var, p'), (var, t), caps, cs, iss))
  let ty = TyRows rho rowts
  pure (PRecord rowps (ann, ty), ty, mconcat caps, concat cons, fold iss)
inferPattern (PTuple elems ann) =
  case elems of
    [] -> pure (PLiteral LiUnit (ann, tyUnit), tyUnit, mempty, [], mempty)
    [x] -> inferPattern x
    xs -> do
      (ps, ts, mconcat -> binds, concat -> cons, fold -> iss) <- unzip5 <$> traverse inferPattern xs
      let ty = foldr1 TyTuple ts
      pure (PTuple ps (ann, ty), ty, binds, cons, iss)

inferPattern (PLiteral l ann) = pure (PLiteral l (ann, t), t, mempty, [], mempty)
  where t = litTy l

inferPattern p = do
  x <- freshTV
  (p', vs, cs, is) <- checkPattern p x
  pure (p', x, vs, cs, is)

checkPattern :: MonadInfer Typed m
             => Pattern Desugared
             -> Type Typed
             -> m ( Pattern Typed
                  , Telescope Typed
                  , [(Type Typed, Type Typed)]
                  , ImplicitScope ClassInfo Typed
                  )
checkPattern (Wildcard ann) ty = pure (Wildcard (ann, ty), mempty, [], mempty)
checkPattern (Capture v ann) ty = pure (Capture v (ann, ty), one v ty, [], mempty)
checkPattern (PAs p v ann) ty = do
  (p', b, cs, is) <- checkPattern p ty
  pure (PAs p' v (ann, ty), one v ty <> b, cs, is)
checkPattern pt@(PRecord rows ann) ty = do
  rho <- freshTV
  (rowps, rowts, caps, cons, imps) <- unzip5 <$> for rows (\(var, pat) -> do
    (p', t, caps, cs, imps) <- inferPattern pat
    pure ((var, p'), (var, t), caps, cs, imps))
  co <- unify (becausePat pt) ty (TyRows rho rowts)
  wrapPattern (PRecord rowps, mconcat caps, concat cons, fold imps) (ann, ty) (TyRows rho rowts, co)

checkPattern pat@(PTuple xs ann) ty@TyTuple{} = do
  let go [x] t = do
        (p, bs, cs, is) <- checkPattern x t
        pure ([p], bs, cs, is)
      go (x:xs) ty = do
        (a, b, _) <- decompose (BecauseOf pat) _TyTuple ty
        (p, bs, cs, is) <- checkPattern x a
        (ps, bss, css, iss) <- go xs b
        pure (p:ps, bs <> bss, cs <> css, is <> iss)
      go _ _ = undefined
  (ps, bs, cs, is) <- go xs ty
  pure (PTuple ps (ann, ty), bs, cs, is)

checkPattern pt@(PType p t ann) ty = do
  (p', it, binds, cs, is) <- inferPattern p
  t' <- resolveKind (BecauseOf pt) t
  _ <- subsumes (becausePat pt) t' it
  co <- unify (becausePat pt) ty t'
  wrapPattern (PType p' t', binds, cs, is) (ann, ty) (t', co)


checkPattern pat@(Destructure con Nothing an) target = do
  (con_ty, sub) <- skolGadt con =<< lookupTy' Strong con
  let (cs, classes, rest) = splitConstructorTy con_ty
      skol = Map.keys sub

  (bindings, is) <- makeClassScope an classes

  _ <- unify (becausePat pat) target rest
  pure ( PGadtCon con skol bindings Nothing (an, target), mempty, cs, is)

checkPattern pat@(Destructure con (Just p) an) target = do
  (con_ty, sub) <- skolGadt con =<< lookupTy' Strong con
  let (cs, classes, rest) = splitConstructorTy con_ty
      skol = Map.keys sub

  (tau, cod, _) <- decompose (becausePat pat) _TyArr rest

  (p, tel, cs', is') <- checkPattern p tau
  _ <- unify (becausePat pat) target cod
  (bindings, is) <- makeClassScope an classes

  pure ( PGadtCon con skol bindings (Just p) (an, cod), tel, cs ++ cs', is <> is')

checkPattern pt ty = do
  (p, ty', binds, cs, is) <- inferPattern pt
  _ <- subsumes (becausePat pt) ty ty'
  pure (p, binds, cs, is)

checkParameter :: MonadInfer Typed m
               => Parameter Desugared
               -> Type Typed
               -> m ( Parameter Typed
                    , Telescope Typed
                    , [(Type Typed, Type Typed)]
                    , ImplicitScope ClassInfo Typed
                    )
checkParameter (PatParam p) t = do
  (p, t, cs, is) <- checkPattern p t
  pure (PatParam p, t, cs, is)
checkParameter (EvParam p) t = do
  (p, t, cs, is) <- checkPattern p t
  pure (EvParam p, t, cs, is)

inferParameter :: MonadInfer Typed m
               => Parameter Desugared
               -> m ( Parameter Typed -- the pattern
                    , TyBinder Typed -- binder for the pattern
                    , Telescope Typed -- captures
                    , [(Type Typed, Type Typed)] -- constraints
                    , ImplicitScope ClassInfo Typed
                    )
inferParameter (PatParam p) = do
  (p, tau, t, cs, is) <- inferPattern p
  pure (PatParam p, Anon tau, t, cs, is)
inferParameter (EvParam p) = do
  (p, tau, t, cs, is) <- inferPattern p
  pure (EvParam p, Anon tau, t, cs, is)

boundTvs :: forall p. (Ord (Var p), Show (Var p))
         => Pattern p -> Telescope p -> Set.Set (Var p)
boundTvs p vs = pat p <> foldTele go vs where
  go :: Type p -> Set.Set (Var p)
  go x = ftv x `Set.union` Set.map (^. skolIdent) (skols x)

  pat Wildcard{} = mempty
  pat (PGadtCon _ _ _ p _) = foldMap pat p
  pat Capture{} = mempty
  pat (Destructure _ p _) = foldMap pat p
  pat (PAs p _ _) = pat p
  pat (PType _ t _) = ftv t
  pat (PRecord ps _) = foldMap (pat . snd) ps
  pat (PTuple ps _) = foldMap pat ps
  pat PLiteral{} = mempty
  pat PList{} = error "PList is handled by desugar"

skolGadt :: MonadNamey m
         => Var Desugared -> (Maybe a, Type Typed, Type Typed) -> m (Type Typed, Subst Typed)
skolGadt var (_, oty, ty) =
  let result (TyPi _ t) = result t
      result (TyWithConstraints _ t) = result t
      result t = t

      mentioned = ftv (result ty)
   in do
     vs <- for (Set.toList (ftv ty `Set.difference` mentioned)) $ \v ->
       (v,) <$> freshSkol (ByExistential var oty) ty v
     pure (apply (Map.fromList vs) ty, Map.fromList vs)

wrapPattern :: Applicative f => (Ann Typed -> Pattern Typed, Telescope Typed, [(Type Typed, Type Typed)], ImplicitScope ClassInfo Typed)
            -> Ann Typed
            -> (Type Typed, Wrapper Typed)
            -> f (Pattern Typed, Telescope Typed, [(Type Typed, Type Typed)], ImplicitScope ClassInfo Typed)
wrapPattern (k, t, s, is) (ann, it) _ = pure (k (ann, it), t, s, is)

splitConstructorTy :: Type Typed -> ([(Type Typed, Type Typed)], [TyBinder Typed], Type Typed)
splitConstructorTy (TyWithConstraints cs t) = splitConstructorTy t & _1 %~ mappend cs
splitConstructorTy (TyPi x@Implicit{} t) = splitConstructorTy t & _2 %~ (x:)
splitConstructorTy t = ([], [], t)

makeClassScope :: MonadNamey m
               => Ann Resolved
               -> [TyBinder Typed]
               -> m ( [(Var Typed, Type Typed)], ImplicitScope ClassInfo Typed )
makeClassScope _ [] = pure mempty
makeClassScope an (Implicit ty:tys) = do
  v <- genName
  (vs, is) <- makeClassScope an tys
  pure ((v, ty):vs, insert an LocalAssum v ty undefined is)
makeClassScope an (_:tys) = makeClassScope an tys -- impossible but why not?
