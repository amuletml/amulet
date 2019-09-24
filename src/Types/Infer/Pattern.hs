{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, ViewPatterns, TupleSections, TypeFamilies #-}
module Types.Infer.Pattern where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

import Control.Monad.Infer
import Control.Lens

import Data.Traversable
import Data.Spanned
import Data.List

import Types.Kinds
import Types.Infer.Builtin
import Types.Wellformed
import Types.Unify (freshSkol)

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
                  )
inferPattern pat@(PType p t ann) = do
  (p', pt, vs, cs) <- inferPattern p
  t' <- resolveKind (becausePat pat) t
  let keepPat ConImplicit{} = False
      keepPat _ = True

  _ <- censor (Seq.filter keepPat) $
    subsumes (becausePat pat) t' pt
  case p' of
    Capture v _ -> pure (PType p' t' (ann, t'), t', one v t', cs)
    _ -> pure (PType p' t' (ann, t'), t', vs, cs)
inferPattern (PRecord rows ann) = do
  rho <- freshTV
  (rowps, rowts, caps, cons) <- unzip4 <$> for rows (\(var, pat) -> do
    (p', t, caps, cs) <- inferPattern pat
    pure ((var, p'), (var, t), caps, cs))
  let ty = TyRows rho rowts
  pure (PRecord rowps (ann, ty), ty, mconcat caps, concat cons)
inferPattern (PTuple elems ann) =
  case elems of
    [] -> pure (PLiteral LiUnit (ann, tyUnit), tyUnit, mempty, [])
    [x] -> inferPattern x
    xs -> do
      (ps, ts, mconcat -> binds, concat -> cons) <- unzip4 <$> traverse inferPattern xs
      let ty = foldr1 TyTuple ts
      pure (PTuple ps (ann, ty), ty, binds, cons)
inferPattern (PLiteral l ann) = pure (PLiteral l (ann, t), t, mempty, [])
  where t = litTy l
inferPattern p = do
  x <- freshTV
  (p', vs, cs) <- checkPattern p x
  pure (p', x, vs, cs)

checkPattern :: MonadInfer Typed m
             => Pattern Desugared
             -> Type Typed
             -> m ( Pattern Typed
                  , Telescope Typed
                  , [(Type Typed, Type Typed)]
                  )
checkPattern (Wildcard ann) ty = pure (Wildcard (ann, ty), mempty, [])
checkPattern (Capture v ann) ty = pure (Capture v (ann, ty), one v ty, [])
checkPattern (PAs p v ann) ty = do
  (p', b, cs) <- checkPattern p ty
  pure (PAs p' v (ann, ty), one v ty <> b, cs)
checkPattern ex@(Destructure con ps ann) target =
  case ps of
    Nothing -> do
      (pty, sub) <- skolGadt con =<< lookupTy' Strong con
      let (cs, ty) =
            case pty of
              TyWithConstraints cs ty -> (cs, ty)
              _ -> ([], pty)
      co <- unify (becausePat ex) target ty
      (_1 %~ mkSkolPat sub) <$> wrapPattern (Destructure con Nothing, mempty, cs) (ann, target) (ty, co)
    Just p ->
      let go cs t sub = do
            (c, d, _) <- decompose (becausePat ex) _TyArr t
            (ps', b, cs') <- checkPattern p c
            co <- unify (becausePat ex) target d
            (_1 %~ mkSkolPat sub) <$>
              wrapPattern (Destructure con (Just ps'), b, cs ++ cs') (ann, target) (d, co)
      in do
        (t, sub) <- skolGadt con =<< lookupTy' Strong con
        case t of
          TyWithConstraints cs ty -> go cs ty sub
          _ -> go [] t sub
  where
    mkSkolPat sub p | Map.null sub = p
                    | otherwise = PSkolem p (Map.keys sub) (ann, target)
checkPattern pt@(PRecord rows ann) ty = do
  rho <- freshTV
  (rowps, rowts, caps, cons) <- unzip4 <$> for rows (\(var, pat) -> do
    (p', t, caps, cs) <- inferPattern pat
    pure ((var, p'), (var, t), caps, cs))
  co <- unify (becausePat pt) ty (TyRows rho rowts)
  wrapPattern (PRecord rowps, mconcat caps, concat cons) (ann, ty) (TyRows rho rowts, co)

checkPattern pat@(PTuple xs ann) ty@TyTuple{} = do
  let go [x] t = do
        (p, bs, cs) <- checkPattern x t
        pure ([p], bs, cs)
      go (x:xs) ty = do
        (a, b, _) <- decompose (BecauseOf pat) _TyTuple ty
        (p, bs, cs) <- checkPattern x a
        (ps, bss, css) <- go xs b
        pure (p:ps, bs <> bss, cs <> css)
      go _ _ = undefined
  (ps, bs, cs) <- go xs ty
  pure (PTuple ps (ann, ty), bs, cs)

checkPattern pt@(PType p t ann) ty = do
  (p', it, binds, cs) <- inferPattern p
  t' <- resolveKind (BecauseOf pt) t
  _ <- subsumes (becausePat pt) t' it
  co <- unify (becausePat pt) ty t'
  wrapPattern (PType p' t', binds, cs) (ann, ty) (t', co)

checkPattern pt ty = do
  (p, ty', binds, cs) <- inferPattern pt
  co <- subsumes (becausePat pt) ty ty'
  pure (PWrapper (co, ty') p (annotation p, ty), binds, cs)

checkParameter :: MonadInfer Typed m
               => Parameter Desugared
               -> Type Typed
               -> m ( Parameter Typed
                    , Telescope Typed
                    , [(Type Typed, Type Typed)]
                    )
checkParameter (PatParam p) t = do
  (p, t, cs) <- checkPattern p t
  pure (PatParam p, t, cs)
checkParameter (EvParam p) t = do
  (p, t, cs) <- checkPattern p t
  pure (EvParam p, t, cs)

inferParameter :: MonadInfer Typed m
               => Parameter Desugared
               -> m ( Parameter Typed -- the pattern
                    , TyBinder Typed -- binder for the pattern
                    , Telescope Typed -- captures
                    , [(Type Typed, Type Typed)] -- constraints
                    )
inferParameter (PatParam p) = do
  (p, tau, t, cs) <- inferPattern p
  pure (PatParam p, Anon tau, t, cs)
inferParameter (EvParam p) = do
  (p, tau, t, cs) <- inferPattern p
  pure (EvParam p, Anon tau, t, cs)

boundTvs :: forall p. (Ord (Var p), Show (Var p))
         => Pattern p -> Telescope p -> Set.Set (Var p)
boundTvs p vs = pat p <> foldTele go vs where
  go :: Type p -> Set.Set (Var p)
  go x = ftv x `Set.union` Set.map (^. skolIdent) (skols x)

  pat Wildcard{} = mempty
  pat (PSkolem p _ _) = pat p
  pat Capture{} = mempty
  pat (Destructure _ p _) = foldMap pat p
  pat (PAs p _ _) = pat p
  pat (PType _ t _) = ftv t
  pat (PRecord ps _) = foldMap (pat . snd) ps
  pat (PTuple ps _) = foldMap pat ps
  pat PLiteral{} = mempty
  pat (PWrapper _ p _) = pat p
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

wrapPattern :: Applicative f => (Ann Typed -> Pattern Typed, Telescope Typed, [(Type Typed, Type Typed)])
            -> Ann Typed
            -> (Type Typed, Wrapper Typed)
            -> f (Pattern Typed, Telescope Typed, [(Type Typed, Type Typed)])
wrapPattern (k, t, s) (ann, it) (ty, w) = pure (PWrapper (w, it) (k (ann, ty)) (ann, it), t, s)
