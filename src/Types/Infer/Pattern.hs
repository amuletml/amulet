{-# LANGUAGE FlexibleContexts, GADTs #-}
{-# LANGUAGE ScopedTypeVariables, ViewPatterns, RankNTypes, TupleSections #-}
module Types.Infer.Pattern where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Control.Monad.Infer
import Control.Lens

import Data.Traversable
import Data.Semigroup
import Data.List

import Types.Kinds
import Types.Infer.Builtin
import Types.Wellformed
import Types.Unify (freshSkol)

import Syntax.Types
import Syntax.Subst
import Syntax

inferPattern :: MonadInfer Typed m
             => Pattern Resolved
             -> m ( Pattern Typed -- the pattern
                  , Type Typed -- type of what the pattern matches
                  , Telescope Typed -- captures
                  , [(Type Typed, Type Typed)] -- constraints
                  )
inferPattern pat@(PType p t ann) = do
  (p', pt, vs, cs) <- inferPattern p
  t' <- resolveKind (BecauseOf pat) t
  _ <- subsumes pat t' pt
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
             => Pattern Resolved
             -> Type Typed
             -> m ( Pattern Typed
                  , Telescope Typed
                  , [(Type Typed, Type Typed)]
                  )
checkPattern (Wildcard ann) ty = pure (Wildcard (ann, ty), mempty, [])
checkPattern (Capture v ann) ty = pure (Capture (TvName v) (ann, ty), one v ty, [])
checkPattern ex@(Destructure con ps ann) target =
  case ps of
    Nothing -> do
      pty <- skolGadt con =<< lookupTy con
      let (cs, ty) =
            case pty of
              TyWithConstraints cs ty -> (cs, ty)
              _ -> ([], pty)
      _ <- unify ex ty target
      pure (Destructure (TvName con) Nothing (ann, ty), mempty, cs)
    Just p ->
      let go cs t = do
            (c, d, _) <- decompose ex _TyArr t
            (ps', b, cs') <- checkPattern p c
            _ <- unify ex d target
            pure (Destructure (TvName con) (Just ps') (ann, d), b, cs ++ cs')
      in do
        t <- skolGadt con =<< lookupTy con
        case t of
          TyWithConstraints cs ty -> go cs ty
          _ -> go [] t
checkPattern pt@(PRecord rows ann) ty = do
  rho <- freshTV
  (rowps, rowts, caps, cons) <- unzip4 <$> for rows (\(var, pat) -> do
    (p', t, caps, cs) <- inferPattern pat
    pure ((var, p'), (var, t), caps, cs))
  _ <- unify pt ty (TyRows rho rowts)
  pure (PRecord rowps (ann, TyRows rho rowts), mconcat caps, concat cons)
checkPattern pt@(PType p t ann) ty = do
  (p', it, binds, cs) <- inferPattern p
  t' <- resolveKind (BecauseOf pt) t
  _ <- subsumes pt t' it
  _ <- unify pt ty t'
  pure (PType p' t' (ann, t'), binds, cs)
checkPattern pt ty = do
  (p, ty', binds, cs) <- inferPattern pt
  _ <- subsumes pt ty ty'
  pure (p, binds, cs)

boundTvs :: forall p. Ord (Var p)
         => Pattern p -> Telescope p -> Set.Set (Var p)
boundTvs p vs = pat p <> foldTele go vs where
  go :: Type p -> Set.Set (Var p)
  go x = ftv x `Set.union` Set.map (^. skolIdent) (skols x)

  pat Wildcard{} = mempty
  pat Capture{} = mempty
  pat (Destructure _ p _) = maybe mempty pat p
  pat (PType _ t _) = ftv t
  pat (PRecord ps _) = foldMap (pat . snd) ps
  pat (PTuple ps _) = foldMap pat ps
  pat PLiteral{} = mempty

skolGadt :: MonadInfer Typed m => Var Resolved -> Type Typed -> m (Type Typed)
skolGadt var ty =
  let result (TyForall _ _ t) = result t
      result (TyWithConstraints _ t) = result t
      result (TyArr _ t) = result t
      result t = t

      related (TyWithConstraints cs _) = cs
      related _ = []

      mentioned = ftv (result ty)
      free (TyVar v, t)
        | v `Set.member` mentioned = [(v, t)]
        | otherwise = []
      free _ = []
      freed = concatMap free (related ty)
      fugitives = mentioned `Set.union` foldMap (ftv . snd) freed
   in do
     vs <- for (Set.toList (ftv ty `Set.difference` fugitives)) $ \v ->
       (v,) <$> freshSkol (ByExistential (TvName var) ty) ty v
     pure $ apply (Map.fromList vs) ty
