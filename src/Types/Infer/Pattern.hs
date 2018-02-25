{-# LANGUAGE FlexibleContexts, GADTs #-}
{-# LANGUAGE ScopedTypeVariables, ViewPatterns, RankNTypes #-}
module Types.Infer.Pattern where

import Data.Traversable

import Control.Monad.Infer

import Syntax

import Types.Infer.Builtin
import Types.Kinds

inferPattern :: MonadInfer Typed m
             => Pattern Resolved
             -> m ( Pattern Typed -- the pattern
                  , Type Typed -- type of what the pattern matches
                  , [(Var Typed, Type Typed)] -- captures
                  )
inferPattern pat@(PType p t ann) = do
  (p', pt, vs) <- inferPattern p
  (t', _) <- resolveKind t `catchError` \x -> throwError (ArisingFrom x (BecauseOf pat))
  _ <- subsumes pat t' pt
  case p' of
    Capture v _ -> pure (PType p' t' (ann, t'), t', [(v, t')])
    _ -> pure (PType p' t' (ann, t'), t', vs)
inferPattern p = do
  x <- freshTV
  (p', binds) <- checkPattern p x
  pure (p', x, binds)

checkPattern :: MonadInfer Typed m
             => Pattern Resolved
             -> Type Typed
             -> m ( Pattern Typed
                  , [(Var Typed, Type Typed)]
                  )
checkPattern (Wildcard ann) ty = pure (Wildcard (ann, ty), [])
checkPattern (Capture v ann) ty = pure (Capture (TvName v) (ann, ty), [(TvName v, ty)])
checkPattern ex@(Destructure con ps ann) ty =
  case ps of
    Nothing -> do
      pty <- lookupTy con
      _ <- unify ex pty ty
      pure (Destructure (TvName con) Nothing (ann, pty), [])
    Just p -> do
      (c, d, _) <- decompose ex _TyArr =<< lookupTy con
      (ps', b) <- checkPattern p c
      _ <- unify ex ty d
      pure (Destructure (TvName con) (Just ps') (ann, d), b)
checkPattern pt@(PRecord rows ann) ty = do
  rho <- freshTV
  (rowps, rowts, caps) <- unzip3 <$> for rows (\(var, pat) -> do
    (p', t, caps) <- inferPattern pat
    pure ((var, p'), (var, t), caps))
  _ <- unify pt ty (TyRows rho rowts)
  pure (PRecord rowps (ann, TyRows rho rowts), concat caps)
checkPattern pt@(PTuple elems ann) ty =
  let go [x] t = (:[]) <$> checkPattern x t
      go (x:xs) t = do
        (left, right, _) <- decompose pt _TyTuple t
        (:) <$> checkPattern x left <*> go xs right
      go [] _ = error "malformed tuple in checkPattern"
    in case elems of
      [] -> do
        _ <- unify pt ty tyUnit
        pure (PTuple [] (ann, tyUnit), [])
      [x] -> checkPattern x ty
      xs -> do
        (ps, concat -> binds) <- unzip <$> go xs ty
        pure (PTuple ps (ann, ty), binds)
checkPattern pt@(PType p t ann) ty = do
  (p', it, binds) <- inferPattern p
  (t', _) <- resolveKind t
  _ <- subsumes pt t' it
  _ <- unify pt ty t'
  pure (PType p' t' (ann, t'), binds)

