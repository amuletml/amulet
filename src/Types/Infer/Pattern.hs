{-# LANGUAGE FlexibleContexts, OverloadedStrings, TupleSections, GADTs #-}
{-# LANGUAGE ScopedTypeVariables, ViewPatterns, RankNTypes #-}
module Types.Infer.Pattern where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Traversable
import Data.Generics
import Data.Triple
import Data.List (sort)
import Data.Span

import Control.Monad.Infer
import Control.Arrow (first, (&&&))

import Syntax.Subst
import Syntax.Raise
import Syntax

import Types.Infer.Builtin
import Types.Wellformed
import Types.Unify
import Types.Holes
import Types.Kinds

import Control.Lens

import Pretty (prettyPrint)

-- inferPattern :: MonadInfer Typed m
--              => (Type Typed -> Type Typed -> m a)
--              -> Pattern Resolved
--              -> m ( Pattern Typed -- the pattern
--                   , Type Typed -- type of what the pattern matches
--                   , [(Var Typed, Type Typed)] -- captures
--                   )
-- inferPattern _ (Wildcard ann) = do
--   x <- freshTV
--   pure (Wildcard (ann, x), x, [])
-- inferPattern _ (Capture v ann) = do
--   x <- freshTV
--   pure (Capture (TvName v) (ann, x), x, [(TvName v, x)])
-- inferPattern unify (Destructure cns ps ann)
--   | Nothing <- ps = do
--     pty <- lookupTy cns
--     pure (Destructure (TvName cns) Nothing (ann, pty), pty, [])
--   | Just p <- ps = do
--     (tup, res, _) <- constructorTy <$> lookupTy cns
--     (p', pt, pb) <- inferPattern unify p
--     _ <- unify tup pt
--     pure (Destructure (TvName cns) (Just p') (ann, res), res, pb)
--   where constructorTy :: Type Typed -> (Type Typed, Type Typed, Type Typed)
--         constructorTy t
--           | TyArr tup res <- t = (tup, res, t)
--           | otherwise = error (T.unpack (prettyPrint t))
-- inferPattern unify (PRecord rows ann) = do
--   rho <- freshTV
--   (rowps, rowts, caps) <- unzip3 <$> for rows (\(var, pat) -> do
--     (p', t, caps) <- inferPattern unify pat
--     pure ((var, p'), (var, t), caps))
--   pure (PRecord rowps (ann, TyRows rho rowts), TyRows rho rowts, concat caps)
-- inferPattern unify (PType p t ann) = do
--   (p', pt, vs) <- inferPattern unify p
--   (t', _) <- resolveKind t
--   _ <- unify pt t'
--   case p' of
--     Capture v _ -> pure (PType p' t' (ann, t'), t', [(v, t')])
--     _ -> pure (PType p' t' (ann, t'), t', vs)
-- inferPattern unify (PTuple elems ann)
--   | [] <- elems = pure (PTuple [] (ann, tyUnit), tyUnit, [])
--   | [x] <- elems = inferPattern unify x
--   | otherwise = do
--     (ps, t:ts, cps) <- unzip3 <$> traverse (inferPattern unify) elems
--     pure (PTuple ps (ann, mkTT t ts), mkTT t ts, concat cps)


inferPattern :: MonadInfer Typed m
             => Pattern Resolved
             -> m ( Pattern Typed -- the pattern
                  , Type Typed -- type of what the pattern matches
                  , [(Var Typed, Type Typed)] -- captures
                  )
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
      ty <- lookupTy con
      (c, d) <- decompose ex _TyArr ty
      (ps', b) <- checkPattern p c
      pure (Destructure (TvName con) (Just ps') (ann, ty), b)
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
        (left, right) <- decompose pt _TyTuple t
        (:) <$> checkPattern x left <*> go xs right
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
  (nt, _) <- resolveKind t
  _ <- subsumes pt it nt
  _ <- unify pt nt ty
  pure (PType p' nt (ann, ty), binds)

