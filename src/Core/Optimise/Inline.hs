{-# LANGUAGE ScopedTypeVariables #-}
module Core.Optimise.Inline
  ( inlineVariable
  , betaReduce
  ) where

import Control.Monad.Writer hiding (pass)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Data.Foldable hiding (find)
import Data.Generics
import Data.Triple

import Core.Optimise
import Syntax (Resolved)

limit :: Int
limit = 100

-- The "opposite" (dual?) of a propagation pass, inlining attempts to
-- "pull in" the definition of a variable it considers cheap.
inlineVariable :: TransformPass
inlineVariable = pass go where
  go it@(CotRef v _) = do
    def <- find v
    case def of
      Just term -> do
        cost <- score term
        if cost >= limit || recursive term v
           then pure it
           else pure term
      Nothing -> pure it
  go x = pure x


betaReduce :: TransformPass
betaReduce = pass go where
  go term = case term of
    CotApp (CotLam Small (var, tp) body) ex -> do
      (var', ref) <- invent tp
      pure . CotLet [(var', tp, ex)] .  substitute (Map.singleton var ref) $ body
    CotTyApp (CotLam Big (var, _) body) tp ->
      pure $ substituteInTys (Map.singleton var tp) body
    _ -> pure $ term


score :: CoTerm -> Trans Int
score = fmap getSum . execWriterT . go where
  go :: CoTerm -> WriterT (Sum Int) Trans ()
  go (CotRef v _) = do
    tell 5
    flip when (tell (negate 4)) =<< lift (isCon v)
  go (CotLam s _ _) = do
    tell $ case s of
      Big -> 0
      Small -> 1
  go (CotApp f x) = go f *> go x
  go (CotLet vs e) = do
    for_ vs $ \(_, _, x) -> do
      tell 1
      go x
    go e
  go (CotMatch e ms) = do
    for_ ms $ \(p, _, x) -> do
      tell (Sum (gdepth p))
      go x
    go e
  go (CotBegin xs x) = for_ xs go *> go x
  go (CotLit _) = tell 0
  go (CotExtend x rs) = go x *> for_ rs (third3A go)
  go (CotTyApp x t) = go x *> tell (Sum (gdepth t))

recursive :: CoTerm -> Var Resolved -> Bool
recursive e v = v `Set.member` freeIn e
