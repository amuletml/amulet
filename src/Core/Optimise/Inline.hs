{-# LANGUAGE ScopedTypeVariables #-}
module Core.Optimise.Inline
  ( inlineVariable
  , betaReduce
  , complexSafePropag
  ) where

import qualified Data.Map.Merge.Strict as Map
import qualified Data.Map.Strict as Map
import qualified Data.VarSet as VarSet
import Data.Generics
import Data.Triple
import Data.Either
import Data.Maybe

import Core.Optimise
import Syntax (Resolved)

import Debug.Trace
import Pretty (pretty)

limit :: Int
limit = 500

-- The "opposite" (dual?) of a propagation pass, inlining attempts to
-- "pull in" the definition of a variable it considers cheap.
inlineVariable :: TransformPass
inlineVariable = pass go where
  go it@(CotRef v _) = do
    def <- find v
    case def of
      Just term -> do
        cost <- score term
        if not (isLambda term) || cost >= limit || recursive term v
           then pure it
           else pure term
      Nothing -> pure it
  go x = pure x

  isLambda CotLam{} = True
  isLambda _ = False

betaReduce :: TransformPass
betaReduce = pass go where
  go term = case term of
    CotApp (CotLam Small (var, tp) body) ex -> do
      (var', ref) <- invent tp
      pure . CotLet [(var', tp, ex)] .  substitute (Map.singleton var ref) $ body
    CotTyApp (CotLam Big (var, _) body) tp ->
      pure $ substituteInTys (Map.singleton var tp) body
    _ -> pure term

complexSafePropag :: TransformPass
complexSafePropag = pass go where
  go :: CoTerm (Var Resolved) -> Trans (CoTerm (Var Resolved))
  go (CotLet vs e) =
    let uses = countUses e
        classify (v, t, e) = do
          cost <- score e
          pure $ if fromMaybe 0 (Map.lookup v uses) == 1 && cost < limit && not (recursiveLam e v)
                    then traceShow (pretty e) $ Left (Map.singleton v e)
                    else Right (v, t, e)
     in do
       (propag, keep) <- partitionEithers <$> (traverse classify vs)
       pure $ CotLet keep (substitute (mconcat propag) e)
  go x = pure x

  countUses = everything combine (mkQ mempty go) where
    go :: CoTerm (Var Resolved) -> Map.Map (Var Resolved) Integer
    go (CotRef v _) = Map.singleton v 1
    go _ = mempty

    combine :: Map.Map (Var Resolved) Integer -> Map.Map (Var Resolved) Integer -> Map.Map (Var Resolved) Integer
    combine = Map.merge Map.preserveMissing Map.preserveMissing (Map.zipWithMatched (const (+)))

score :: CoTerm (Var Resolved)  -> Trans Int
score (CotRef v _) = do
  x <- isCon v
  if x
     then pure 0
     else pure 5
score (CotLam s _ t)
  | s == Big  = score t
  | otherwise = (+ 1) <$> score t
score (CotApp f x) = (+) <$> score f <*> score x
score (CotLet vs e) = do
  vs' <- sum <$> traverse (score . thd3) vs
  (+) vs' <$> score e
score (CotMatch e bs) = do
  bs' <- sum <$> traverse (score . thd3) bs
  (+) bs' <$> score e
score (CotExtend e rs) = do
  rs' <- sum <$> traverse (score . thd3) rs
  (+) rs' <$> score e
score CotLit{} = pure 1
score (CotTyApp t _) = (+ 1) <$> score t

recursive :: VarSet.IsVar a => CoTerm a -> Var Resolved -> Bool
recursive e v = v `VarSet.member` freeIn e

recursiveLam :: VarSet.IsVar a => CoTerm a -> Var Resolved -> Bool
recursiveLam (CotLam _ _ e) v = v `VarSet.member` freeIn e
recursiveLam _ _ = False
