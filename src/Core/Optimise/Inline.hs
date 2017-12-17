module Core.Optimise.Inline
  ( inlineOnce
  ) where

import qualified Data.Map.Strict as Map
import Data.Triple

import Control.Monad.Reader
import Core.Optimise

import Syntax (Resolved)

threshold :: Integer
threshold = 100

inlineOnce :: TransformPass
inlineOnce = afterPass go where
  go :: CoTerm -> TransM CoTerm
  go app@CotApp{} = do
    env <- asks vars
    let application = unwind app
    case application of
      (CotRef fun _, args) -> case Map.lookup fun env of
        Just lam@CotLam{} -> do
          cost <- score (Just fun) lam
          pure $ if cost >= threshold
                    then app
                    else reduce lam args
        _ -> pure app
      (lam@CotLam{}, args) -> do
        pure $ reduce lam args
      _ -> pure app
  go x = pure x

reduce :: CoTerm -> [Either CoTerm CoType] -> CoTerm
reduce (CotLam Big _ b) (Right _:xs) = reduce b xs
reduce (CotLam Small (arg, _) b) (Left x:xs)
  = reduce (substitute (Map.fromList [(arg, x)]) b) xs
reduce c ap = foldl rewind c ap where
  rewind :: CoTerm -> Either CoTerm CoType -> CoTerm
  rewind c (Left t) = CotApp c t
  rewind c (Right tp) = CotTyApp c tp

unwind :: CoTerm -> (CoTerm, [Either CoTerm CoType])
unwind = go [] where
  go acc (CotApp t arg)  = go (Left arg:acc) t
  go acc (CotTyApp t tp) = go (Right tp:acc) t
  go acc x = (x, acc)

score :: Maybe (Var Resolved) -> CoTerm -> TransM Integer
score (Just x) f
  | x `elem` freeIn f = pure 1000
  | otherwise = score Nothing f
score Nothing f = go f where
  go :: CoTerm -> TransM Integer
  go (CotLam Small _ e) = succ <$> go e
  go (CotLam Big _ e) = go e
  go (CotTyApp e _) = go e
  go (CotRef v _) = do
    x <- asks (Map.lookup v . vars)
    case x of
      Just e -> go e
      Nothing -> pure 5
  go (CotApp f x) = (+) <$> go f <*> go x
  go (CotLet vs e) = do
    costs <- traverse (go . thd3) vs
    (sum costs +) <$> go e
  go (CotMatch e bs) = do
    costs <- traverse (go . thd3) bs
    (sum costs +) <$> go e
  go (CotBegin xs e) = do
    costs <- traverse go xs
    (sum costs +) <$> go e
  go CotLit{} = pure 0
  go (CotExtend e xs) = do
    costs <- traverse (go . thd3) xs
    (sum costs +) <$> go e
