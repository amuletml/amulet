{-# LANGUAGE ConstraintKinds, ScopedTypeVariables, TupleSections, GeneralizedNewtypeDeriving, DerivingStrategies #-}
module Core.Optimise
  ( mapTermM, mapTerm1M
  , mapTerm, mapTerm1
  , substitute, substituteInTys
  , module Core.Core
  , Var(..)
  , TransformPass, TransState(..), Trans
  , pass, pass', transformTerm, transformStmts, runTransform

  , invent, abstract, abstract', fresh
  , find, isCon, findForeign
  ) where

import qualified Data.Map.Strict as Map

import Data.Traversable
import Data.Semigroup
import Data.Generics (everywhere, everywhereM, mkM, mkT)
import Data.Triple (third3A)
import Data.Maybe (fromMaybe)

import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Writer hiding (pass, (<>))
import Control.Monad.Infer (fresh)
import Control.Monad.Gen

import Core.Core
import Syntax

-- Apply a function to each child in the provided expr. Note this does not
-- apply to all descendants.
mapTerm1M :: Monad m => (CoTerm -> m CoTerm) -> CoTerm -> m CoTerm
mapTerm1M f t = case t of
  CotRef{} -> pure t
  CotLit{} -> pure t
  CotLam s v b -> CotLam s v <$> f b
  CotApp g x -> CotApp <$> f g <*> f x
  CotLet vs e -> CotLet <$> traverse (third3A f) vs <*> f e
  CotMatch e bs -> CotMatch <$> f e <*> traverse (third3A f) bs
  CotBegin es e -> CotBegin <$> traverse f es <*> f e
  CotExtend t rs -> CotExtend <$> f t <*> traverse (third3A f) rs
  CotTyApp e t -> CotTyApp <$> f e <*> pure t

-- Apply a function to all descendants in the provided expr.
mapTermM :: Monad m => (CoTerm -> m CoTerm) -> CoTerm -> m CoTerm
mapTermM f = f <=< mapTerm1M (mapTermM f)

mapTerm1 :: (CoTerm -> CoTerm) -> CoTerm -> CoTerm
mapTerm1 f = runIdentity . mapTerm1M (pure . f)

mapTerm :: (CoTerm -> CoTerm) -> CoTerm -> CoTerm
mapTerm f = runIdentity . mapTermM (pure . f)

substitute :: Map.Map (Var Resolved) CoTerm -> CoTerm -> CoTerm
substitute m = mapTerm subst
  where subst e@(CotRef v _) = fromMaybe e (Map.lookup v m)
        subst e = e

substituteInTys :: Map.Map (Var Resolved) CoType -> CoTerm -> CoTerm
substituteInTys m = everywhere (mkT go) where
  go :: CoType -> CoType
  go t@(CotyVar v) = Map.findWithDefault t v m
  go x = x

data TransState
  = TransState { vars :: Map.Map (Var Resolved) CoTerm
               , types :: Map.Map (Var Resolved) [(Var Resolved, CoType)]
               , cons :: Map.Map (Var Resolved) CoType
               , foreigns :: Map.Map (Var Resolved) CoType }
  deriving (Show)

instance Semigroup TransState where
  TransState a b c d <> TransState a' b' c' d' =
    TransState (a <> a') (b <> b') (c <> c') (d <> d')

instance Monoid TransState where
  mempty = TransState mempty mempty mempty mempty
  mappend = (<>)

newtype Trans a = Trans { runTrans :: ReaderT TransState (Gen Int) a }
  deriving newtype (Functor, Applicative, Monad, MonadReader TransState, MonadGen Int)

extendVars :: [(Var Resolved, CoType, CoTerm)] -> Trans a -> Trans a
extendVars vs = local (\s -> s { vars = foldr (\(v, _, e) m -> Map.insert v e m) (vars s) vs })

newtype TransformPass = Pass { runPass :: CoTerm -> Trans CoTerm }

instance Semigroup TransformPass where
  Pass f <> Pass g = Pass (\x -> g x >>= f)
  {-# INLINE [0] (<>) #-}

instance Monoid TransformPass where
  mempty = Pass pure
  mappend = (<>)

{-# INLINE [0] pass' #-}
pass' :: (CoTerm -> CoTerm) -> TransformPass
pass' = Pass . (pure .)

{-# INLINE [0] pass #-}
pass :: (CoTerm -> Trans CoTerm) -> TransformPass
pass = Pass

transformTerm :: TransformPass -> CoTerm -> Trans CoTerm
transformTerm pass = visit <=< runPass pass where
  visit (CotLet vars body) = do
    vars' <- extendVars vars (traverse (third3A transform) vars)
    body' <- extendVars vars' (transform body)
    pure (CotLet vars' body')
  visit (CotMatch e cases) = do
    e' <- transform e
    cases' <- for cases (\(p, t, bod) ->
                           (p, t,) <$> case p of
                             CopCapture v _ -> extendVars [(v, t, e)] (transform bod)
                             _ -> transform bod)
    pure (CotMatch e' cases')
  visit x = mapTerm1M (transformTerm pass) x

  transform = transformTerm pass

transformStmts :: TransformPass -> [CoStmt] -> Trans [CoStmt]
transformStmts _ [] = pure []
transformStmts pass (x@(CosForeign v t _):xs) =
  (x:) <$> local (\s -> s { foreigns = Map.insert v t (foreigns s) }) (transformStmts pass xs)
transformStmts pass (CosLet vars:xs) = do
  vars' <- extendVars vars (traverse (third3A (transformTerm pass)) vars)
  (CosLet vars':) <$> extendVars vars' (transformStmts pass xs)
transformStmts pass (x@(CosType v cases):xs) =
  (x:) <$> local (\s ->
    s { types = Map.insert v cases (types s)
      , cons = Map.union (Map.fromList cases) (cons s) }) (transformStmts pass xs)

runTransform :: Trans a -> Gen Int a
runTransform = flip runReaderT mempty
             . runTrans

invent :: CoType -> Trans (Var Resolved, CoTerm)
invent t = do
  x <- fresh
  pure (x, CotRef x t)

-- Rather magic, replaces everything matching the "predicate" by a
-- `let`-bound variable.
abstract :: (CoTerm -> Trans (Maybe CoType)) -> CoTerm -> Trans CoTerm
abstract p = fmap (uncurry (flip CotLet)) . runWriterT . everywhereM (mkM go) where
  go :: CoTerm -> WriterT [(Var Resolved, CoType, CoTerm)] Trans CoTerm
  go term = do
    t <- lift (p term)
    case t of
      Just tp -> do
        (var, ref) <- lift $ invent tp
        tell [(var, tp, term)]
        pure ref
      Nothing -> pure term

abstract' :: (CoTerm -> Maybe CoType) -> CoTerm -> Trans CoTerm
abstract' p = abstract (pure . p)

find :: Var Resolved -> Trans (Maybe CoTerm)
find var = Map.lookup var <$> asks vars

isCon :: Var Resolved -> Trans Bool
isCon var = Map.member var <$> asks cons

findForeign :: Var Resolved -> Trans (Maybe CoType)
findForeign var = Map.lookup var <$> asks foreigns
