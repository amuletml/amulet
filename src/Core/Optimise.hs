{-# LANGUAGE ConstraintKinds, ScopedTypeVariables, TupleSections #-}

module Core.Optimise
  ( mapTermM, mapTerm1M
  , mapTerm, mapTerm1
  , substitute
  , module Core.Core
  , Var(..)
  , TransformPass(..), TransState(..), TransM
  , beforePass, afterPass
  , beforePass', afterPass'
  , transformTerm, transformStmts, runTransform
  ) where

import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Generics (gmapM, mkM, Typeable)
import Data.Triple (third3A)
import Data.Traversable

import Control.Monad.Reader
import Control.Monad.Identity

import Core.Core
import Syntax

type TupleT m a = [(a, CoType, CoTerm)] -> m [(a, CoType, CoTerm)]


termT :: forall a m. (Typeable a, Monad m) => (CoTerm -> m CoTerm) -> a -> m a
termT f = mkM f
      >=> mkM (traverse f :: [CoTerm] -> m [CoTerm]) -- CotBegin
      >=> mkM (tupT :: TupleT m (Var Resolved))      -- CotLet
      >=> mkM (tupT :: TupleT m CoPattern)           -- CotMatch
      >=> mkM (tupT :: TupleT m Text)                -- CotExtend
  where tupT = traverse (third3A f)

-- Apply a function to each child in the provided expr. Note this does not
-- apply to all descendants.
mapTerm1M :: Monad m => (CoTerm -> m CoTerm) -> CoTerm -> m CoTerm
mapTerm1M f = gmapM (termT f)

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

data TransState = TransState { vars :: Map.Map (Var Resolved) CoTerm
                             , cons :: Map.Map (Var Resolved) [(Var Resolved, CoType)] }
                deriving (Show)

type TransM = Reader TransState

extendVars :: [(Var Resolved, CoType, CoTerm)] -> TransM a -> TransM a
extendVars vs = local (\s -> s { vars = foldr (\(v, _, e) m -> Map.insert v e m) (vars s) vs })

data TransformPass
  = TransformPass { before :: CoTerm -> TransM CoTerm
                  , after :: CoTerm -> TransM CoTerm }

instance Monoid TransformPass where
  mempty = TransformPass pure pure
  mappend (TransformPass b a) (TransformPass b' a') = TransformPass (b <=< b') (a <=< a')

beforePass, afterPass :: (CoTerm -> TransM CoTerm) -> TransformPass
beforePass fn = TransformPass { before = fn, after = pure }
afterPass  fn = TransformPass { before = pure, after = fn }

beforePass', afterPass' :: (CoTerm -> CoTerm) -> TransformPass
beforePass' fn = TransformPass { before = pure . fn, after = pure }
afterPass'  fn = TransformPass { before = pure, after = pure . fn }

transformTerm :: TransformPass -> CoTerm -> TransM CoTerm
transformTerm pass = before pass >=> visit >=> after pass where
  visit (CotLet vars body) = do
    vars' <- extendVars vars (traverse (third3A transform) vars)
    body' <- extendVars vars' (transform body)
    pure (CotLet vars' body')
  visit (CotMatch e cases) = do
    e' <- transform e
    cases' <- for cases (\(p, t, bod) ->
                           (p, t,) <$> case p of
                                         CopCapture v -> extendVars [(v, t, e)] (transform bod)
                                         _ -> transform bod)
    pure (CotMatch e' cases')
  visit x = mapTerm1M (transformTerm pass) x

  transform = transformTerm pass

transformStmts :: TransformPass -> [CoStmt] -> TransM [CoStmt]
transformStmts _ [] = pure []
transformStmts pass (x@CosForeign{}:xs) = (x:) <$> transformStmts pass xs
transformStmts pass (CosLet vars:xs) = do
  vars' <- extendVars vars (traverse (third3A (transformTerm pass)) vars)
  (CosLet vars':) <$> extendVars vars' (transformStmts pass xs)
transformStmts pass (x@(CosType v _ cases):xs) =
  (x:) <$> local (\s -> s { cons = Map.insert v cases (cons s) }) (transformStmts pass xs)

runTransform :: TransM a -> a
runTransform = flip runReader (TransState Map.empty Map.empty)
