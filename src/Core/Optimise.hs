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
  , find, isCon
  ) where

import qualified Data.Map.Strict as Map

import Data.Traversable
import Data.Generics (everywhere, everywhereM, gmapM, mkM, mkT, Typeable)
import Data.Triple (third3A)
import Data.Maybe (fromMaybe)
import Data.Text (Text)

import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Writer hiding (pass)
import Control.Monad.Infer (fresh)
import Control.Monad.State
import Control.Monad.Gen

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

substituteInTys :: Map.Map (Var Resolved) CoType -> CoTerm -> CoTerm
substituteInTys m = everywhere (mkT go) where
  go :: CoType -> CoType
  go t@(CotyVar v) = Map.findWithDefault t v m
  go x = x

data TransState
  = TransState { vars :: Map.Map (Var Resolved) CoTerm
               , types :: Map.Map (Var Resolved) [(Var Resolved, CoType)]
               , cons :: Map.Map (Var Resolved) CoType }
  deriving (Show)

newtype Trans a = Trans { runTrans :: StateT Integer (ReaderT TransState (Gen Int)) a }
  deriving newtype (Functor, Applicative, Monad, MonadReader TransState, MonadGen Int)

extendVars :: [(Var Resolved, CoType, CoTerm)] -> Trans a -> Trans a
extendVars vs = local (\s -> s { vars = foldr (\(v, _, e) m -> Map.insert v e m) (vars s) vs })

newtype TransformPass = Pass { runPass :: CoTerm -> Trans CoTerm }

instance Monoid TransformPass where
  mempty = Pass pure
  mappend (Pass f) (Pass g) = Pass (f <=< g)

pass' :: (CoTerm -> CoTerm) -> TransformPass
pass' = pass . (pure .)

pass :: (CoTerm -> Trans CoTerm) -> TransformPass
pass f = Pass go where
  go x = do
    x' <- f x
    when (x /= x') $ pmodify succ
    pure x'
  pmodify :: (Integer -> Integer) -> Trans ()
  pmodify = Trans . modify

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
transformStmts pass (x@CosForeign{}:xs) = (x:) <$> transformStmts pass xs
transformStmts pass (CosLet vars:xs) = do
  vars' <- extendVars vars (traverse (third3A (transformTerm pass)) vars)
  (CosLet vars':) <$> extendVars vars' (transformStmts pass xs)
transformStmts pass (x@(CosType v cases):xs) =
  (x:) <$> local (\s ->
    s { types = Map.insert v cases (types s)
      , cons = Map.union (Map.fromList cases) (cons s) }) (transformStmts pass xs)

runTransform :: Trans a -> Gen Int (a, Integer)
runTransform = flip runReaderT (TransState Map.empty Map.empty Map.empty)
             . flip runStateT 0
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
