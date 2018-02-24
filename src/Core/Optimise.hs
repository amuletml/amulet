{-# LANGUAGE ConstraintKinds, ScopedTypeVariables, TupleSections, GeneralizedNewtypeDeriving, DerivingStrategies, BangPatterns #-}
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
import Data.Data
import Data.VarSet (IsVar(..))

import Control.Monad.Writer hiding (pass, (<>))
import Control.Monad.Infer (fresh)
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Applicative
import Control.Monad.Gen

import Core.Core
import Syntax

fuel :: Int
fuel = 5000

-- Apply a function to each child in the provided expr. Note this does not
-- apply to all descendants.
mapTerm1M :: Monad m => (CoTerm a -> m (CoTerm a)) -> CoTerm a -> m (CoTerm a)
mapTerm1M f t = case t of
  CotRef{} -> pure t
  CotLit{} -> pure t
  CotLam s v b -> CotLam s v <$> f b
  CotApp g x -> CotApp <$> f g <*> f x
  CotLet vs e -> CotLet <$> traverse (third3A f) vs <*> f e
  CotMatch e bs -> CotMatch <$> f e <*> traverse (third3A f) bs
  CotExtend t rs -> CotExtend <$> f t <*> traverse (third3A f) rs
  CotTyApp e t -> CotTyApp <$> f e <*> pure t

-- Apply a function to all descendants in the provided expr.
mapTermM :: Monad m => (CoTerm a -> m (CoTerm a)) -> CoTerm a -> m (CoTerm a)
mapTermM f = f <=< mapTerm1M (mapTermM f)

mapTerm1 :: (CoTerm a -> CoTerm a) -> CoTerm a -> CoTerm a
mapTerm1 f = runIdentity . mapTerm1M (pure . f)

mapTerm :: (CoTerm a -> CoTerm a) -> CoTerm a -> CoTerm a 
mapTerm f = runIdentity . mapTermM (pure . f)

substitute :: Ord a => Map.Map a (CoTerm a) -> CoTerm a -> CoTerm a
substitute m = mapTerm subst
  where subst e@(CotRef v _) = fromMaybe e (Map.lookup v m)
        subst e = e

substituteInTys :: forall a. (Data a, Typeable a, Ord a, IsVar a) => Map.Map (Var Resolved) (CoType a) -> CoTerm a -> CoTerm a
substituteInTys m = everywhere (mkT go) where
  go :: CoType a -> CoType a
  go t@(CotyVar v) = Map.findWithDefault t (toVar v) m
  go x = x

data TransState
  = TransState { vars :: Map.Map (Var Resolved) (CoTerm (Var Resolved))
               , types :: Map.Map (Var Resolved) [(Var Resolved, CoType (Var Resolved))]
               , cons :: Map.Map (Var Resolved) (CoType (Var Resolved))
               , foreigns :: Map.Map (Var Resolved) (CoType (Var Resolved)) }
  deriving (Show)

instance Semigroup TransState where
  TransState a b c d <> TransState a' b' c' d' =
    TransState (a <> a') (b <> b') (c <> c') (d <> d')

instance Monoid TransState where
  mempty = TransState mempty mempty mempty mempty
  mappend = (<>)

newtype Trans a = Trans { runTrans :: MaybeT (StateT Int (ReaderT TransState (Gen Int))) a }
  deriving newtype (Functor, Applicative, Monad, MonadReader TransState, MonadGen Int, MonadState Int, Alternative)

extendVars :: [(Var Resolved, CoType (Var Resolved), CoTerm (Var Resolved))] -> Trans a -> Trans a
extendVars vs = local (\s -> s { vars = foldr (\(v, _, e) m -> Map.insert v e m) (vars s) vs })

newtype TransformPass = Pass { runPass :: CoTerm (Var Resolved) -> Trans (CoTerm (Var Resolved)) }

instance Semigroup TransformPass where
  Pass f <> Pass g = Pass (g >=> f)
  {-# INLINE [0] (<>) #-}

instance Monoid TransformPass where
  mempty = Pass pure
  mappend = (<>)

{-# INLINE [0] pass' #-}
pass' :: (CoTerm (Var Resolved) -> CoTerm (Var Resolved)) -> TransformPass
pass' = pass . (pure .)

{-# INLINE [0] pass #-}
pass :: (CoTerm (Var Resolved) -> Trans (CoTerm (Var Resolved))) -> TransformPass
pass f = Pass $ \term -> do
  !x <- get
  if x >= 0
     then do
       modify pred
       f term
     else empty

transformTerm :: TransformPass -> CoTerm (Var Resolved) -> Trans (CoTerm (Var Resolved))
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

transformStmts :: TransformPass -> [CoStmt (Var Resolved)] -> Trans [CoStmt (Var Resolved)]
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

runTransform :: Trans a -> Gen Int (Maybe a)
runTransform = flip runReaderT mempty
             . flip evalStateT fuel
             . runMaybeT
             . runTrans

invent :: CoType (Var Resolved) -> Trans (Var Resolved, CoTerm (Var Resolved))
invent t = do
  x <- fresh
  pure (x, CotRef x t)

-- Rather magic, replaces everything matching the "predicate" by a
-- `let`-bound variable.
abstract :: (CoTerm (Var Resolved) -> Trans (Maybe (CoType (Var Resolved)))) -> CoTerm (Var Resolved) -> Trans (CoTerm (Var Resolved))
abstract p = fmap (uncurry (flip CotLet)) . runWriterT . everywhereM (mkM go) where
  go :: CoTerm (Var Resolved) -> WriterT [(Var Resolved, CoType (Var Resolved), CoTerm (Var Resolved))] Trans (CoTerm (Var Resolved))
  go term = do
    t <- lift (p term)
    case t of
      Just tp -> do
        (var, ref) <- lift $ invent tp
        tell [(var, tp, term)]
        pure ref
      Nothing -> pure term

abstract' :: (CoTerm (Var Resolved) -> Maybe (CoType (Var Resolved))) -> CoTerm (Var Resolved) -> Trans (CoTerm (Var Resolved))
abstract' p = abstract (pure . p)

find :: Var Resolved -> Trans (Maybe (CoTerm (Var Resolved)))
find var = Map.lookup var <$> asks vars

isCon :: Var Resolved -> Trans Bool
isCon var = Map.member var <$> asks cons

findForeign :: Var Resolved -> Trans (Maybe (CoType (Var Resolved)))
findForeign var = Map.lookup var <$> asks foreigns

