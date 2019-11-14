{-# LANGUAGE DeriveFunctor, TupleSections #-}
module Amc.Editor.NameyMT
  ( NameyMT
  , evalNameyMT
  ) where

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.Namey
import Syntax.Var

-- | A thread-safe version of NameyT.
--
-- Internally, this reserves a block of names from the 'TVar', and then uses
-- them. This avoids having to perform an atomic operation for every single
-- fresh variable usage.
data NameyMT a = NameyMT
  { runNameyMT :: TVar Int -> (Int, Int) -> IO (a, (Int, Int)) }
  deriving (Functor)

evalNameyMT :: NameyMT a -> TVar Int -> IO a
evalNameyMT action var = fst <$> runNameyMT action var (0, 0)

instance Applicative NameyMT where
  pure x = NameyMT $ \_ s -> pure (x, s)
  {-# INLINE pure #-}
  NameyMT mf <*> NameyMT mx = NameyMT $ \v s -> do
    (f, s') <- mf v s
    (x, s'') <- mx v s'
    pure (f x, s'')
  {-# INLINE (<*>) #-}

instance Monad NameyMT where
  NameyMT mx >>= f = NameyMT $ \v s -> do
    (x, s') <- mx v s
    runNameyMT (f x) v s'
  {-# INLINE (>>=) #-}

instance MonadNamey NameyMT where
  genName = NameyMT $ \v (x, lim) ->
    if x < lim
    then pure (TgName (genAlnum x) x, (x + 1, lim))
    else genNameAtLim v
  {-# INLINE genName #-}

instance MonadIO NameyMT where
  liftIO m = NameyMT $ \_ s -> (,s) <$> liftIO m

-- | The number of names to reserve in one go.
incr :: Int
incr = 64

genNameAtLim :: TVar Int -> IO (Name, (Int, Int))
genNameAtLim v = atomically $ do
  next <- readTVar v
  writeTVar v (next + incr)
  pure (TgName (genAlnum next) next, (next + 1, next + incr))
