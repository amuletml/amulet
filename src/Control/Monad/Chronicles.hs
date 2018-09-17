{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}

-- | A simple wrapper for "Control.Monad.Chronicle" which allows writing
-- single errors to a 'Seq'.
module Control.Monad.Chronicles
  ( module Control.Monad.Chronicle
  , Chronicles, ChroniclesT, MonadChronicles
  , dictates, confesses, retcons
  , recover, absolving
  , catchChronicle
  ) where

import Control.Monad.Chronicle

import Data.Sequence (Seq)

type ChroniclesT c = ChronicleT (Seq c)

type Chronicles c = Chronicle (Seq c)

type MonadChronicles c = MonadChronicle (Seq c)

-- | 'dictate' a single value
dictates :: MonadChronicles c m => c -> m ()
dictates = dictate . pure

-- | 'confess' a single value
confesses :: MonadChronicles c m => c -> m a
confesses = confess . pure

-- | 'retcon' a single value
retcons :: MonadChronicles c m => (c -> c) -> m a -> m a
retcons f = retcon (f<$>)

-- | Like 'absolve', but without the discarding of errors
recover :: MonadChronicle c m => a -> m a -> m a
recover r m = do
  m' <- memento m
  case m' of
    Left e -> dictate e >> pure r
    Right x -> pure x

-- | Like 'absolve', but with the ability to execute a monad action instead.
--
-- It's worth noting the arguments are flipped, so this a monadic
-- equivalent of @(<|>)@.
absolving :: MonadChronicle c m => m a -> m a -> m a
absolving l r = memento l >>= either (const r) pure

-- | An equivalent of 'catchError' for 'MonadChronicle'.
--
-- Note this does not catch non-fatal errors. Use 'retcon' for that.
catchChronicle :: MonadChronicle c m => m a -> (c -> m a) -> m a
catchChronicle m f = memento m >>= either f pure
