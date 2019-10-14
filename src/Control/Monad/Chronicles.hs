{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}

-- | A simple wrapper for "Control.Monad.Chronicle" which allows writing
-- single errors to a 'Seq'.
module Control.Monad.Chronicles
  ( module Control.Monad.Chronicle
  , Chronicles, ChroniclesT, MonadChronicles
  , dictates, confesses, retcons
  , dictate
  , recover, absolving, silence
  , catchChronicle
  ) where

import qualified Control.Monad.Chronicle as M
import Control.Monad.Chronicle hiding (dictate)

import Data.Sequence (Seq)

import GHC.Stack

type ChroniclesT c = ChronicleT (Seq c)
type Chronicles c = Chronicle (Seq c)
type MonadChronicles c = MonadChronicle (Seq c)

-- | 'dictate' a single value
dictates :: MonadChronicles c m => c -> m ()
dictates = dictate . pure

-- | 'confess' a single value
confesses :: MonadChronicles c m => c -> m a
confesses = confess . pure

dictate :: MonadChronicles c m => Seq c -> m ()
dictate = M.dictate

-- | 'retcon' a single value
retcons :: MonadChronicles c m => (c -> c) -> m a -> m a
retcons f = retcon (f<$>)

-- | Like 'absolve', but without the discarding of errors
recover :: MonadChronicle c m => a -> m a -> m a
recover r m = do
  m' <- memento m
  case m' of
    Left e -> M.dictate e >> pure r
    Right x -> pure x

-- | Like 'absolve', but with the ability to execute a monad action instead.
--
-- It's worth noting the arguments are flipped, so this a monadic
-- equivalent of @(<|>)@.
absolving :: MonadChronicle c m => m a -> m a -> m a
absolving l r = memento l >>= either (const r) pure

-- | Remove any record that a computation had. If it ended with
-- 'confess', explode.
silence :: (HasCallStack, MonadChronicle c m) => m a -> m a
silence = absolve se where
  se = error "silence: computation ended with fatal error"

-- | An equivalent of 'catchError' for 'MonadChronicle'.
--
-- Note, this captures both fatal and non-fatal errors and treats them
-- identically. This may or may not be what you want.
catchChronicle :: MonadChronicle c m => m a -> (c -> m a) -> m a
catchChronicle m f = memento (M.condemn m) >>= either f pure
