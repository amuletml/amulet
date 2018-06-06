{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving, FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Monad.Namey where

import qualified Control.Monad.Writer.Strict as StrictW
import qualified Control.Monad.State.Strict as StrictS
import qualified Control.Monad.Writer.Lazy as LazyW
import qualified Control.Monad.State.Lazy as LazyS
import qualified Control.Monad.Reader as Reader
import Control.Monad.State.Class
import Control.Monad.IO.Class
import Control.Monad.Except
import Control.Applicative
import Control.Monad.Cont

import Data.Functor.Identity

newtype NameyT nam m a =
  NameyT { unNameyT :: StrictS.StateT [nam] m a }
  deriving newtype
  ( Functor
  , Applicative
  , Monad
  , MonadTrans
  , MonadPlus
  , Alternative
  , Reader.MonadReader r
  , StrictW.MonadWriter w
  , MonadError e
  , MonadIO
  )

type Namey n = NameyT n Identity

instance MonadState s m => MonadState s (NameyT nam m) where
  get = lift StrictS.get
  put = lift . StrictS.put

class Monad m => MonadNamey nam m | m -> nam where
  genName :: m nam
  forkNames :: m [nam]

runNameyT :: NameyT nam m a -> [nam] -> m (a, [nam])
runNameyT (NameyT k) ns = StrictS.runStateT k ns

evalNameyT :: Monad m => NameyT nam m a -> [nam] -> m a
evalNameyT (NameyT k) ns = StrictS.evalStateT k ns

runNamey :: NameyT nam Identity a -> [nam] -> (a, [nam])
runNamey (NameyT k) ns = StrictS.runState k ns

instance Monad m => MonadNamey nam (NameyT nam m) where
  genName = NameyT $ do
    (x:xs) <- get
    put xs
    pure x
  forkNames = NameyT get

instance MonadNamey n m => MonadNamey n (ContT r m) where
  genName = lift genName
  forkNames = lift forkNames

instance MonadNamey n m => MonadNamey n (StrictS.StateT s m) where
  genName = lift genName
  forkNames = lift forkNames

instance MonadNamey n m => MonadNamey n (LazyS.StateT s m) where
  genName = lift genName
  forkNames = lift forkNames

instance (MonadNamey n m, Monoid s) => MonadNamey n (StrictW.WriterT s m) where
  genName = lift genName
  forkNames = lift forkNames

instance (MonadNamey n m, Monoid s) => MonadNamey n (LazyW.WriterT s m) where
  genName = lift genName
  forkNames = lift forkNames

instance MonadNamey n m => MonadNamey n (ExceptT e m) where
  genName = lift genName
  forkNames = lift forkNames

instance MonadNamey n m => MonadNamey n (Reader.ReaderT e m) where
  genName = lift genName
  forkNames = lift forkNames

