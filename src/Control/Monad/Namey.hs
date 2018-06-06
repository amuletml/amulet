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
import Control.Arrow

import qualified Data.Text as T
import Data.Functor.Identity
import Data.Fixed
import Data.Char

import Syntax.Var

newtype NameyT m a =
  NameyT { unNameyT :: StrictS.StateT Int m a }
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

type Namey = NameyT Identity

instance MonadState s m => MonadState s (NameyT m) where
  get = lift StrictS.get
  put = lift . StrictS.put

class Monad m => MonadNamey m where
  genName :: m Name

runNameyT :: Functor m => NameyT m a -> Name -> m (a, Name)
runNameyT (NameyT k) (TgName _ i) = fmap (second genVar) $ StrictS.runStateT k i where
  genVar x = TgName (genAlnum x) x
runNameyT _ _ = undefined

evalNameyT :: Monad m => NameyT m a -> Name -> m a
evalNameyT (NameyT k) (TgName _ i) = StrictS.evalStateT k i
evalNameyT _ _ = undefined

runNamey :: NameyT Identity a -> Name -> (a, Name)
runNamey (NameyT k) (TgName _ i) = second genVar $ StrictS.runState k i where
  genVar x = TgName (genAlnum x) x
runNamey _ _ = undefined

instance Monad m => MonadNamey (NameyT m) where
  genName = NameyT $ do
    x <- get
    put (x + 1)
    pure (TgName (genAlnum x) x)

instance MonadNamey m => MonadNamey (ContT r m) where
  genName = lift genName

instance MonadNamey m => MonadNamey (StrictS.StateT s m) where
  genName = lift genName

instance MonadNamey m => MonadNamey (LazyS.StateT s m) where
  genName = lift genName

instance (MonadNamey m, Monoid s) => MonadNamey (StrictW.WriterT s m) where
  genName = lift genName

instance (MonadNamey m, Monoid s) => MonadNamey (LazyW.WriterT s m) where
  genName = lift genName

instance MonadNamey m => MonadNamey (ExceptT e m) where
  genName = lift genName

instance MonadNamey m => MonadNamey (Reader.ReaderT e m) where
  genName = lift genName

genAlnum :: Int -> T.Text
genAlnum 0 = T.singleton 'a'
genAlnum n = go (fromIntegral n) T.empty (floor (logBase 26 (fromIntegral n :: Double))) where
  go :: Double -> T.Text -> Int -> T.Text
  go n out 0 = T.snoc out (chr (97 + floor n))
  go n out p =
    let m = floor (n / (26 ^ p))
     in go (n `mod'` (26 ^ p)) (T.snoc out (chr (96 + m))) (p - 1)

