{-# LANGUAGE
      MultiParamTypeClasses
      , FlexibleInstances
#-}

module Control.Monad.Report where

import Data.Foldable

class Monad m => MonadReport r m where
  report :: r -> m ()

  reports :: [r] -> m ()
  reports = traverse_ report

instance Show r => MonadReport r IO where
  report = print
