module Control.Timing
  ( Timer
  , newTimer
  , endTimer
  , withTimer
  ) where

import Control.Monad.IO.Class

import System.Environment

import Data.Time.Clock

import Data.List

-- | A timer with 10^-12 second resolution.
data Timer = Timer !String {-# UNPACK #-} !UTCTime

-- | Create a new timer with the given description.
newTimer :: MonadIO m => String -> m Timer
newTimer desc = Timer desc <$> liftIO getCurrentTime

-- | End the given timer and report the elapsed time to AMC_TIMING.
endTimer :: MonadIO m => Timer -> m ()
endTimer (Timer desc t_then) = do
  t_now <- liftIO getCurrentTime
  let elapsed = t_now `diffUTCTime` t_then
  report <- liftIO $ lookupEnv "AMC_TIMING"
  case report of
    Just x | "file:" `isPrefixOf` x -> do
      let fname = drop (length "file:") x
      liftIO $ appendFile fname (reportTimer desc elapsed)
    Just x | x == "io" || x == "stdout" ->
      liftIO $ putStrLn (reportTimer desc elapsed)
    _ -> pure ()

withTimer :: MonadIO m => String -> m a -> m a
withTimer desc k = do
  timer <- newTimer desc
  r <- k
  endTimer timer
  pure r

reportTimer :: String -> NominalDiffTime -> String
reportTimer desc t = "amc: " ++ desc ++ " took " ++ show t
