{-# LANGUAGE MultiWayIf, Trustworthy, OverloadedStrings #-}
module Control.Timing
  ( Timer
  , newTimer
  , endTimer
  , withTimer
  , timingReport
  ) where

import Control.Exception.Base (evaluate, mask)

import Control.Concurrent.MVar
import Control.Monad.IO.Class

import System.Environment
import System.IO.Unsafe (unsafePerformIO)
import System.IO

import qualified Data.IntSet as Set
import Data.Time.Clock
import Data.Hashable
import Data.Fixed
import Data.List
import Data.Ord

import Text.Pretty.Semantic

-- | A timer with 10^-12 second resolution.
data Timer =
  Timer { _desc :: !String
        , _start :: {-# UNPACK #-} !UTCTime
        , _dHash :: {-# UNPACK #-} !Int
        }

-- | A timing event (either the start of a timer, or the end of a timer)
data Event
  = Elapsed { _timer :: {-# UNPACK #-} !Timer
            , elapsed :: !NominalDiffTime
            }
  | Started {-# UNPACK #-} !Timer

finishedTimers :: MVar [Event]
finishedTimers = unsafePerformIO (newMVar [])
{-# NOINLINE finishedTimers #-}

-- | Create a new timer with the given description.
newTimer :: MonadIO m => String -> m Timer
newTimer desc = do
  let dHash = hash desc
  timer <- Timer desc <$> liftIO getCurrentTime <*> pure dHash
  liftIO $ mask $ \_ -> do
    q <- takeMVar finishedTimers
    putMVar finishedTimers =<< evaluate (Started timer:q) 
  pure timer

-- | End the given timer and report the elapsed time to AMC_TIMING.
endTimer :: MonadIO m => Timer -> m ()
endTimer timer@(Timer desc t_then _) = do
  t_now <- liftIO getCurrentTime
  let elapsed = t_now `diffUTCTime` t_then
  report <- liftIO $ lookupEnv "AMC_LIVE_TIMING"
  case report of
    Just{} -> liftIO $ do
      t <- hIsTerminalDevice stderr
      if t
         then hPutDoc stderr (pretty (Elapsed timer elapsed))
         else hPutStrLn stderr (reportTimer desc elapsed)
    _ -> pure ()

  --     ↓ can't be made into (.) because impredicative polymorphism
  liftIO $ mask $ \_ -> do
    q <- takeMVar finishedTimers
    putMVar finishedTimers =<< evaluate (Elapsed timer elapsed:q) 

withTimer :: MonadIO m => String -> m a -> m a
withTimer desc k = do
  timer <- newTimer desc
  r <- k
  endTimer timer
  pure r

reportTimer :: String -> NominalDiffTime -> String
reportTimer desc t = "amc: " ++ desc ++ " took " ++ show (prettyTime t)

prettyTime :: NominalDiffTime -> Doc
prettyTime t = magnitude where
  magnitude =
    let pTime = negate $ logBase 10 (realToFrac sec)
        pTime :: Double
        -- We take -log₁₀(elapsed) to get the magnitude so the bounds
        -- are clearer.
     in if | pTime < 0 -> show_s sec <> "s"
           | pTime > 0 && pTime <= 3 -> show_s (sec * mili) <> "ms"
           | pTime > 3 && pTime <= 6 -> show_s (sec * micro) <> "us"
           | pTime > 6 && pTime <= 9 -> show_s (sec * nano) <> "ns"
           | otherwise {- pTime > 9 -} -> show_s (sec * pico) <> "ps"

  sec = nominalDiffTimeToSeconds t

  show_s :: Pico -> Doc
  show_s = sliteral . (shown :: Double -> Doc) . realToFrac

  mili, micro, nano, pico :: Pico
  mili  = 10 ^ (3 :: Int)
  micro = 10 ^ (6 :: Int)
  nano  = 10 ^ (9 :: Int)
  pico  = 10 ^ (12 :: Int)

timingReport :: (Doc -> IO ()) -> IO ()
timingReport putDoc = do
  times <- reverse <$> swapMVar finishedTimers []
  let slowest_ts =
        take 5 $ sortOn (Down . elapsed) (filter isFinished times)
      slowest = vsep
        [ "5 slowest elapsed timers:"
        , vsep (map (indent 2 . bullet . pretty) slowest_ts)
        ]
      isFinished x = case x of
        Elapsed{} -> True
        _ -> False

  putDoc $
    vsep [ "Timing tree:"
         , makeTimingTree mempty 0 [] times
         , parens (int (length times) <+> "events in total")
         , empty
         , slowest
         ]
  where
    makeTimingTree :: Set.IntSet -> Int -> [String] -> [Event] -> Doc
    makeTimingTree set i acc (Started (Timer d _ hash):xs)
      | hash `Set.member` set = makeTimingTree set i acc xs
      | otherwise =
        indent i (char '*' <+> string d) <#> makeTimingTree (Set.insert i set) (i + 1) (d:acc) xs

    makeTimingTree set i (d:acc) (ev@(Elapsed (Timer d' _ _) _):xs)
      | d == d'   = indent (i - 1) (reportEnd ev) <#> makeTimingTree set (i - 1) acc xs
      | otherwise = indent i (reportEnd ev) <#> makeTimingTree set i (d:acc) xs

    makeTimingTree set i [] (Elapsed{}:xs) =
      makeTimingTree set i [] xs

    makeTimingTree _ _ _ [] = empty

    reportEnd :: Event -> Doc
    reportEnd (Elapsed _ took) = "→ took" <+> prettyTime took
    reportEnd Started{} = undefined

instance Pretty Event where
  pretty (Elapsed (Timer desc _ _) took) =
    vsep [ char '«' <> string desc <> "» took" <+> prettyTime took ]
  pretty (Started (Timer desc _ _)) =
    vsep [ keyword "Started" <+> char '«' <> skeyword (string desc) <> char '»' ]
