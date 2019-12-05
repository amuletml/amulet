{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import AmuletLsp.Loop

import Options.Applicative

import qualified System.Log.Handler.Simple as LH
import qualified System.Log.Formatter as LF
import qualified System.Log.Handler as LH
import System.Log.Logger
import GHC.IO.Encoding
import System.Exit
import System.IO

import Version

newtype Args = Args { _log :: Maybe FilePath }

argParser :: ParserInfo Args
argParser = info (args <**> helper <**> version)
       $ fullDesc <> progDesc ("The Amulet Language Server, version " ++ $(amcVersion))
  where
    version :: Parser (a -> a)
    version
      = infoOption $(amcVersion)
      $ long "version" <> short 'v' <> help "Show version information"

    args :: Parser Args
    args = Args
      <$> optional ( option str
           ( long "log" <> metavar "FILE"
          <> help "Write log messages to the given file. Use '-' to write to stderr." ) )

setupLogger :: Maybe FilePath -> IO ()
setupLogger Nothing =
  updateGlobalLogger rootLoggerName $ setHandlers ([] :: [LH.GenericHandler Handle])
setupLogger (Just path) = do
  let level = DEBUG
  logStream <- case path of
    "-" -> pure stderr
    _ -> openFile path AppendMode
  hSetEncoding logStream utf8

  logH <- LH.streamHandler logStream level

  let logHandle  = logH { LH.closeFunc = hClose }
      logFormat  = LF.tfLogFormatter "%H:%M:%S%q" "[$time] [$tid/$prio] [$loggername] $msg"
      logHandler = LH.setFormatter logHandle logFormat

  updateGlobalLogger rootLoggerName $
    setLevel level . setHandlers [logHandler]

main :: IO ()
main = do
  setLocaleEncoding utf8
  Args log <- execParser argParser
  setupLogger log
  exitWith =<< run
