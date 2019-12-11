{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TypeFamilies, MultiWayIf #-}

module Amc.Repl
  ( repl
  , replFrom
  , runRemoteReplCommand
  ) where

import qualified Data.Text.Lazy as L
import qualified Data.Text.IO as T
import qualified Data.Text as T

import System.Console.Haskeline hiding (display, bracket, throwTo)
import System.IO

import Parser.Token
import Parser.Error
import Parser.Wrapper (runParser)

import qualified Network.Socket as Net

import Control.Monad.State.Strict
import Control.Exception
import Control.Concurrent

import Amc.Repl.Server
import Amc.Repl.State
import Amc.Repl.Eval
import Amc.Repl.Command

runRepl :: Listener -> InputT (StateT ReplState IO) ()
runRepl tid = do
  line <- getInputLine "> "
  case line of
    Nothing -> finish tid
    Just "" -> runRepl tid
    Just (':':cmd) -> do
      lift $ uncurry (execCommand tid) . span (/=' ') $ cmd
      runRepl tid
    Just line -> getInput line False >>= (lift . execString "=stdin" . T.pack) >> runRepl tid

  where
    getInput :: String -> Bool -> InputT (StateT ReplState IO) String
    getInput input empty =
      case runParser "=stdin" (L.pack input) parseRepl' of
        (Nothing, xs) ->
          case last xs of
            UnclosedString{} -> continue
            UnclosedComment{} -> continue
            UnexpectedEnd{} -> continue
            UnexpectedToken (Token TcEOF _ _) _ -> continue
            _ -> pure input
        _ -> pure input
        where continue = do
                line <- getInputLine ". "
                case line of
                  Nothing -> pure input
                  Just "" | empty -> pure input
                  Just line -> getInput (input ++ '\n':line) (line == "")

runRemoteReplCommand :: Int -> String -> IO ()
runRemoteReplCommand port command = Net.withSocketsDo $ do
  sock <- Net.socket Net.AF_INET Net.Stream Net.defaultProtocol
  r <- try $ Net.connect sock (Net.SockAddrInet (fromIntegral port) $ Net.tupleToHostAddress (127, 0, 0, 1))
  case r of
    Right () -> do
      handle <- Net.socketToHandle sock ReadWriteMode
      hSetEncoding handle utf8
      T.hPutStrLn handle (T.pack command)
      T.hPutStrLn handle mempty
      T.putStr =<< T.hGetContents handle
    Left (_ :: SomeException) ->
      putStrLn $ "Failed to connect to server on port " ++ show port


repl :: ReplConfig -> IO ()
repl config = replFrom config Nothing

replFrom :: ReplConfig -> Maybe FilePath -> IO ()
replFrom config file = do
  state <- execStateT (loadFile file) =<< defaultState config
  hSetBuffering stdout LineBuffering

  ready <- newEmptyMVar
  tid <-
    if port config /= 0
       then forkIO $ startServer ready (port config) state
       else do
         putMVar ready ()
         myThreadId

  takeMVar ready
  bracket (pure ()) (const (killThread tid)) $ \() ->
    evalStateT (runInputT (completeInScope `setComplete` defaultSettings) (runRepl (Just tid))) state

