{-# LANGUAGE TemplateHaskell #-}
module Amc.Explain
  ( findError
  , displayError
  , explainError
  ) where

import qualified Data.IntMap.Strict as Map

import Data.Functor

import Amc.Explain.TH

import Control.Exception

import System.Process
import System.Exit

import System.IO.Error
import System.IO

errors :: Map.IntMap String
errors = Map.fromList
  $(qEmbedFiles . map ("doc/errors/" ++) =<< qReadFilesList "doc/errors.txt")

-- | Locate an error given a specific id
findError :: Int -> Maybe String
findError = flip Map.lookup errors

displayError :: String -> IO ()
displayError msg = do
  let line_no = length (lines msg)
  if line_no <= 20 then putStr msg else
    withCreateProcess ((proc "less" ["-R"]) { std_in = CreatePipe }) (\stdin _ _ ph ->
      case stdin of
        Just handle -> do
          hPutStr handle msg
          hFlush handle
          hClose handle
          waitForProcess ph $> ()
        Nothing -> putStr msg)
    `catch` \ioe ->
      if isDoesNotExistError ioe
      then putStr msg
      else throwIO ioe


explainError :: Int -> IO ()
explainError code =
  case findError code of
    Just err -> displayError err >> exitSuccess
    Nothing -> do
      putStrLn $ "No explanation for error E" ++ show code
      exitFailure
