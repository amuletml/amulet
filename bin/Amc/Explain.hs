{-# LANGUAGE TemplateHaskell #-}
module Amc.Explain where

import qualified Data.IntMap.Strict as Map

import Amc.Explain.TH

import Control.Exception

import System.Process
import System.Exit

import System.IO.Error
import System.IO

errors :: Map.IntMap String
errors = Map.fromList
  $(qEmbedFiles . map ("doc/errors/" ++) =<< qReadFilesList "doc/errors.txt")

explainError :: Int -> IO ()
explainError code =
  case code `Map.lookup` errors of
    Just err -> do
      let line_no = length (lines err)
      if line_no > 20
         then
          withCreateProcess ((proc "less" ["-R"]) { std_in = CreatePipe }) (\stdin _ _ ph ->
            case stdin of
              Just handle -> do
                hPutStr handle err
                hFlush handle
                hClose handle
                exitWith =<< waitForProcess ph
              Nothing -> do
                putStr err
                exitSuccess)
            `catch`
              \ioe ->
                if isDoesNotExistError ioe
                   then do
                     putStr err
                     exitSuccess
                   else throwIO ioe
         else putStr err
      exitSuccess
    Nothing -> do
      putStrLn $ "No explanation for error E" ++ show code
      exitFailure
