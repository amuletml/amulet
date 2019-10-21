{-# LANGUAGE TemplateHaskell #-}
module Amc.Explain where

import qualified Data.IntMap.Strict as Map

import Amc.Explain.TH

import System.Exit

errors :: Map.IntMap String
errors = Map.fromList
  $(qEmbedFiles . map ("doc/errors/" ++) =<< qReadFilesList "doc/errors.txt")

explainError :: Int -> IO ()
explainError code =
  case code `Map.lookup` errors of
    Just err -> do
      putStr err
      exitSuccess
    Nothing -> do
      putStrLn $ "No explanation for error E" ++ show code
      exitFailure
