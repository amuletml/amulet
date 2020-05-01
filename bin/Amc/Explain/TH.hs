{-# LANGUAGE TemplateHaskell #-}
module Amc.Explain.TH where

import Control.Exception

import Language.Haskell.TH.Syntax
import Language.Haskell.TH

import System.FilePath
import System.IO

qReadFile :: FilePath -> Q String
qReadFile path = do
  qAddDependentFile path
  qRunIO $
    withFile path ReadMode $ \handle -> do
      hSetEncoding handle utf8
      s <- hGetContents handle
      _ <- evaluate (length s)
      pure s

qReadFilesList :: FilePath -> Q [FilePath]
qReadFilesList path = do
  qAddDependentFile path
  contents <- qRunIO $
    withFile path ReadMode $ \handle -> do
      hSetEncoding handle utf8
      s <- hGetContents handle
      _ <- evaluate (length s)
      pure s
  let files = lines contents
      fileP = map (head . words) files
  pure fileP

qEmbedFile :: FilePath -> ExpQ
qEmbedFile path = stringE =<< qReadFile path

qEmbedFiles :: [FilePath] -> ExpQ
qEmbedFiles = listE . go where
  go :: [FilePath] -> [ExpQ]
  go (p:ps) = [| ($(fileNo p), $(qEmbedFile p)) |] : go ps
  go [] = []

  fileNo :: FilePath -> ExpQ
  fileNo = lift . (read :: String -> Int) . takeBaseName
