{-# LANGUAGE TemplateHaskell #-}
module Amc.Explain.TH where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH

import System.FilePath

qReadFile :: FilePath -> Q String
qReadFile path = do
  qAddDependentFile path
  str <- qRunIO (readFile path)
  length str `seq` pure str

qReadFilesList :: FilePath -> Q [FilePath]
qReadFilesList path = do
  qAddDependentFile path
  contents <- qRunIO (readFile path)
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
