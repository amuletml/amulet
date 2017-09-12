{-# LANGUAGE RankNTypes #-}
module Main where

import Parser
import Text.Parsec
import Backend.Compile
import Pretty

import System.Environment

import Types.Infer

compileFromTo :: FilePath 
              -> String
              -> (forall a. Pretty a => a -> IO ())
              -> IO ()
compileFromTo fp x emit =
  case parse program fp x of
    Right prg ->
      case inferProgram prg of
        Left e -> print e
        Right _ ->
          let out = compileProgram prg
           in emit out
    Left e -> print e

main :: IO ()
main = do
  ags <- getArgs
  case ags of
    [x] -> do
      x' <- readFile x
      compileFromTo x x' ppr
    [x, t] -> do
      x' <- readFile x
      compileFromTo x x' $ writeFile t . uglyPrint
    [] -> error "REPL not implemented yet"
    _ -> do
      putStrLn "usage: amulet from.ml to.lua"
      putStrLn "usage: amulet from.ml"
      putStrLn "usage: amulet"
