{-# LANGUAGE RankNTypes, OverloadedStrings #-}
module Main where

import Parser
import Text.Parsec
import Backend.Compile

import Pretty

import System.Environment

import Types.Infer

import qualified Data.Text.IO as T
import qualified Data.Text as T

import qualified Data.Map as M

import Control.Monad.Infer

import Errors

compileFromTo :: FilePath
              -> T.Text
              -> (forall a. Pretty a => a -> IO ())
              -> IO ()
compileFromTo fp x emit =
  case parse program fp x of
    Right prg ->
      case inferProgram prg of
        Left e -> report e x
        Right (prg, _) ->
          let out = compileProgram prg
           in emit out
    Left e -> print e

test :: String -> IO ()
test x = do
  putStrLn "\x1b[1;32mProgram:\x1b[0m"
  case parse program "<test>" (T.pack x) of
    Right prg ->
      case inferProgram prg of
        Left e -> report e (T.pack x)
        Right (_, env) -> do
          putStrLn (x <> "\x1b[1;32mType inference:\x1b[0m")
          forM_ (M.toList $ values (difference env builtinsEnv)) $ \(k, t) ->
            T.putStrLn (prettyPrint k <> " : " <> prettyPrint t)
    Left e -> print e

main :: IO ()
main = do
  ags <- getArgs
  case ags of
    [x] -> do
      x' <- T.readFile x
      compileFromTo x x' ppr
    ["test", x] -> do
      x' <- readFile x
      test x'
    [x, t] -> do
      x' <- T.readFile x
      compileFromTo x x' $ T.writeFile t . uglyPrint
    [] -> error "REPL not implemented yet"
    _ -> do
      putStrLn "usage: amulet from.ml to.lua"
      putStrLn "usage: amulet from.ml"
      putStrLn "usage: amulet"
