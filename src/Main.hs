{-# LANGUAGE RankNTypes, OverloadedStrings #-}
module Main where

import Parser
import Text.Parsec
import Backend.Compile

import Text.Show.Pretty
import Pretty

import System.Environment

import Types.Infer

import qualified Data.Text.IO as T
import qualified Data.Text as T

import qualified Data.Map as M

import Control.Monad.Infer

import Syntax

compileFromTo :: FilePath
              -> T.Text
              -> (forall a. Pretty a => a -> IO ())
              -> IO ()
compileFromTo fp x emit =
  case parse program fp x of
    Right prg ->
      case inferProgram prg of
        Left e -> print e
        Right (prg, _) ->
          let out = compileProgram prg
           in emit out
    Left e -> print e

tag :: Var Parsed -> Type Typed -> Var Typed
tag (Name v) t = TvName v t
tag (Refresh k a) t = TvRefresh (tag k t) a

test :: String -> IO ()
test x =
  case parse program "<test>" (T.pack x) of
    Right prg ->
      case inferProgram prg of
        Left e -> print e
        Right (prg, env) -> do
          _ <- forM (M.toList $ values env) $ \(k, t) ->
            T.putStrLn (prettyPrint k <> " : " <> prettyPrint t)
          let out = compileProgram prg
           in T.putStrLn (prettyPrint out)
    Left e -> print e


main :: IO ()
main = do
  ags <- getArgs
  case ags of
    [x] -> do
      x' <- T.readFile x
      compileFromTo x x' ppr
    ["dump-bits", x] -> do
      x' <- T.readFile x
      case parse program x x' of
        Right prg -> do
          pPrint prg
          case inferProgram prg of
            Left e -> pPrint e
            Right ts -> pPrint ts
        Left e -> print e
    [x, t] -> do
      x' <- T.readFile x
      compileFromTo x x' $ T.writeFile t . uglyPrint
    [] -> error "REPL not implemented yet"
    _ -> do
      putStrLn "usage: amulet from.ml to.lua"
      putStrLn "usage: amulet from.ml"
      putStrLn "usage: amulet"
