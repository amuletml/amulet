{-# LANGUAGE RankNTypes, OverloadedStrings #-}
module Main where

import Text.Parsec

import System.Environment

import qualified Data.Text.IO as T
import qualified Data.Text as T
import qualified Data.Map as M

import Control.Monad.Infer

import Text.Show.Pretty

import Backend.Compile

import Types.Infer

import Syntax.Resolve
import Syntax.Desugar
import Syntax

import Optimise.Collect

import Errors
import Parser
import Pretty

data CompileResult = CSuccess ([Toplevel Typed], Env)
                   | CParse   ParseError
                   | CResolve ResolveError
                   | CInfer   TypeError

compile :: SourceName -> T.Text -> CompileResult
compile name x =
  case parse program name x of
    Right parsed -> runGen $ do
      desugared <- desugarProgram parsed
      resolved <- resolveProgram desugared
      case resolved of
        Right resolved -> do
          infered <- inferProgram resolved
          case infered of
            Right prg -> pure (CSuccess prg)
            Left e -> pure (CInfer e)
        Left e -> pure (CResolve e)
    Left e -> CParse e

compileFromTo :: FilePath
              -> T.Text
              -> (forall a. Pretty a => a -> IO ())
              -> IO ()
compileFromTo fp x emit =
  case compile fp x of
    CSuccess (prg, env) -> emit (compileProgram env prg)
    CParse e -> print e
    CResolve e -> putStrLn "Resolution error" >> report e x
    CInfer e -> putStrLn "Type error" >> report e x

test :: String -> IO ()
test x = do
  putStrLn "\x1b[1;32mProgram:\x1b[0m"
  case compile "<test>" (T.pack x) of
    CSuccess (prog, env) -> do
      let info = tally prog
      pPrint info
      putStrLn (x <> "\x1b[1;32mType inference:\x1b[0m")
      forM_ (M.toList $ values (difference env builtinsEnv)) $ \(k, t) ->
        T.putStrLn (prettyPrint k <> " : " <> prettyPrint t)
    CParse e -> print e
    CResolve e -> report e (T.pack x)
    CInfer e -> report e (T.pack x)

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
