module Main where

import Parser
import Text.Parsec
import Backend.Compile
import Pretty

import System.Environment
import System.Posix.Terminal (queryTerminal)
import System.Posix.IO (stdOutput)

import Control.Monad.Infer
import Types.Infer
import Syntax.Subst

main :: IO ()
main = do
  [x] <- getArgs
  c <- readFile x
  istty <- queryTerminal stdOutput
  case parse program x c of
    Right prg -> do
      let out = compileProgram prg
      if istty
         then putStrLn $ prettyPrint out
         else putStrLn $ uglyPrint out
    Left e -> print e
