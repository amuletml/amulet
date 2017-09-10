module Main where

import Parser
import Text.Parsec
import Backend.Compile
import Pretty

import System.Environment
import System.Posix.Terminal (queryTerminal)
import System.Posix.IO (stdOutput)

import Types.Infer

main :: IO ()
main = do
  [x] <- getArgs
  c <- readFile x
  istty <- queryTerminal stdOutput
  case parse program x c of
    Right prg ->
      case inferProgram prg of
        Left e -> print e
        Right _ ->
          let out = compileProgram prg
           in if istty
                then putStrLn $ prettyPrint out
                else putStrLn $ uglyPrint out
    Left e -> print e
