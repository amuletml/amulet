module Main where

import Parser
import Text.Parsec

main :: IO ()
main = do
  x <- getLine
  parseTest exprP x
