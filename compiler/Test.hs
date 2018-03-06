module Test where

import System.IO (BufferMode(..), hSetBuffering, stdout, stderr)
import System.Exit (exitFailure)

import Control.Monad (unless)

import qualified Test.Types.Infer as Types

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  results <- sequence [ Types.tests ]

  unless (and results) exitFailure

