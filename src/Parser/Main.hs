module Main where

import System.Environment

import qualified Data.Text.Lazy as L
import qualified Data.Text.IO as T
import qualified Data.Text as T
import Data.Foldable

import Parser.Wrapper (ParseResult(POK, PFailed), Token(..), runLexer)
import Parser.Lexer (lexerScan)
import Parser.Context

testLexer :: [(FilePath, T.Text)] -> IO ()
testLexer fs = for_ fs $ \(name, file) ->
  case runLexer name (L.fromStrict file) lexerScan of
    POK _ toks -> do
      print (map (\(Token t _) -> t) toks)
      go toks Done defaultContext
    PFailed es -> print es

  where
    go :: [Token] -> PendingState -> [Context] -> IO ()
    go []     Done _  = pure ()
    go (t:ts) Done cs = uncurry (go ts) =<< handleContext t cs

    go ts     (Result (Token tok' _) toks') cs = do
      putStrLn (take 10 (show tok' ++ repeat ' ') ++ show cs)
      go ts toks' cs
    go ts     (Working tok) cs = uncurry (go ts) =<< handleContext tok cs

main :: IO ()
main = do
  files <- getArgs
  files' <- traverse T.readFile files
  _ <- testLexer (zip files files')
  pure ()
