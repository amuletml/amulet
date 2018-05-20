{-# LANGUAGE FlexibleContexts #-}
module Main where

import System.Environment

import Control.Monad.Writer

import qualified Data.Text.Lazy as L
import qualified Data.Text.IO as T
import qualified Data.Text as T
import Data.Foldable

import Parser.Wrapper (Token(..), runLexer)
import Parser.Lexer (lexerScan)
import Parser.Context
import Parser.Error

import qualified Text.Pretty.Semantic as S
import qualified Text.Pretty.Ansi as A
import qualified Text.Pretty.Note as N

testLexer :: [(FilePath, T.Text)] -> IO ()
testLexer fs = for_ fs $ \(name, file) ->
  case runLexer name (L.fromStrict file) lexerScan of
    (Just toks, es) -> do
      print (map (\(Token t _ _) -> t) toks)
      go toks Done defaultContext
      unless (null es) (print es)
    (Nothing, es) -> traverse_ dispMsg es

  where
    go :: [Token] -> PendingState -> [Context] -> IO ()
    go []     Done _  = pure ()
    go (tok:ts) Done cs =
      let (res, msg) = runWriter (handleContext tok cs)
      in traverse_ dispMsg (msg :: [ParseError]) >> uncurry (go ts) res

    go ts     (Result (Token tok' _ _) toks') cs = do
      putStrLn (take 10 (show tok' ++ repeat ' ') ++ show cs)
      go ts toks' cs
    go ts     (Working tok) cs =
      let (res, msg) = runWriter (handleContext tok cs)
      in traverse_ dispMsg (msg :: [ParseError]) >> uncurry (go ts) res

    dispMsg :: ParseError -> IO ()
    dispMsg = T.putStrLn
            . A.displayDecorated
            . fmap (either N.toAnsi S.toAnsi)
            . S.filterSimpleDoc (either (const True) S.uncommentFilter)
            . S.renderPretty 0.4 100
            . N.format (N.fileSpans fs)

main :: IO ()
main = do
  files <- getArgs
  files' <- traverse T.readFile files
  _ <- testLexer (zip files files')
  pure ()
