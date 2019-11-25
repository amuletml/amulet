{-# LANGUAGE FlexibleContexts #-}

{- | A helper tool for testing the context system. This lexes the provided
   file(s) and prints out each token, plus the active context stack.
-}
module Main where

import System.Environment

import Control.Monad.Writer
import Control.Arrow hiding ((<+>))

import qualified Data.Text.Lazy as L
import qualified Data.Text.IO as T
import qualified Data.Text as T
import Data.Foldable

import Parser.Wrapper (Token(..), runLexer)
import Parser.Lexer (lexerScan)
import Parser.Context
import Parser.Error

import qualified Text.Pretty.Ansi as A
import qualified Text.Pretty.Note as N
import Text.Pretty.Semantic

testLexer :: [(FilePath, T.Text)] -> IO ()
testLexer fs = for_ fs $ \(name, file) ->
  case runLexer (T.pack name) (L.fromStrict file) lexerScan of
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
      putDoc (string (take 10 (show tok' ++ repeat ' ')) <+> hsep (map pretty cs))
      go ts toks' cs
    go ts     (Working tok) cs =
      let (res, msg) = runWriter (handleContext tok cs)
      in traverse_ dispMsg (msg :: [ParseError]) >> uncurry (go ts) res

    dispMsg :: ParseError -> IO ()
    dispMsg = T.putStrLn
            . A.displayDecorated
            . fmap (either N.toAnsi toAnsi)
            . filterSimpleDoc (either (const True) uncommentFilter)
            . renderPretty 0.4 100
            . N.format (N.fileSpans (map (first T.pack) fs) N.defaultHighlight)

main :: IO ()
main = do
  files <- getArgs
  files' <- traverse T.readFile files
  _ <- testLexer (zip files files')
  pure ()
