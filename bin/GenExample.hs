module Main where

import Control.Monad.Namey
import Control.Monad.Infer
import Control.Monad.State
import Control.Lens

import Parser.Wrapper
import Parser.Token
import Parser.Lexer

import System.Environment

import qualified Data.Text.IO as T
import qualified Data.Text as T

import Data.Position
import Data.Maybe

import Frontend.Driver
import Frontend.Errors

import GHC.IO.Encoding
import System.Exit
import System.IO

main :: IO ()
main = do
  setLocaleEncoding utf8
  [file] <- getArgs

  driver <- makeDriver
  (((core, errors), driver), _) <-
      flip runNameyT firstName
    . flip runStateT driver
    $ compiles file

  x <- T.readFile file

  fileMap <- fileMap driver

  case core of
    Nothing -> do
      putStrLn "<p style=\"color: red\">Error in generating example<pre>"
      hReportAll stdout Amc fileMap errors
      putStrLn "</pre></p>"
      exitSuccess
    Just _ -> pure ()

  case runLexerTrivial (T.pack file) (x ^. lazy) lexerScan of
    (Just ts, _) -> putStrLn . toMarkdown (T.lines x) $ genTable ts
    (Nothing, _) -> error "Lexing error"

genTable :: [Token] -> [(T.Text, [Token])]
genTable ((Token (TcComment comment) _ _):xs) =
  let (ours, next) = break isComment xs
   in (comment, dropWhile isTrivial ours):genTable next
genTable (_:_) = error "Error in example input"
genTable [] = []

toMarkdown :: [T.Text] -> [(T.Text, [Token])] -> String
toMarkdown lines = foldMap go where
  go (comment, span) =
    let Token _ (SourcePos _ firstLine _) _ = head span
        Token _ _ (SourcePos _ lastLine _) = last span
        cmnt = T.drop 2 (T.take (T.length comment - 2) comment)
     in
      unlines (
        [ "<div class=explanation>"
        , T.unpack cmnt
        , "</div>"
        , "<div class=code-cell>"
        , "```amulet"
        ]
      ++ map T.unpack (mapMaybe (\x -> lines ^? ix x) [firstLine-1..lastLine-1])
      ++ [ "```"
         , "</div></tr>" ])

isComment :: Token -> Bool
isComment (Token TcComment{} _ _) = True
isComment _ = False

isTrivial :: Token -> Bool
isTrivial (Token TcComment{} _ _) = True
isTrivial (Token TcWhitespace{} _ _) = True
isTrivial _ = False
