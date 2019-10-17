{-# LANGUAGE FlexibleContexts #-}
module Test.Syntax.Resolve (tests) where

import Test.Util

import Control.Monad.Infer
import Control.Monad.State
import Control.Lens

import qualified Data.Text.Lazy as L
import qualified Data.Text as T

import Parser.Wrapper (runParser)
import Parser

import Syntax.Resolve (ResolveResult(..))

import qualified Text.Pretty.Note as N
import Text.Pretty.Semantic

import Frontend.Driver
import Frontend.Errors

result :: String -> T.Text -> IO T.Text
result file contents
  = flip evalNameyT firstName
  . flip evalStateT emptyDriver
  $ do
  let parsed = requireJust file contents $ runParser file (L.fromStrict contents) parseTops
  (resolved, errors) <- resolve "tests/resolve" parsed

  files <- ((file, contents):) <$> (fileMap =<< get)
  let fmt :: N.Note a Style => [a] -> [N.NoteDoc Style]
      fmt = map (N.format (N.fileSpans files N.defaultHighlight))
  pure . displayPlainVerbose . vsep . concat $
    [ maybe [] (pure . fmap Right . pretty . program) resolved
    , fmt (errors ^. parseErrors)
    , fmt (errors ^. resolveErrors)
    ]

tests :: IO TestTree
tests = testGroup "Tests.Syntax.Resolve" <$> goldenDirM result "tests/resolve/" ".ml"
