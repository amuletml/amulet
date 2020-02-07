{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
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
result file contents = do
  driver <- makeDriver
  flip evalNameyT firstName . flip evalStateT driver $ do
    let name = T.pack file
        parsed = requireJust name contents $ runParser name (L.fromStrict contents) parseTops
    (resolved, errors) <- resolve "tests/resolve" parsed

    files <- ((name, contents):) <$> (fileMap =<< get)
    let fmt :: N.Note a Style => [a] -> [N.NoteDoc Style]
        fmt = map (N.format (N.fileSpans files N.defaultHighlight))
    pure . cleanup . displayPlainVerbose . vsep . concat $
      [ maybe [] (pure . fmap Right . pretty . program) resolved
      , fmt (errors ^. parseErrors)
      , fmt (errors ^. resolveErrors)
      ]

  where
    -- | Terrible function to drop any "Searched in" paths. Otherwise we
    -- end up with tests which depend on library paths.
    cleanup
      = T.unlines
      . filter (\line -> not ("    •" `T.isPrefixOf` line && ".ml" `T.isSuffixOf` line))
      . T.lines

tests :: IO TestTree
tests = testGroup "Tests.Syntax.Resolve" <$> goldenDirM result "tests/resolve/" ".ml"
