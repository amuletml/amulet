{-# LANGUAGE OverloadedStrings #-}
module Test.Core.Lint (tests) where

import qualified Data.Text as T
import Data.List

import Control.Monad.Infer (firstName)
import Control.Monad.State
import Control.Monad.Namey

import Core.Simplify
import Core.Lint

import Text.Pretty.Semantic

import Test.Tasty.HUnit
import Test.Tasty

import System.Directory

import Frontend.Driver
import Frontend.Errors
import CompileTarget

data Mode = Strict | Lax
  deriving Eq

testLint :: Mode -> String -> Assertion
testLint mode file = do
  libPath <- makeRelativeToCurrentDirectory "lib"
  path <- makeRelativeToCurrentDirectory file
  let driver = makeDriverWith DriverConfig
        { libraryPath = [ libPath ]
        , callbacks = defaultCallbacks
        , checkOnly = False
        , target = lua }
  (((core, errors), driver), name) <-
      flip runNameyT firstName
    . flip runStateT driver
    $ compiles path
  case core of
    Just core | mode == Lax || not (hasErrors defaultFilter { filterAll = True } errors) -> do
      let core' = evalNamey (optimise defaultInfo { useLint = True } core) name
      case runLintOK (checkStmt emptyScope core') of
        Nothing -> pure ()
        Just (_, es) -> assertFailure $ "Core lint failed: " ++ displayS (pretty es)
    _ -> do
      files <- fileMap driver
      assertFailure . T.unpack . display $ reportAll files errors

tests :: IO TestTree
tests = do
  folderLint <- map (testCase <*> testLint Lax . ("tests/lint/"++)) . sort <$> listDirectory "tests/lint/"

  pure $ testGroup "Test.Core.Lint"
    [ testGroup "Examples" (map (testCase <*> testLint Strict . ("examples/"++)) files)
    , testGroup "Test folder" folderLint
    ]

files :: [String]
files =
  [ "gadt/vector.ml"
  , "gadt/existential.ml"
  , "gadt/term.ml"

  , "class/lens.ml"
  , "class/profunctors.ml"
  , "class/quantified.ml"

  , "amulet-logo.ml"
  , "church-lists.ml"
  , "coroutines.ml"
  , "guess.ml"
  , "id.ml"
  , "lazy-list.ml"
  , "lists.ml"
  , "mini-servant.ml"
  , "modules.ml"
  , "peano.ml"
  , "pipe.ml"
  , "polymorphic-recursion.ml"
  ]
