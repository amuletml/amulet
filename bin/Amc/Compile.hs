{-# LANGUAGE NamedFieldPuns, RankNTypes #-}
module Amc.Compile
  ( Optimise(..)
  , Options(..)
  , compileFile
  , watchFile
  ) where

import System.Directory
import System.FilePath
import System.FSNotify

import Control.Monad.Infer (firstName)
import Control.Monad.Namey
import Control.Monad.State
import Control.Concurrent

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Position (SourceName)
import Data.Bifunctor
import Data.Foldable
import Data.Functor
import Data.List

import Backend.Lua

import Syntax.Resolve.Scope (exportedNames)
import Core.Optimise.DeadCode (deadCodePass)
import Core.Optimise.Reduce (reducePass)
import Core.Simplify
import Core.Lint

import Syntax.Var

import Text.Pretty.Semantic hiding (empty)

import qualified Frontend.Driver as D
import Frontend.Errors

import qualified Amc.Debug as D

data Optimise = Opt | NoOpt
  deriving Show

data Options = Options
  { optLevel :: Optimise
  , lint     :: Bool
  , export   :: Bool
  , debug    :: D.DebugMode
  }
  deriving Show

compileIt :: (D.Driver, Name)
          -> Options -> SourceName -> (Doc -> IO ())
          -> IO ((D.Driver, Name), Set.Set FilePath)
compileIt (driver, name) Options { optLevel, lint, export, debug } file emit = do
  path <- canonicalizePath file
  ((((core, errors), sig), driver), name) <-
      flip runNameyT name
    . flip runStateT driver
    $ (,) <$> D.compiles path <*> D.getSignature path

  files <- D.fileMap driver
  reportAllS files errors

  case core of
    Nothing -> pure ()
    Just core -> do
      let sig' = if export then sig else Nothing
          info = defaultInfo { useLint = lint
                             , exportNames = foldMap exportedNames sig' }
          optimised = flip evalNamey name $ case optLevel of
            Opt -> optimise info core
            NoOpt -> do
              lintIt "Lower" (checkStmt emptyScope core) (pure ())
              (lintIt "Optimised"  =<< checkStmt emptyScope) . deadCodePass info <$> reducePass info core
          lua = compileProgram sig' optimised
      D.dumpCore debug core optimised lua
      emit (pretty lua)

  fileNames <- traverse (canonicalizePath . fst) files
  pure ((driver, name), Set.fromList fileNames)
  where
    lintIt name = if lint then runLint name else flip const

-- | Compile a file, passing the result to some "emitting function", such
-- as writing to a file or the terminal.
compileFile :: Options -> D.DriverConfig -> SourceName
            -> (Doc -> IO ())
            -> IO ()
compileFile opt config file emit =
  compileIt (D.makeDriverWith config, firstName) opt file emit $> ()

-- | Compile a file, and then watch for c
watchFile :: Options -> D.DriverConfig -> SourceName
          -> (Doc -> IO ())
          -> IO ()
watchFile opt config file emit = do
  chan <- newChan
  withManager (go mempty (D.makeDriverWith config, firstName) chan)

  where
    go :: Map.Map FilePath StopListening -> (D.Driver, Name) -> EventChannel -> WatchManager -> IO ()
    go dirs state channel mgr = do
      (state, files) <- compileIt state opt file emit

      -- Update the files we're currently watching.
      let newDirs = Set.map dropFileName files
      sequence_ (Map.withoutKeys dirs newDirs)
      dirs' <- foldrM
        (\dir dirs -> do
            putStrLn ("Watching " ++ quot dir)
            cancel <- watchDirChan mgr dir (const True) channel
            pure (Map.insert dir cancel dirs))
        dirs (newDirs Set.\\ Map.keysSet dirs)

      changes <- wait files channel
      putStrLn ("File(s) " ++ intercalate ", " (map quot changes) ++ " changed.")
      go dirs' (first (execState D.tock) state) channel mgr

    quot x = "'" ++ x ++ "'"

    wait :: Set.Set FilePath -> EventChannel -> IO [FilePath]
    wait files chan = do
      changed <- eventPath <$> readChan chan
      path <- liftIO $ canonicalizePath changed
      if Set.member path files
      then pure [path]
      else wait files chan
