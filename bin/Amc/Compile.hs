{-# LANGUAGE NamedFieldPuns, RankNTypes #-}
module Amc.Compile
  ( Optimise(..)
  , Options(..)
  , compileFile
  , watchFile
  , compileViaChicken
  ) where

import System.Environment
import System.Directory
import System.FilePath
import System.FSNotify
import System.Process
import System.Exit
import System.IO

import Control.Monad.Infer (firstName)
import Control.Monad.Namey
import Control.Monad.State
import Control.Concurrent
import Control.Exception

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Position (SourceName)
import qualified Data.Text as T
import Data.Bifunctor
import Data.Foldable
import Data.Functor
import Data.Maybe
import Data.List

import Backend.Scheme
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
  path <- canonicalizePath (T.unpack file)
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

  fileNames <- traverse (canonicalizePath . T.unpack . fst) files
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

compileViaChicken :: Options -> D.DriverConfig -> SourceName -> String -> IO ()
compileViaChicken opt config file output = go (D.makeDriverWith config, firstName) opt where
  lintIt name = if lint opt then runLint name else flip const

  go :: (D.Driver, Name) -> Options -> IO ()
  go (driver, name) Options { optLevel, lint, debug } = do
    path <- canonicalizePath (T.unpack file)
    ((((core, errors), _), driver), name) <-
        flip runNameyT name
      . flip runStateT driver
      $ (,) <$> D.compiles path <*> D.getSignature path

    files <- D.fileMap driver
    reportAllS files errors

    base_ss <- D.locateSchemeBase config
    base_ss <- case base_ss of
      Just s -> pure s
      Nothing -> throwIO (userError "Couldn't locate base.ss file")

    scm <- case core of
      Nothing -> pure mempty
      Just core -> do
        let info = defaultInfo { useLint = lint, exportNames = mempty }
            optimised = flip evalNamey name $ case optLevel of
              Opt -> optimise info core
              NoOpt -> do
                lintIt "Lower" (checkStmt emptyScope core) (pure ())
                (lintIt "Optimised"  =<< checkStmt emptyScope) . deadCodePass info <$> reducePass info core
            lua = compileProgram Nothing optimised
            chicken = genScheme optimised
        D.dumpCore debug core optimised lua
        pure chicken

    (path, temp_h) <- openTempFile "." "amulet.ss"
    hPutDoc temp_h scm
    hClose temp_h

    chicken <- getChicken
    let chicken_process = proc chicken [ "-prologue", base_ss
                                       , "-uses", "library"
                                       , "-x", "-strict-types"
                                       , "-strip", "-static"
                                       , path
                                       , "-o", output
                                       ]
    setEnv "CHICKEN_OPTIONS" "-emit-link-file /dev/null"
    (_, _, _, handle) <-
      createProcess (chicken_process { std_out = Inherit
                                     , std_in = NoStream
                                     , std_err = Inherit
                                     })

    code <- waitForProcess handle

    -- removePathForcibly path

    case code of
      ExitSuccess   -> pure ()
      ExitFailure _ -> exitWith code

getChicken :: IO String
getChicken = do
  x <- lookupEnv "CHICKENC"
  pure $ fromMaybe "chicken-csc" x
