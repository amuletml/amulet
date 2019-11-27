{-# LANGUAGE NamedFieldPuns, RankNTypes #-}
module Amc.Compile
  ( Optimise(..)
  , Options(..), ChickenOptions(..)
  , Emit
  , compileFile
  , watchFile
  , compileWithLua
  , compileWithChicken
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
import Control.Timing

import qualified Data.Text.IO as T
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Position (SourceName)
import qualified Data.Text as T
import Data.Bifunctor
import Data.Foldable
import Data.Functor
import Data.Maybe
import Data.List

import Syntax.Resolve.Scope (Signature, exportedNames)
import Core.Optimise.DeadCode (deadCodePass)
import Core.Optimise.Reduce (reducePass)
import Core.Core (Stmt)
import Core.Simplify
import Core.Lint
import Core.Var

import Backend.Scheme
import Backend.Lua

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

data ChickenOptions = ChickenOptions
  { keepScm       :: Maybe FilePath
  , useCC         :: (Maybe String, [String])
  , useLD         :: (Maybe String, [String])
  , staticChicken :: Bool
  , output        :: FilePath
  }
  deriving Show

type Emit = Maybe Signature -> [Stmt CoVar] -> IO Doc

compileIt :: (D.Driver, Name)
          -> Options
          -> SourceName
          -> Emit
          -> IO ()
          -> IO ((D.Driver, Name), Set.Set FilePath)
compileIt (driver, name) Options { optLevel, lint, export, debug } file emit exit = do
  path <- canonicalizePath (T.unpack file)
  ((((core, errors), sig), driver), name) <-
      flip runNameyT name
    . flip runStateT driver
    $ (,) <$> D.compiles path <*> D.getSignature path

  files <- D.fileMap driver
  reportAllS files errors

  case core of
    Nothing -> exit
    Just core -> do
      let sig' = if export then sig else Nothing
          info = defaultInfo { useLint = lint
                             , exportNames = foldMap exportedNames sig' }
          optimised = flip evalNamey name $ case optLevel of
            Opt -> optimise info core
            NoOpt -> do
              lintIt "Lower" (checkStmt emptyScope core) (pure ())
              (lintIt "Optimised"  =<< checkStmt emptyScope) . deadCodePass info <$> reducePass info core
      compiled <- emit sig' optimised
      D.dumpCore debug core optimised compiled

  fileNames <- traverse (canonicalizePath . T.unpack . fst) files
  pure ((driver, name), Set.fromList fileNames)
  where
    lintIt name = if lint then runLint name else flip const

-- | Compile a file, passing the result to some "emitting function", such
-- as writing to a file or the terminal.
compileFile :: Options -> D.DriverConfig -> SourceName -> Emit
            -> IO ()
compileFile opt config file emit =
  compileIt (D.makeDriverWith config, firstName) opt file emit exitFailure $> ()

-- | Compile a file, and then watch for c
watchFile :: Options -> D.DriverConfig -> SourceName -> Emit
          -> IO ()
watchFile opt config file emit = do
  chan <- newChan
  withManager (go mempty (D.makeDriverWith config, firstName) chan)

  where
    go :: Map.Map FilePath StopListening -> (D.Driver, Name) -> EventChannel -> WatchManager -> IO ()
    go dirs state channel mgr = do
      (state, files) <- compileIt state opt file emit (pure ())

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

compileWithLua :: Maybe FilePath -> Emit
compileWithLua file sig prog = do
  let lua = pretty $ compileProgram sig prog
  case file of
    Nothing -> putDoc lua
    Just f -> T.writeFile f . display . renderPretty 0.4 100 $ lua
  pure lua

compileWithChicken :: D.DriverConfig -> ChickenOptions -> Emit
compileWithChicken config opt _ prog = do
  let scm = genScheme prog

  (path, temp_h) <- openTempFile "." "amulet.ss"
  withTimer "Generating Scheme" $ hPutDoc temp_h scm
  hClose temp_h

  chicken <- getChicken

  base_ss <- D.locateSchemeBase config
  base_ss <- case base_ss of
    Just s -> pure s
    Nothing -> throwIO (userError "Couldn't locate the base.ss file")

  let chicken_process = proc chicken chicken_cmdline
      chicken_cmdline =
        [ "-prologue", base_ss
        , "-uses", "library"
        , "-x", "-strict-types"
        , "-strip"
        , path, "-o", output opt ]
        ++ case fst (useCC opt) of
             Just cc -> [ "-cc", cc ]
             Nothing -> []
        ++ case fst (useLD opt) of
             Just cc -> [ "-ld", cc ]
             Nothing -> []
        ++ (snd (useCC opt) >>= \x -> ["-C", x])
        ++ (snd (useLD opt) >>= \x -> ["-L", x])
        ++ [ "-static" | staticChicken opt ]

  code <- withTimer ("Chicken compiler for " ++ path) $ do
    setEnv "CHICKEN_OPTIONS" "-emit-link-file /dev/null"
    (_, _, _, handle) <-
      createProcess (chicken_process { std_out = Inherit
                                     , std_in = NoStream
                                     , std_err = Inherit
                                     })
    waitForProcess handle

  case keepScm opt of
    Just "-" -> do
      putStrLn "amc: Generated Scheme:"
      hPutDoc stdout scm
      removeFile path
    Just p -> renameFile path p
    _ -> removeFile path

  case code of
    ExitSuccess   -> pure (pretty scm)
    ExitFailure _ -> exitWith code

getChicken :: IO String
getChicken = do
  x <- lookupEnv "CHICKENC"
  pure $ fromMaybe "chicken-csc" x
