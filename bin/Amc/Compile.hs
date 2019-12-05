{-# LANGUAGE NamedFieldPuns, RankNTypes, DuplicateRecordFields,
   DisambiguateRecordFields #-}
module Amc.Compile
  ( Optimise(..)
  , Options(..), ChickenOptions(..), StaticOptions(..)
  , Emit
  , compileFile
  , watchFile
  , compileWithLua
  , compileWithChicken
  , compileStaticLua
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

import qualified Data.Text.Encoding as T
import qualified Data.ByteString as Bs
import qualified Data.Text.IO as T
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Text as T

import Data.Position (SourceName)
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

import qualified Amc.Compile.Shim as C
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

data StaticOptions = StaticOptions
  { keepLua  :: Maybe FilePath
  , keepC    :: Maybe FilePath
  , suseCC   :: (Maybe String, [String])
  , suseLD   :: (Maybe String, [String])
  , soutput  :: FilePath
  , luaImpl  :: String
  , isStatic :: Bool
  }

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

compileStaticLua :: D.DriverConfig -> StaticOptions -> Emit
compileStaticLua _ opt sig prog = do
  -- Generate the Lua code
  let lua = pretty $ compileProgram sig prog

  -- ... and print it, if the user asked to do so;
  case keepLua opt of
    Nothing -> pure ()
    Just "-" -> putDoc lua
    Just f -> T.writeFile f . display . renderPretty 0.4 100 $ lua

  -- Find the correct C compiler and linker flags for the Lua
  -- implementation the user chose.
  cflagsI <- words <$> readProcess "pkg-config" [ luaImpl opt, "--cflags-only-I" ] ""
  ldFlags <- words <$> readProcess "pkg-config"
               ([ luaImpl opt, "--libs" ] ++ [ "--static" | isStatic opt ])
               ""

  -- Create temporary files for the C compiler output
  (obj_file, temp_h) <- openTempFile "." "amulet.o"
  hClose temp_h

  -- ... and for our C shim.
  (path, temp_h) <- openTempFile "." "amulet.c"

  -- Don't forget to remove them!
  let cleanup = traverse_ removePathForcibly [ obj_file, path ]

  -- Print the C shim to the C temporary file.
  genCCode temp_h lua
  hClose temp_h

  -- and call the C compiler on it.
  cc_status <- withTimer ("C compiler for " ++ path) $ do
    let (cc_program, cc_args) = first (fromMaybe "gcc") (suseCC opt)
        cc_process = proc cc_program cc_cmdline
        cc_cmdline = [ "-c", path, "-O3", "-g", "-o", obj_file ] ++ cc_args ++ cflagsI
    (_, _, _, handle) <-
      createProcess (cc_process { std_out = Inherit
                                , std_in = NoStream
                                , std_err = Inherit })
    waitForProcess handle

  -- If the user asked to keep the C shim, then we do so here.
  case keepC opt of
    Just "-" -> do
      genCCode stdout lua
      removeFile path
    Just p -> renameFile path p
    _ -> removeFile path

  -- If the C compiler failed, then we fail as well.
  exitMaybe cleanup cc_status

  -- Call the linker.
  ld_status <- withTimer ("System linker for " ++ path) $ do
    let (ld_program, ld_args) = first (fromMaybe "gcc") (suseLD opt)
        ld_cmdline = [ "-o", soutput opt, obj_file ] ++ ld_args ++ ldFlags
        ld_process = proc ld_program ld_cmdline
    (_, _, _, handle) <-
      createProcess (ld_process { std_out = Inherit
                                , std_in = NoStream
                                , std_err = Inherit })
    waitForProcess handle

  -- If the linker failed, then we fail as well.
  exitMaybe cleanup ld_status

  cleanup
  pure lua

genCCode :: Handle -> Doc -> IO ()
genCCode h code = do
  let text = toText code
      bytes = Bs.unpack (T.encodeUtf8 text `Bs.snoc` 0)

  hPutStrLn h $ "static char program[" ++ show (length bytes) ++ "] = {" ++ intercalate "," (map show bytes) ++ "};"
  hPutStrLn h C.shim

exitMaybe :: IO () -> ExitCode -> IO ()
exitMaybe cleanup c =
  case c of
    ExitSuccess{} -> pure ()
    e -> cleanup *> exitWith e
