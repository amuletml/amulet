{-# LANGUAGE RankNTypes, OverloadedStrings, ScopedTypeVariables, FlexibleContexts, TemplateHaskell, NamedFieldPuns #-}
module Main(main) where

import System.Exit (ExitCode(..), exitWith)
import System.IO (hPutStrLn, stderr)
import Options.Applicative hiding (ParseError)

import qualified Data.Text.IO as T
import qualified Data.Text as T
import Data.Position (SourceName)

import Control.Monad.Infer (Env, firstName)
import Control.Monad.Namey
import Control.Monad.State

import System.Directory

import Language.Lua.Syntax
import Backend.Lua

import qualified Syntax.Builtin as Bi

import Core.Optimise.Reduce (reducePass)
import Core.Optimise.Newtype (killNewtypePass)
import Core.Optimise.DeadCode (deadCodePass)
import Core.Simplify (optimise)
import Core.Core (Stmt)
import Core.Var (CoVar)

import Text.Pretty.Semantic hiding (empty)

import Frontend.Driver
import Frontend.Errors

import qualified Debug as D
import Repl

import Version

runCompile :: MonadIO m
           => DoOptimise -> SourceName
           -> m ( Maybe ( Env
                        , [Stmt CoVar]
                        , [Stmt CoVar]
                        , LuaStmt)
                , ErrorBundle
                , Driver )
runCompile opt file = do
  path <- liftIO $ canonicalizePath file
  (((env, core, errors), driver), name) <-
      flip runNameyT firstName
    . flip runStateT emptyDriver
    $ do
      (core, errors) <- compile path
      ~(Just env) <- getTypeEnv path
      pure (env, core, errors)

  pure $ case core of
    Nothing -> (Nothing, errors, driver)
    Just core ->
      let optimised = flip evalNamey name $ case opt of
            Do -> optimise core
            Don't -> deadCodePass <$> (reducePass =<< killNewtypePass core)
          lua = compileProgram optimised
      in ( Just (env, core, optimised, lua)
         , errors
         , driver )

compileFromTo :: DoOptimise -> D.DebugMode
              -> FilePath
              -> (forall a. Pretty a => a -> IO ())
              -> IO ()
compileFromTo opt dbg file emit = do
  (compiled, errors, driver) <- runCompile opt file
  files <- fileMap driver
  reportAllS files errors
  case compiled of
    Just (env, core, opt, lua) -> do
      D.dump dbg [] core opt lua Bi.builtinEnv env
      emit lua
    Nothing -> pure ()

data DoOptimise = Do | Don't

data CompilerOptions = CompilerOptions
  { debugMode   :: D.DebugMode
  , libraryPath :: [String]
  }
  deriving (Show)

data Command
  = Compile
    { input       :: FilePath
    , output      :: Maybe FilePath
    , optLevel    :: Int
    , options     :: CompilerOptions
    }
  | Repl
    { toLoad      :: Maybe FilePath
    , serverPort  :: Int
    , options     :: CompilerOptions
    }
  | Connect
    { remoteCmd   :: String
    , serverPort  :: Int
    }
  deriving (Show)

data Args
  = Args
    { mainCommand :: Command
    }
  deriving (Show)

argParser :: ParserInfo Args
argParser = info (args <**> helper <**> version)
       $ fullDesc <> progDesc ("The Amulet compiler and REPL, version " ++ $(amcVersion))
  where
    version :: Parser (a -> a)
    version
      = infoOption $(amcVersion)
      $ long "--version" <> short 'v' <> help "Show version information"

    args :: Parser Args
    args = Args <$> command'

    command' :: Parser Command
    command' =
      ( hsubparser
      $  command "compile"
         ( info compileCommand
         $ fullDesc <> progDesc "Compile an Amulet file to Lua.")
      <> command "repl"
         ( info replCommand
         $ fullDesc <> progDesc "Launch the Amulet REPL." )
      <> command "connect"
         ( info connectCommand
         $ fullDesc <> progDesc "Connect to an already running REPL instance." )
      ) <|> pure (Repl Nothing defaultPort (CompilerOptions D.Void []))

    compileCommand :: Parser Command
    compileCommand = Compile
      <$> argument str (metavar "FILE" <> help "The file to compile.")
      <*> optional ( option str $
           ( long "out" <> short 'o' <> metavar "FILE"
          <> help "Write the generated Lua to a specific file." ) )
      <*> option auto ( long "opt" <> short 'O' <> metavar "LEVEL" <> value 1 <> showDefault
                     <> help "Controls the optimisation level." )
      <*> compilerOptions

    replCommand :: Parser Command
    replCommand = Repl
      <$> optional (argument str (metavar "FILE" <> help "A file to load into the REPL."))
      <*> option auto ( long "port" <> metavar "PORT" <> value defaultPort <> showDefault
                     <> help "Port to use for the REPL server." )
      <*> compilerOptions

    connectCommand :: Parser Command
    connectCommand = Connect
      <$> argument str (metavar "COMMAND" <> help "The command to run on the remote REPL.")
      <*> option auto ( long "port" <> metavar "PORT" <> value defaultPort <> showDefault
                     <> help "Port the remote REPL is hosted on." )

    compilerOptions :: Parser CompilerOptions
    compilerOptions = CompilerOptions
      <$> ( flag' D.Test   (long "test" <> short 't' <> help "Provides additional debug information on the output")
        <|> flag' D.TestTc (long "test-tc"           <> help "Provides additional type check information on the output")
        <|> pure D.Void )
      <*> pure []

    optional :: Parser a -> Parser (Maybe a)
    optional p = (Just <$> p) <|> pure Nothing

    defaultPort :: Int
    defaultPort = 5478

main :: IO ()
main = do
  options <- execParser argParser
  case options of
    Args (Repl { toLoad, serverPort, options }) -> replFrom serverPort (debugMode options) toLoad
    Args (Connect { remoteCmd, serverPort }) -> runRemoteReplCommand serverPort remoteCmd

    Args (Compile { input, output = Just output }) | input == output -> do
      hPutStrLn stderr ("Cannot overwrite input file " ++ input)
      exitWith (ExitFailure 1)

    Args (Compile { input, output, optLevel, options }) -> do
      exists <- doesFileExist input
      if not exists
      then hPutStrLn stderr ("Cannot find input file " ++ input)
        >> exitWith (ExitFailure 1)
      else pure ()

      let opt = if optLevel >= 1 then Do else Don't
          writeOut :: Pretty a => a -> IO ()
          writeOut = case output of
                   Nothing -> putDoc . pretty
                   Just f -> T.writeFile f . T.pack . show . pretty
      compileFromTo opt (debugMode options) input writeOut
