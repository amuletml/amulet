{-# LANGUAGE RankNTypes, OverloadedStrings, ScopedTypeVariables, FlexibleContexts, TemplateHaskell, NamedFieldPuns #-}
module Main(main) where

import System.Exit (ExitCode(..), exitWith)
import System.IO (hPutStrLn, stderr)
import System.Directory

import Control.Monad.Infer (firstName)
import Control.Monad.Namey
import Control.Monad.State

import qualified Data.Text.IO as T
import qualified Data.Text as T
import Data.Traversable

import Options.Applicative hiding (ParseError)

import Text.Pretty.Semantic hiding (empty)

import qualified Frontend.Driver as D
import Frontend.Errors

import qualified Amc.Debug as D
import qualified Amc.Repl as R
import qualified Amc.Compile as C

import Version

data DoOptimise = NoOpt | Opt
  deriving Show

newtype DoLint = DoLint Bool
  deriving Show

newtype DoExport = DoExport Bool
  deriving Show

data Prelude = NoPrelude | DefaultPrelude | CustomPrelude String
  deriving Show

data CompilerOptions = CompilerOptions
  { debugMode   :: D.DebugMode
  , libraryPath :: [String]
  , coreLint    :: Bool
  }
  deriving (Show)

data Command
  = Compile
    { input       :: FilePath
    , output      :: Maybe FilePath
    , optLevel    :: Int
    , export      :: Bool
    , watch       :: Bool
    , options     :: CompilerOptions
    }
  | Repl
    { toLoad      :: Maybe FilePath
    , serverPort  :: Int
    , prelude     :: Prelude
    , options     :: CompilerOptions
    }
  | Connect
    { remoteCmd   :: String
    , serverPort  :: Int
    }
  deriving (Show)

newtype Args
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
      $ long "version" <> short 'v' <> help "Show version information"

    args :: Parser Args
    args = Args <$> command'

    command' :: Parser Command
    command' =
      hsubparser
      (  command "compile"
         ( info compileCommand
         $ fullDesc <> progDesc "Compile an Amulet file to Lua.")
      <> command "repl"
         ( info replCommand
         $ fullDesc <> progDesc "Launch the Amulet REPL." )
      <> command "connect"
         ( info connectCommand
         $ fullDesc <> progDesc "Connect to an already running REPL instance." )
      ) <|> pure (Repl Nothing defaultPort DefaultPrelude (CompilerOptions D.Void [] False))

    compileCommand :: Parser Command
    compileCommand = Compile
      <$> argument str (metavar "FILE" <> help "The file to compile.")
      <*> optional ( option str
           ( long "out" <> short 'o' <> metavar "FILE"
          <> help "Write the generated Lua to a specific file." ) )
      <*> option auto ( long "opt" <> short 'O' <> metavar "LEVEL" <> value 1 <> showDefault
                     <> help "Controls the optimisation level." )
      <*> switch (long "export" <> help "Export all declared variables in this module, returning them at the end of the program.")
      <*> switch (long "watch" <> help "After compiling, watch for further changes to the file and recompile it again.")
      <*> compilerOptions

    replCommand :: Parser Command
    replCommand = Repl
      <$> optional (argument str (metavar "FILE" <> help "A file to load into the REPL."))
      <*> option auto ( long "port" <> metavar "PORT" <> value defaultPort <> showDefault
                     <> help "Port to use for the REPL server." )
      <*> ( flag' NoPrelude (long "no-prelude" <> help "Do not load files with a prelude.")
        <|> option (CustomPrelude <$> str) ( long "prelude" <> metavar "PATH" <> help "Specify a custom prelude to use." )
        <|> pure DefaultPrelude )
      <*> compilerOptions

    connectCommand :: Parser Command
    connectCommand = Connect
      <$> argument str (metavar "COMMAND" <> help "The command to run on the remote REPL.")
      <*> option auto ( long "port" <> metavar "PORT" <> value defaultPort <> showDefault
                     <> help "Port the remote REPL is hosted on." )

    compilerOptions :: Parser CompilerOptions
    compilerOptions = CompilerOptions
      <$> ( flag' D.Test   (long "test" <> short 't' <> hidden <> help "Provides additional debug information on the output")
        <|> flag' D.TestTc (long "test-tc"           <> hidden <> help "Provides additional type check information on the output")
        <|> pure D.Void )
      <*> many (option str (long "lib" <> help "Add a folder to the library path"))
      <*> switch ( long "core-lint" <> hidden
                 <> help ( "Verify that Amulet's intermediate representation is well-formed. This is an internal debugging flag, "
                        ++ "and should only be used if you suspect there is a bug in Amulet." ) )

    optional :: Parser a -> Parser (Maybe a)
    optional p = (Just <$> p) <|> pure Nothing

    defaultPort :: Int
    defaultPort = 5478

driverConfig :: CompilerOptions -> IO D.DriverConfig
driverConfig CompilerOptions { debugMode = debug, libraryPath =  paths } = do
  paths <- sequence <$> for paths (\path -> do
    path' <- canonicalizePath path
    exists <- doesDirectoryExist path'
    pure $ if exists then Right path' else Left path)

  case paths of
    Left path -> do
      hPutStrLn stderr (path ++ ": No such directory")
      exitWith (ExitFailure 1)
    Right paths -> do
      config <- D.makeConfig
      pure config { D.libraryPath = paths ++ D.libraryPath config
                  , D.callbacks = D.dumpCallbacks debug }

findPrelude :: Prelude -> D.DriverConfig -> IO (Maybe FilePath)
findPrelude NoPrelude _ = pure Nothing
findPrelude (CustomPrelude path) _ = do
  wholePath <- canonicalizePath path
  exists <- doesFileExist wholePath
  unless exists $ do
    hPutStrLn stderr (path ++ ": No such file")
    exitWith (ExitFailure 1)

  pure (Just wholePath)
findPrelude DefaultPrelude config = do
  prelude <- D.locatePrelude config
  case prelude of
    Nothing -> do
      hPutStrLn stderr "Cannot locate prelude. Check your package path, or run using --no-prelude."
      exitWith (ExitFailure 1)
    Just prelude -> pure (Just prelude)

main :: IO ()
main = do
  options <- execParser argParser
  case options of
    Args Repl { toLoad, serverPort, prelude, options } -> do
      dConfig <- driverConfig options
      prelude <- findPrelude prelude dConfig
      root <- getCurrentDirectory
      R.replFrom R.ReplConfig { R.port = serverPort
                               , R.debugMode = debugMode options
                               , R.root = root
                               , R.driverConfig = dConfig
                               , R.prelude = prelude
                               , R.coreLint = coreLint options }
        toLoad
    Args Connect { remoteCmd, serverPort } -> R.runRemoteReplCommand serverPort remoteCmd

    Args Compile { input, output = Just output } | input == output -> do
      hPutStrLn stderr ("Cannot overwrite input file " ++ input)
      exitWith (ExitFailure 1)

    Args Compile { input, options = options@CompilerOptions { debugMode = D.TestTc } } -> do
      exists <- doesFileExist input
      if not exists
      then hPutStrLn stderr ("Cannot find input file " ++ input)
        >> exitWith (ExitFailure 1)
      else pure ()

      config <- driverConfig options
      path <- liftIO $ canonicalizePath input
      (errors, driver) <-
          flip evalNameyT firstName
        . flip runStateT (D.makeDriverWith config)
        $ D.getTypeEnv path >> D.getErrorsAll path
      files <- D.fileMap driver
      reportAllS files errors

    Args Compile { input, output, optLevel, export, options, watch } -> do
      exists <- doesFileExist input
      if not exists
      then hPutStrLn stderr ("Cannot find input file " ++ input)
        >> exitWith (ExitFailure 1)
      else pure ()

      let opts = C.Options
            { C.optLevel = if optLevel >= 1 then C.Opt else C.NoOpt
            , C.lint = coreLint options
            , C.export = export
            , C.debug = debugMode options
            }

          writeOut :: Pretty a => a -> IO ()
          writeOut = case output of
                   Nothing -> putDoc . pretty
                   Just f -> T.writeFile f . T.pack . show . pretty

      config <- driverConfig options
      if watch
      then C.watchFile   opts config input writeOut
      else C.compileFile opts config input writeOut
