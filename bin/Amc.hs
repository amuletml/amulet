{-# LANGUAGE RankNTypes, OverloadedStrings, ScopedTypeVariables, FlexibleContexts, TemplateHaskell, NamedFieldPuns #-}
module Main(main) where

import System.Exit (ExitCode(..), exitWith)
import System.IO (hPutStrLn, stderr, withFile, IOMode(..))
import System.Directory

import Control.Monad.Infer (firstName)
import Control.Monad.Namey
import Control.Monad.State
import Control.Timing

import qualified Data.Text as T
import Data.Traversable
import Data.Maybe

import Options.Applicative hiding (ParseError)

import Text.Pretty.Semantic hiding (empty)

import qualified Frontend.Driver as D
import Frontend.Errors

import qualified CompileTarget as CT

import qualified Amc.Debug as D
import qualified Amc.Repl as R
import qualified Amc.Compile as C
import Amc.Explain

import GHC.IO.Encoding

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
    , time        :: Maybe FilePath
    , options     :: CompilerOptions
    }
  | StaticGen
    { input       :: FilePath
    , cOutput     :: FilePath
    , keepLua     :: Maybe FilePath
    , keepCShim   :: Maybe FilePath
    , optLevel    :: Int
    , watch       :: Bool
    , time        :: Maybe FilePath
    , useLiblua   :: String
    , useCC       :: Maybe String
    , useLD       :: Maybe String
    , ccOptions   :: [String]
    , ldOptions   :: [String]
    , static      :: Bool
  | Native
    { input       :: FilePath
    , optLevel    :: Int
    , watch       :: Bool
    , time        :: Maybe FilePath
    , options     :: CompilerOptions
    }
  | Chicken
    { input       :: FilePath
    , cOutput     :: FilePath
    , optLevel    :: Int
    , watch       :: Bool
    , time        :: Maybe FilePath
    , keepScheme  :: Maybe FilePath
    , useCC       :: Maybe String
    , useLD       :: Maybe String
    , ccOptions   :: [String]
    , ldOptions   :: [String]
    , static      :: Bool
    , musl        :: Bool
    , dynamicCsc  :: Bool
    , options     :: CompilerOptions
    }
  | Repl
    { toLoad      :: Maybe FilePath
    , serverPort  :: Int
    , prelude     :: Prelude
    , noCode      :: Bool
    , options     :: CompilerOptions
    }
  | Connect
    { remoteCmd   :: String
    , serverPort  :: Int
    }
  | Explain { errId :: Int }
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
      <> command "native"
         ( info nativeCommand
         $ fullDesc <> progDesc "Compile an Amulet program to C.")
      <> command "chicken"
         ( info chickenCommand
         $ fullDesc <> progDesc "Compile an Amulet program to C, using Chicken Scheme.")
      <> command "static"
         ( info nativeCommand
         $ fullDesc <> progDesc "Compile an Amulet program to a native program that embeds the Lua interpreter.")
      <> command "repl"
         ( info replCommand
         $ fullDesc <> progDesc "Launch the Amulet REPL." )
      <> command "connect"
         ( info connectCommand
         $ fullDesc <> progDesc "Connect to an already running REPL instance." )
      <> command "explain"
         ( info explainCommand
         $ fullDesc <> progDesc "Explain an error message." )
      ) <|> pure (Repl Nothing defaultPort DefaultPrelude False (CompilerOptions D.Void [] False))

    explainCommand :: Parser Command
    explainCommand = Explain
      <$> argument auto (metavar "ERROR" <> help "The error message code to explain")

    compileCommand :: Parser Command
    compileCommand = Compile
      <$> argument str (metavar "FILE" <> help "The file to compile.")
      <*> optional ( option str
           ( long "out" <> short 'o' <> metavar "FILE"
          <> help "Write the generated Lua to a specific file." ) )
      <*> opt
      <*> switch (long "export" <> help "Export all declared variables in this module, returning them at the end of the program.")
      <*> watch <*> time
      <*> compilerOptions

    nativeCommand :: Parser Command
    nativeCommand = Native
      <$> argument str (metavar "FILE" <> help "The file to compile.")
      <*> opt
      <*> watch <*> time
      <*> compilerOptions

    chickenCommand :: Parser Command
    chickenCommand = Chicken
      <$> argument str (metavar "FILE" <> help "The file to compile.")
      <*> option str
           ( long "out" <> short 'o' <> metavar "FILE"
          <> help "Put the generated executable in this file"
          <> showDefault
          <> value "amulet.out" )
      <*> opt <*> watch <*> time
      <*> optional (option str
            ( long "keep-scheme" <> metavar "FILE" <> hidden
           <> help "Write the generated Scheme to a file. This is a debugging flag. Do not expect the generated Scheme to be readable."))
      <*> optional (option str
            ( long "cc" <> metavar "PROGRAM" <> hidden
           <> help "Use PROGRAM as the C compiler"))
      <*> optional (option str
            ( long "ld" <> metavar "PROGRAM" <> hidden
           <> help "Use PROGRAM as the object file linker"))
      <*> many (option str (short 'C' <> help "Pass an option to the C compiler"))
      <*> many (option str (short 'L' <> help "Pass an option to the linker"))
      <*> switch (long "static" <> short 's' <> help "Pass -static to the C compiler and linker")
      <*> switch (long "musl"
               <> help "Use musl-gcc for compilation. Implies --static")
      <*> switch (long "dynamic-libchicken"
               <> help "Link against the Chicken Scheme libraries dynamically.")
      <*> compilerOptions

    nativeCommand :: Parser Command
    nativeCommand = StaticGen
      <$> argument str (metavar "FILE" <> help "The file to compile.")
      <*> option str
           ( long "out" <> short 'o' <> metavar "FILE"
          <> help "Put the generated executable in this file"
          <> showDefault
          <> value "amulet.out" )
      <*> optional (option str
            ( long "keep-lua" <> metavar "FILE" <> hidden
           <> help "Write the generated Lua code to a file."))
      <*> optional (option str
            ( long "keep-shim" <> metavar "FILE" <> hidden
           <> help "Write the generated C shim to a file."))
      <*> opt <*> watch <*> time
      <*> option str
            ( long "lua" <> metavar "LUA" <> hidden
           <> help ("Use this Lua implementation. This is used as the name of the pkg-config package"
                 ++ "used to find the proper include paths and libraries for this Lua implementation.")
           <> value "lua" )
      <*> optional (option str
            ( long "cc" <> metavar "PROGRAM" <> hidden
           <> help "Use PROGRAM as the C compiler"))
      <*> optional (option str
            ( long "ld" <> metavar "PROGRAM" <> hidden
           <> help "Use PROGRAM as the object file linker"))
      <*> many (option str (short 'C' <> help "Pass an option to the C compiler"))
      <*> many (option str (short 'L' <> help "Pass an option to the linker"))
      <*> switch (long "static" <> short 's' <> help "Pass -static to the C compiler and linker")
      <*> compilerOptions

    replCommand :: Parser Command
    replCommand = Repl
      <$> optional (argument str (metavar "FILE" <> help "A file to load into the REPL."))
      <*> option auto ( long "port" <> metavar "PORT" <> value defaultPort <> showDefault
                     <> help "Port to use for the REPL server." )
      <*> ( flag' NoPrelude (long "no-prelude" <> help "Do not load files with a prelude.")
        <|> option (CustomPrelude <$> str) ( long "prelude" <> metavar "PATH" <> help "Specify a custom prelude to use." )
        <|> pure DefaultPrelude )
      <*> switch (long "no-code" <> help "Stop compilation of loaded modules after type-checking.")
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

    opt = option auto ( long "opt" <> short 'O' <> metavar "LEVEL" <> value 1 <> showDefault
                     <> help "Controls the optimisation level." )
    time = optional (option str
            ( long "time" <> metavar "FILE" <> hidden
           <> help "Write the self-timing report to a file. Use - for stdout."))
    watch = switch (long "watch" <> help "After compiling, watch for further changes to the file and recompile it again.")

    defaultPort :: Int
    defaultPort = 5478

driverConfig :: CompilerOptions -> IO D.DriverConfig
driverConfig CompilerOptions { debugMode = debug, libraryPath = paths } = do
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
  setLocaleEncoding utf8

  options <- execParser argParser
  case options of
    Args Repl { toLoad, serverPort, prelude, noCode, options } -> do
      dConfig <- driverConfig options
      prelude <- findPrelude prelude dConfig
      root <- getCurrentDirectory
      R.replFrom R.ReplConfig { R.port = serverPort
                               , R.debugMode = debugMode options
                               , R.root = root
                               , R.driverConfig = dConfig { D.checkOnly = noCode }
                               , R.prelude = prelude
                               , R.coreLint = coreLint options }
        toLoad
    Args Connect { remoteCmd, serverPort } -> R.runRemoteReplCommand serverPort remoteCmd

    Args Explain { errId } -> explainError errId

    Args Compile { input, output = Just output } | input == output -> do
      hPutStrLn stderr ("Cannot overwrite input file " ++ input)
      exitWith (ExitFailure 1)

    Args Compile { input, options = options@CompilerOptions { debugMode = D.TestTc }, time } -> do
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

      case time of
        Just "-" -> timingReport putDoc
        Just file ->
          withFile file WriteMode $ \h ->
            timingReport (hPutDoc h)
        Nothing -> pure ()

    Args Compile{ input, output, optLevel, export, options, watch, time } -> do
      exists <- doesFileExist input
      if not exists
      then hPutStrLn stderr ("Cannot find input file " ++ input)
        >> exitWith (ExitFailure 1)
      else pure ()

      config <- driverConfig options
      let opts = C.Options
            { C.optLevel = if optLevel >= 1 then C.Opt else C.NoOpt
            , C.lint = coreLint options
            , C.export = export
            , C.debug = debugMode options
            }

      compileOrWatch watch time opts config { D.target = CT.lua } (T.pack input) (C.compileWithLua output)

    Args Native{ input, optLevel, options, watch, time } -> do
      exists <- doesFileExist input
      if not exists
      then hPutStrLn stderr ("Cannot find input file " ++ input)
        >> exitWith (ExitFailure 1)
      else pure ()

      config <- driverConfig options
      let opts = C.Options
            { C.optLevel = if optLevel >= 1 then C.Opt else C.NoOpt
            , C.lint = coreLint options
            , C.export = False
            , C.debug = debugMode options
            }

      compileOrWatch watch time opts config { D.target = CT.native } (T.pack input)
        (C.compileWithLlvm Nothing)

    Args opt@Chicken{ input, cOutput, optLevel, options, time, keepScheme, useCC, ccOptions, useLD, ldOptions } -> do
      exists <- doesFileExist input
      if not exists
      then hPutStrLn stderr ("Cannot find input file " ++ input)
        >> exitWith (ExitFailure 1)
      else pure ()

      let opts = C.Options
            { C.optLevel = if optLevel >= 1 then C.Opt else C.NoOpt
            , C.lint = coreLint options
            , C.debug = debugMode options
            , C.export = False
            }

          copts = C.ChickenOptions
            { C.keepScm = keepScheme
            , C.useCC = (mkCc useCC, ccOptions ++ [ "-static" | static opt || musl opt ])
            , C.useLD = (useLD, ldOptions ++ [ "-static" | static opt || musl opt ])
            , C.staticChicken = not (dynamicCsc opt)
            , C.output = cOutput
            }
          mkCc c | musl opt = Just (fromMaybe "musl-gcc" c) | otherwise = c

      config <- driverConfig options
      compileOrWatch False time opts config { D.target = CT.scheme } (T.pack input)
        (C.compileWithChicken config copts)

    Args opt@StaticGen{ options } -> do
      exists <- doesFileExist (input opt)
      if not exists
      then hPutStrLn stderr ("Cannot find input file " ++ input opt)
        >> exitWith (ExitFailure 1)
      else pure ()

      let opts = C.Options
            { C.optLevel = if optLevel opt >= 1 then C.Opt else C.NoOpt
            , C.lint = coreLint options
            , C.debug = debugMode options
            , C.export = False
            }

          copts = C.StaticOptions
            { C.keepLua       = keepLua opt
            , C.keepC         = keepCShim opt
            , C.suseCC        = (useCC opt, ccOptions opt ++ [ "-static" | static opt ])
            , C.suseLD        = (useLD opt, ldOptions opt ++ [ "-static" | static opt ])
            , C.isStatic      = static opt
            , C.soutput       = cOutput opt
            , C.luaImpl       = useLiblua opt
            }

      config <- driverConfig options
      compileOrWatch False (time opt) opts config { D.target = CT.lua } (T.pack (input opt))
        (C.compileStaticLua config copts)

compileOrWatch :: Bool -> Maybe FilePath -> C.Options -> D.DriverConfig -> T.Text -> C.Emit -> IO ()
compileOrWatch True _ opts config source emit = C.watchFile opts config source emit
compileOrWatch False time opts config source emit = do
  C.compileFile opts config source emit
  case time of
    Just "-" -> timingReport putDoc
    Just file ->
      withFile file WriteMode (timingReport . hPutDoc)
    Nothing -> pure ()
