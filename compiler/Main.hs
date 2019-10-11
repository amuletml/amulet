{-# LANGUAGE RankNTypes, OverloadedStrings, ScopedTypeVariables, FlexibleContexts #-}
module Main where

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
import Text.Pretty.Note

import Frontend.Driver
import Frontend.Errors

import qualified Debug as D
import Repl

runCompile :: MonadIO m => DoOptimise -> SourceName
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
      ~(Just env, _) <- getTypeEnv path
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

data CompilerOptions
  = CompilerOptions
    { debugMode   :: D.DebugMode
    , forceRepl   :: Bool
    , optLevel    :: Int
    , replCommand :: Maybe String
    , serverPort  :: Int

    , input       :: Maybe FilePath
    , output      :: Maybe FilePath
    }
  deriving (Show)

isError :: Note a b => a -> Bool
isError x = diagnosticKind x == ErrorMessage

flags :: ParserInfo CompilerOptions
flags = flip info (fullDesc <> progDesc "The Amulet compiler and REPL") . flip (<**>) helper $
  CompilerOptions
   <$> ( flag' D.Test   (long "test" <> short 't' <> help "Provides additional debug information on the output")
     <|> flag' D.TestTc (long "test-tc"           <> help "Provides additional type check information on the output")
     <|> pure D.Void )

   <*> switch ( long "repl" <> short 'r' <> help "Go to the REPL after loading each file" )

   <*> option auto ( long "opt" <> short 'O' <> metavar "LEVEL" <> value 1 <> help "Controls the optimisation level." )

   <*> (Just <$>
         option str
         ( long "client" <> short 'c' <> metavar "COMMAND"
        <> help "Connect to another running REPL to execute the command" )
     <|> pure Nothing)

   <*> option auto ( long "port" <> metavar "PORT" <> value 5478 <> help "Port to use for the REPL server. (Default: 5478)" )

   <*> (argument (Just <$> str) (metavar "FILE") <|> pure Nothing)
   <*> (Just <$>
         option str
         ( long "out" <> short 'o' <> metavar "FILE"
        <> help "Write the generated Lua to a specific file." )
     <|> pure Nothing)


main :: IO ()
main = do
  options <- execParser flags
  case options of
    CompilerOptions { replCommand = Just str, serverPort = i } -> runRemoteReplCommand i str
    CompilerOptions { debugMode = db, input = Nothing, serverPort = i } -> repl i db
    CompilerOptions { debugMode = db, forceRepl = True, input = file, serverPort = i } -> replFrom i db file

    CompilerOptions { output = Just out, input = Just file }
      | out == file -> do
          hPutStrLn stderr ("Cannot overwrite input file " ++ out)
          exitWith (ExitFailure 1)

    -- CompilerOptions { debugMode = db, optLevel = opt, output = Nothing, input = Just file } | db /= D.Void -> do
    --   let opt' = if opt >= 1 then Do else Don't
    --   _ <- test opt' db file
    --   pure ()

    CompilerOptions { debugMode = db, optLevel = opt, output = out, input = Just file } -> do
      let opt' = if opt >= 1 then Do else Don't
          out' :: Pretty a => a -> IO ()
          out' = case out of
                   Nothing -> putDoc . pretty
                   Just f -> T.writeFile f . T.pack . show . pretty
      compileFromTo opt' db file out'
