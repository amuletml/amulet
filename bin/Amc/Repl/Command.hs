{-# LANGUAGE OverloadedStrings, FlexibleContexts, ScopedTypeVariables, ViewPatterns, TypeFamilies, MultiWayIf, TemplateHaskell #-}

module Amc.Repl.Command
  ( execCommand
  , Listener
  , finish
  ) where

import qualified Control.Monad.Infer as T
import Control.Monad.State.Strict
import Control.Exception
import Control.Monad.Namey
import Control.Concurrent
import Control.Lens

import qualified Data.ByteString as Bs
import qualified Data.Text.Lazy as L
import qualified Data.Text as T

import Data.Foldable
import Data.Position
import Data.Spanned
import Data.Maybe
import Data.Char
import Data.List

import Text.Read (readMaybe)

import System.Console.Haskeline hiding (display, bracket, throwTo)
import System.Directory
import System.IO

import Syntax.Resolve (ResolveResult(..), resolveProgram)
import Syntax.Resolve.Import (runNullImport)
import Syntax (displayType)
import qualified Syntax.Var as S
import qualified Syntax as S

import Parser
import Parser.Wrapper (runParser)

import Core.Simplify

import qualified Frontend.Driver as D
import Frontend.Errors

import Text.Pretty.Semantic

import CompileTargets (lua)

import Amc.Repl.State
import Amc.Repl.Eval
import Amc.Explain
import Version

type Listener = Maybe ThreadId

finish :: MonadIO m => Listener -> m ()
finish Nothing = pure ()
finish (Just i) = liftIO $ throwTo i ThreadKilled


execCommand :: (MonadState ReplState m, MonadIO m) => Listener -> String -> String -> m ()
execCommand tid "quit"  _   = finish tid
execCommand tid "q"     _   = finish tid

execCommand _ "h"       _   = helpCommand
execCommand _ "help"    _   = helpCommand

execCommand _ "l"       arg = loadCommand arg
execCommand _ "load"    arg = loadCommand arg

execCommand _ "r"       _   = reloadCommand
execCommand _ "reload"  _   = reloadCommand

execCommand _ "t"       arg = typeCommand arg
execCommand _ "type"    arg = typeCommand arg

execCommand _ "i"       arg = infoCommand arg
execCommand _ "info"    arg = infoCommand arg

execCommand _ "c"       arg = compileCommand arg
execCommand _ "compile" arg = compileCommand arg

execCommand _ "explain" arg = explainCommand arg

execCommand _ "add-library-path" arg =
  case dropWhile isSpace arg of
    [] -> liftIO $ putStrLn ":add-library-path needs an argument"
    dir -> do
      path <- liftIO $ canonicalizePath dir
      exists <- liftIO $ doesDirectoryExist path
      if exists
      then wrapDriver $ D.adjustConfig (\c -> c { D.libraryPath = path : D.libraryPath c })
      else liftIO . putStrLn $ arg ++ ": No such directory"

execCommand _ "version"  _   = liftIO (putStrLn ("The Amulet compiler, version " ++ $amcVersion))
execCommand _ "complete" arg = do
  let word = dropWhile isSpace arg
  (_, completions) <- completeInScope (reverse word, "")
  out <- gets outputHandle
  liftIO $
    for_ completions $ \(Completion rep _ _) ->
      hPutStrLn out rep

execCommand _ cmd _ = outputDoc ("Unknown command" <+> verbatim cmd <+> " Use :h for help.")

helpCommand :: (MonadState ReplState m, MonadIO m) => m ()
helpCommand =
  outputDoc "Available commands:\n\
    \ :q[uit]                 Quit\n\
    \ :l[oad]    <file>       Import definitions from <file>\n\
    \ :r[eload]               Reload all loaded modules\n\
    \ :t[ype]    <expr>       Show the type of <expr>\n\
    \ :i[nfo]    <var>        Prints the type of <var> as it can be found in the environment\n\
    \ :c[ompile] <file>       Compiles the current environment to <file>\n\
    \ :explain   <err>        Explain an error message.\n\
    \ :add-library-path <dir> Adds the directory <dir> to the library path\n\
    \ :complete  <expr>       Shows possible completions for <expr>"

loadCommand :: (MonadState ReplState m, MonadIO m) => String -> m ()
loadCommand arg =
  case parseArgs arg of
    [file] -> loadFile (Just file)
    _ -> outputDoc "Usage `:load [file]`"

reloadCommand :: (MonadState ReplState m, MonadIO m) => m ()
reloadCommand = loadFile =<< gets currentFile

infoCommand :: (MonadState ReplState m, MonadIO m) => String -> m ()
infoCommand (T.pack . dropWhile isSpace -> input) = do
  state <- get
  let files :: [(SourceName, T.Text)]
      files = [("<input>", input)]
      (parsed, parseMsg) = runParser "<input>" (L.fromStrict input) parseInfoVar
      handle = outputHandle state
  liftIO $ traverse_ (hReport (outputHandle state) files) parseMsg
  case parsed of
    Nothing -> pure ()
    Just var -> do
      let prog :: [S.Toplevel S.Parsed]
          prog = [ S.LetStmt S.NonRecursive S.Public
                   [ S.Binding (S.Name "_") (spanOf var)
                        (S.VarRef (getL var) (spanOf var))
                        True (spanOf var) ] (spanOf var) ]

      resolved <-
          flip evalNameyT (lastName state)
        . runNullImport
        $ resolveProgram lua (resolveScope state) prog

      case resolved of
        Right (ResolveResult [ S.LetStmt _ _ [S.Binding _ _ (S.VarRef name _) _ _] _ ] _ _) ->
          liftIO . hPutDoc handle . displayType $
            (inferScope state ^. T.names . at name . non undefined)
        _ -> liftIO . hPutDoc handle $ "Name not in scope:" <+> pretty (getL var)

typeCommand :: (MonadState ReplState m, MonadIO m) => String -> m ()
typeCommand (dropWhile isSpace -> input) = do
  state <- get
  let files :: [(SourceName, T.Text)]
      files = [("<input>", T.pack input)]
      (parsed, parseMsg) = runParser "<input>" (L.pack input) parseReplExpr
      handle = outputHandle state
  liftIO $ traverse_ (hReport (outputHandle state) files) parseMsg
  case parsed of
    Nothing -> pure ()
    Just parsed -> do
      let ann = spanOf parsed
          prog :: [S.Toplevel S.Parsed]
          prog = [ S.LetStmt S.NonRecursive S.Public [ S.Matching (S.Wildcard ann) parsed ann ] ann ]

      (infer, es) <- wrapDriver $ do
        D.tick
        D.inferWith (root (config state)) prog (resolveScope state) (inferScope state)
      hReportAll (outputHandle state) Repl files es
      case infer of
        Nothing -> pure ()
        Just (prog, _, _) ->
          let ~[S.LetStmt _ _ [ S.TypedMatching _ expr _ _ ] _] = prog
              t = S.getType expr
          in liftIO $ hPutDoc handle (string input <+> colon <+> displayType t)

compileCommand :: (MonadState ReplState m, MonadIO m) => FilePath -> m ()
compileCommand [] = liftIO $ putStrLn ":compile command needs an argument"
compileCommand (dropWhile isSpace -> path) = do
  current <- gets currentFile
  output <- gets outputHandle
  files <- D.fileMap =<< gets driver
  case current of
    Just file -> do
      in_p <- liftIO $ canonicalizePath file
      (core, errors) <- wrapDriver $ D.tick >> D.compiles in_p
      handle <- liftIO $ openFile path WriteMode

      case core of
        Just core -> do
          lint <- gets (coreLint . config)
          optm <- wrapNamey $ optimise (defaultInfo { useLint = lint }) core
          (_, lua) <- emitCore optm
          liftIO $ Bs.hPutStr handle lua
        Nothing ->
          hReportAll output Repl files errors
      liftIO $ hClose handle
    Nothing -> liftIO $ putStrLn "No file loaded"

explainCommand :: MonadIO m => String -> m ()
explainCommand arg
  | Just code <- readMaybe . dropWhile isSpace . dropWhileEnd isSpace $ arg
  = liftIO $ case findError code of
    Just err -> displayError err
    Nothing -> putStrLn $ "No explanation for error E" ++ show code
  | "" <- arg = liftIO $ putStrLn "Expected error code"
  | otherwise = liftIO $ putStrLn "Cannot parse error code (should just be a number)"

-- | Split a string into arguments
parseArgs :: String -> [String]
parseArgs xs =
  let (ys, y) = parseArgs' xs
      parseArgs' "" = ([], Nothing)
      parseArgs' (' ':xs) = (parseArgs xs, Nothing)
      parseArgs' ('\\':x:xs) = Just . (x:) . fromMaybe [] <$> parseArgs' xs
      parseArgs' (x:xs) = Just . (x:) . fromMaybe [] <$> parseArgs' xs
   in maybe ys (:ys) y
