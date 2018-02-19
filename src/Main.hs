{-# LANGUAGE RankNTypes, OverloadedStrings, ScopedTypeVariables #-}
module Main where

import System.Environment

import qualified Data.ByteString.Builder as B
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Text as T
import Data.Position
import Data.Span

import Data.Foldable

import Control.Monad.Infer
import Control.Lens

import Backend.Compile

import Types.Infer

import Syntax.Resolve
import Syntax.Desugar
import Syntax

import Core.Simplify
import Core.Lower
import Core.Core

import Text.PrettyPrint.Leijen
import Errors
import Parser
import Parser.Wrapper

data CompileResult = CSuccess ([Toplevel Typed], [CoStmt], [CoStmt], Env)
                   | CParse   String Span
                   | CResolve ResolveError
                   | CInfer   TypeError


compile :: SourceName -> T.Text -> CompileResult
compile name x =
  case runParser name (B.toLazyByteString $ T.encodeUtf8Builder x) parseInput of
    POK _ parsed -> runGen $ do
      desugared <- desugarProgram parsed
      resolved <- resolveProgram desugared
      case resolved of
        Right resolved -> do
          infered <- inferProgram resolved
          case infered of
            Right (prog, env) -> do
              lower <- runReaderT (lowerProg prog) env
              optm <- optimise lower
              pure (CSuccess (prog, lower, optm, env))
            Left e -> pure (CInfer e)
        Left e -> pure (CResolve e)
    PFailed msg sp -> CParse msg sp

compileFromTo :: FilePath
              -> T.Text
              -> (forall a. Pretty a => a -> IO ())
              -> IO ()
compileFromTo fp x emit =
  case compile fp x of
    CSuccess (_, _, core, env) -> emit (compileProgram env core)
    CParse e s -> putStrLn "Parse error" >> report (pretty s <> colon <+> pretty e) x
    CResolve e -> putStrLn "Resolution error" >> report e x
    CInfer e -> putStrLn "Type error" >> report e x

test :: String -> IO (Maybe ([CoStmt], Env))
test x = do
  putStrLn "\x1b[1;32m(* Program: *)\x1b[0m"
  case compile "<test>" (T.pack x) of
    CSuccess (_, core, optm, env) -> do
      putStrLn x
      putStrLn "\x1b[1;32m(* Type inference: *)\x1b[0m"
      ifor_ (difference env builtinsEnv ^. values) . curry $ \(k :: Var Resolved, t :: Type Typed) ->
        putDoc (pretty k <> colon <> pretty t)
      putStrLn "\x1b[1;32m(* Kind inference: *)\x1b[0m"
      ifor_ (difference env builtinsEnv ^. types) . curry $ \(k, t) ->
        putDoc (pretty k <> colon <> pretty t)
      putStrLn "\x1b[1;32m(* Core lowering: *)\x1b[0m"
      traverse_ (putDoc . pretty) core
      putStrLn "\x1b[1;32m(* Optimised: *)\x1b[0m"
      traverse_ (putDoc . pretty) optm
      pure (Just (core, env))
    CParse e s -> Nothing <$ report (pretty s <> colon <+> pretty e) (T.pack x)
    CResolve e -> Nothing <$ report e (T.pack x)
    CInfer e -> Nothing <$ report e (T.pack x)

main :: IO ()
main = do
  ags <- getArgs
  case ags of
    [x] -> do
      x' <- T.readFile x
      compileFromTo x x' (putDoc . pretty)
    ["test", x] -> do
      x' <- readFile x
      _ <- test x'
      pure ()
    [x, t] -> do
      x' <- T.readFile x
      compileFromTo x x' $ T.writeFile t . T.pack . show . pretty
    [] -> error "REPL not implemented yet"
    _ -> do
      putStrLn "usage: amulet from.ml to.lua"
      putStrLn "usage: amulet from.ml"
      putStrLn "usage: amulet"
