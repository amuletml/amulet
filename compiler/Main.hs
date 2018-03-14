{-# LANGUAGE RankNTypes, OverloadedStrings, ScopedTypeVariables #-}
module Main where

import System.Environment (getArgs)

import qualified Data.ByteString.Builder as B
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Text as T

import Control.Monad.Reader (runReaderT)
import Control.Monad.Gen (runGen)
import Control.Lens (ifor_, (^.))

import "amuletml" Data.Position (SourceName)
import "amuletml" Data.Span (Span)

import "amuletml" Control.Monad.Infer (Env, TypeError, difference, values, types)
import "amuletml" Backend.Compile (compileProgram)

import "amuletml" Types.Infer (inferProgram, builtinsEnv)

import "amuletml" Syntax.Resolve (ResolveError, resolveProgram)
import "amuletml" Syntax.Desugar (desugarProgram)
import "amuletml" Syntax.Pretty (tidyPrettyType)
import "amuletml" Syntax (Toplevel, Typed, Var, Resolved, Type)

import "amuletml" Core.Occurrence (OccursVar, tagOccursVar)
import "amuletml" Core.Simplify (optimise)
import "amuletml" Core.Lower (lowerProg)
import "amuletml" Core.Core (Stmt)

import "amuletml" Pretty (Pretty(pretty), putDoc, (<+>), colon)

import "amuletml" Parser.Wrapper (ParseResult(POK, PFailed), runParser)
import "amuletml" Parser (parseInput)

import Errors (reportP, reportR, reportI)


data CompileResult a
  = CSuccess ([Toplevel Typed], [Stmt (Var Resolved)], [Stmt a], Env)
  | CParse   String Span
  | CResolve ResolveError
  | CInfer   TypeError


compile :: SourceName -> T.Text -> CompileResult (OccursVar (Var Resolved))
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
              pure (CSuccess (prog, lower, tagOccursVar optm, env))
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
    CParse e s -> putStrLn "Parse error" >> reportP e s x
    CResolve e -> putStrLn "Resolution error" >> reportR e x
    CInfer e -> putStrLn "Type error" >> reportI e x

test :: String -> IO (Maybe ([Stmt (Var Resolved)], Env))
test x = do
  putStrLn "\x1b[1;32m(* Program: *)\x1b[0m"
  case compile "<test>" (T.pack x) of
    CSuccess (ast, core, optm, env) -> do
      putDoc (pretty ast)
      putStrLn "\x1b[1;32m(* Type inference: *)\x1b[0m"
      ifor_ (difference env builtinsEnv ^. values) . curry $ \(k :: Var Resolved, t :: Type Typed) ->
        putDoc (pretty k <+> colon <+> tidyPrettyType t)
      putStrLn "\x1b[1;32m(* Kind inference: *)\x1b[0m"
      ifor_ (difference env builtinsEnv ^. types) . curry $ \(k, t) ->
        putDoc (pretty k <+> colon <+> pretty t)
      putStrLn "\x1b[1;32m(* Core lowering: *)\x1b[0m"
      putDoc (pretty core)
      putStrLn "\x1b[1;32m(* Optimised: *)\x1b[0m"
      putDoc (pretty optm)
      putStrLn "\x1b[1;32m(* Compiled: *)\x1b[0m"
      putDoc (pretty (compileProgram env optm))
      pure (Just (core, env))
    CParse e s -> Nothing <$ reportP e s (T.pack x)
    CResolve e -> Nothing <$ reportR e (T.pack x)
    CInfer e -> Nothing <$ reportI e (T.pack x)

testTc :: String -> IO (Maybe ([Stmt (Var Resolved)], Env))
testTc x = do
  putStrLn "\x1b[1;32m(* Program: *)\x1b[0m"
  case compile "<test>" (T.pack x) of
    CSuccess (ast, core, _, env) -> do
      putDoc (pretty ast)
      putStrLn "\x1b[1;32m(* Type inference: *)\x1b[0m"
      ifor_ (difference env builtinsEnv ^. values) . curry $ \(k :: Var Resolved, t :: Type Typed) ->
        putDoc (pretty k <+> colon <+> tidyPrettyType t)
      putStrLn "\x1b[1;32m(* Kind inference: *)\x1b[0m"
      ifor_ (difference env builtinsEnv ^. types) . curry $ \(k, t) ->
        putDoc (pretty k <+> colon <+> pretty t)
      pure (Just (core, env))
    CParse e s -> Nothing <$ reportP e s (T.pack x)
    CResolve e -> Nothing <$ reportR e (T.pack x)
    CInfer e -> Nothing <$ reportI e (T.pack x)

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
    ["test-tc", x] -> do
      x' <- readFile x
      _ <- testTc x'
      pure ()
    [x, t] -> do
      x' <- T.readFile x
      compileFromTo x x' $ T.writeFile t . T.pack . show . pretty
    [] -> error "REPL not implemented yet"
    _ -> do
      putStrLn "usage: amulet from.ml to.lua"
      putStrLn "usage: amulet from.ml"
      putStrLn "usage: amulet"
