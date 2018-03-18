{-# LANGUAGE RankNTypes, OverloadedStrings, ScopedTypeVariables, FlexibleContexts #-}
module Main where

import System.Exit (ExitCode(..), exitWith)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import System.Console.GetOpt

import qualified Data.ByteString.Builder as B
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Text as T
import Data.Foldable

import Control.Monad.Reader (runReaderT)
import Control.Monad.Gen (runGen)
import Control.Lens (ifor_, (^.))

import "amuletml" Data.Position (SourceName)
import "amuletml" Data.Span (Span)

import "amuletml" Control.Monad.Infer (Env, TypeError, difference, values, types)
import "amuletml" Backend.Compile (compileProgram)

import "amuletml" Types.Infer (inferProgram, builtinsEnv)

import "amuletml" Syntax.Resolve (ResolveError, resolveProgram)
import qualified "amuletml" Syntax.Resolve.Scope as RS
import "amuletml" Syntax.Resolve.Toplevel (extractToplevels)
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

compile :: [(SourceName, T.Text)] -> CompileResult (OccursVar (Var Resolved))
compile [] = error "Cannot compile empty input"
compile (file:files) = runGen $ do
  file' <- go (Right ([], RS.builtinScope, RS.emptyModules, builtinsEnv)) file
  files' <- foldlM go file' files
  case files' of
    Right (prg, _, _, env) -> do
      lower <- runReaderT (lowerProg prg) env
      optm <- optimise lower
      pure (CSuccess (prg, lower, tagOccursVar optm, env))

    Left err -> pure err

  where
    go (Right (tops, scope, modScope, env)) (name, file) =
      case runParser name (B.toLazyByteString $ T.encodeUtf8Builder file) parseInput of
        POK _ parsed -> do
          desugared <- desugarProgram parsed
          resolved <- resolveProgram scope modScope desugared
          case resolved of
            Right (resolved, modScope') -> do
              infered <- inferProgram env resolved
              case infered of
                Right (prog, env') ->
                  let (var, tys) = extractToplevels desugared
                      (var', tys') = extractToplevels resolved
                  in pure $ Right (tops ++ prog
                                  , scope { RS.varScope = RS.insertN (RS.varScope scope) (zip var var')
                                          , RS.tyScope  = RS.insertN (RS.tyScope scope)  (zip tys tys')
                                          }
                                  , modScope'
                                  , env')
                Left e -> pure $ Left $ CInfer e
            Left e -> pure $ Left $ CResolve e
        PFailed msg sp -> pure $ Left $ CParse msg sp
    go x _ = pure x


compileFromTo :: [(FilePath, T.Text)]
              -> (forall a. Pretty a => a -> IO ())
              -> IO ()
compileFromTo fs emit =
  case compile fs of
    CSuccess (_, _, core, env) -> emit (compileProgram env core)
    CParse e s -> putStrLn "Parse error" >> reportP e s fs
    CResolve e -> putStrLn "Resolution error" >> reportR e fs
    CInfer e -> putStrLn "Type error" >> reportI e fs

test :: [(FilePath, T.Text)] -> IO (Maybe ([Stmt (Var Resolved)], Env))
test fs = do
  putStrLn "\x1b[1;32m(* Program: *)\x1b[0m"
  case compile fs of
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
    CParse e s -> Nothing <$ reportP e s fs
    CResolve e -> Nothing <$ reportR e fs
    CInfer e -> Nothing <$ reportI e fs

testTc :: [(FilePath, T.Text)] -> IO (Maybe ([Stmt (Var Resolved)], Env))
testTc fs = do
  putStrLn "\x1b[1;32m(* Program: *)\x1b[0m"
  case compile fs of
    CSuccess (ast, core, _, env) -> do
      putDoc (pretty ast)
      putStrLn "\x1b[1;32m(* Type inference: *)\x1b[0m"
      ifor_ (difference env builtinsEnv ^. values) . curry $ \(k :: Var Resolved, t :: Type Typed) ->
        putDoc (pretty k <+> colon <+> pretty t)
      putStrLn "\x1b[1;32m(* Kind inference: *)\x1b[0m"
      ifor_ (difference env builtinsEnv ^. types) . curry $ \(k, t) ->
        putDoc (pretty k <+> colon <+> pretty t)
      pure (Just (core, env))
    CParse e s -> Nothing <$ reportP e s fs
    CResolve e -> Nothing <$ reportR e fs
    CInfer e -> Nothing <$ reportI e fs

data CompilerOption = Test | TestTc | Out String
  deriving (Show)

flags :: [OptDescr CompilerOption]
flags = [ Option ['t'] ["test"] (NoArg Test)
          "Provides additional debug information on the output"
        , Option [] ["test-tc"] (NoArg TestTc)
          "Provides additional type check information on the output"
        , Option ['o'] ["out"]  (ReqArg Out "OUT")
          "Writes the generated Lua to a specific file."
        ]

main :: IO ()
main = do
  ags <- getArgs
  case getOpt Permute flags ags of
    (_ , [], []) -> do
      hPutStrLn stderr "REPL not implemented yet"
      exitWith (ExitFailure 1)

    ([], files, []) -> do
      files' <- traverse T.readFile files
      compileFromTo (zip files files') (putDoc . pretty)
      pure ()

    ([Test], files, []) -> do
      files' <- traverse T.readFile files
      _ <- test (zip files files')
      pure ()

    ([TestTc], files, []) -> do
      files' <- traverse T.readFile files
      _ <- testTc (zip files files')
      pure ()

    ([Out o], files, []) -> do
      files' <- traverse T.readFile files
      compileFromTo (zip files files') (T.writeFile o . T.pack . show . pretty)
      pure ()

    (_, _, []) -> do
      hPutStrLn stderr (usageInfo "Invalid combination of flags" flags)
      exitWith (ExitFailure 1)

    (_, _, errs) -> do
      hPutStrLn stderr (concat errs ++ usageInfo "amc: The Amulet compiler" flags)
      exitWith (ExitFailure 1)
