{-# LANGUAGE RankNTypes, OverloadedStrings, ScopedTypeVariables, FlexibleContexts #-}
module Main where

import System.Exit (ExitCode(..), exitWith)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import System.Console.GetOpt

import qualified Data.Text.Lazy as L
import qualified Data.Text.IO as T
import qualified Data.Text as T
import Data.Foldable

import Control.Monad.Gen (runGen)
import Control.Lens (ifor_, (^.), to)

import Data.Position (SourceName)

import Control.Monad.Infer (Env, TypeError, difference, values, types)
import Backend.Compile (compileProgram)

import Types.Infer (inferProgram, builtinsEnv)

import Syntax.Resolve (ResolveError, resolveProgram)
import qualified Syntax.Resolve.Scope as RS
import Syntax.Resolve.Toplevel (extractToplevels)
import Syntax.Types
import Syntax.Desugar (desugarProgram)
import Syntax.Pretty (tidyPrettyType)
import Syntax (Toplevel, Typed, Var, Resolved, Type)

import Core.Occurrence (OccursVar, tagOccursVar)
import Core.Simplify (optimise)
import Core.Lower (runLowerT, lowerProg)
import Core.Core (Stmt)
import Core.Var (CoVar)

import Pretty (Pretty(pretty), putDoc, (<+>), colon)

import Parser.Wrapper (ParseResult(POK, PFailed), Token(..), runParser, runLexer)
import Parser.Error (ParseError)
import Parser (parseInput)
import Parser.Lexer (lexerScan)

import Errors (reportS, reportR, reportI)

data CompileResult a
  = CSuccess [Toplevel Typed] [Stmt CoVar] [Stmt a] Env
  | CParse   [ParseError]
  | CResolve ResolveError
  | CInfer   TypeError

compile :: [(SourceName, T.Text)] -> CompileResult (OccursVar CoVar)
compile [] = error "Cannot compile empty input"
compile (file:files) = runGen $ do
  file' <- go (Right ([], RS.builtinScope, RS.emptyModules, builtinsEnv)) file
  files' <- foldlM go file' files
  case files' of
    Right (prg, _, _, env) -> do
      lower <- runLowerT (lowerProg prg)
      optm <- optimise lower
      pure (CSuccess prg lower (tagOccursVar optm) env)

    Left err -> pure err

  where
    go (Right (tops, scope, modScope, env)) (name, file) =
      case runParser name (L.fromStrict file) parseInput of
        POK _ parsed -> do
          resolved <- resolveProgram scope modScope parsed
          case resolved of
            Right (resolved, modScope') -> do
              desugared <- desugarProgram resolved
              infered <- inferProgram env desugared
              case infered of
                Right (prog, env') ->
                  let (var, tys) = extractToplevels parsed
                      (var', tys') = extractToplevels resolved
                  in pure $ Right (tops ++ prog
                                  , scope { RS.varScope = RS.insertN (RS.varScope scope) (zip var var')
                                          , RS.tyScope  = RS.insertN (RS.tyScope scope)  (zip tys tys')
                                          }
                                  , modScope'
                                  , env')
                Left e -> pure $ Left $ CInfer e
            Left e -> pure $ Left $ CResolve e
        PFailed es -> pure $ Left $ CParse es
    go x _ = pure x


compileFromTo :: [(FilePath, T.Text)]
              -> (forall a. Pretty a => a -> IO ())
              -> IO ()
compileFromTo fs emit =
  case compile fs of
    CSuccess _ _ core env -> emit (compileProgram env core)
    CParse es -> traverse_ (flip reportS fs) es
    CResolve e -> putStrLn "Resolution error" >> reportR e fs
    CInfer e -> putStrLn "Type error" >> reportI e fs

test :: [(FilePath, T.Text)] -> IO (Maybe ([Stmt CoVar], Env))
test fs = do
  putStrLn "\x1b[1;32m(* Program: *)\x1b[0m"
  case compile fs of
    CSuccess ast core optm env -> do
      putDoc (pretty ast)
      putStrLn "\x1b[1;32m(* Type inference: *)\x1b[0m"
      ifor_ (difference env builtinsEnv ^. values . to toMap) . curry $ \(k :: Var Resolved, t :: Type Typed) ->
        putDoc (pretty k <+> colon <+> tidyPrettyType t)
      putStrLn "\x1b[1;32m(* Kind inference: *)\x1b[0m"
      ifor_ (difference env builtinsEnv ^. types ^. to toMap) . curry $ \(k :: Var Resolved, t) ->
        putDoc (pretty k <+> colon <+> pretty t)
      putStrLn "\x1b[1;32m(* Core lowering: *)\x1b[0m"
      putDoc (pretty core)
      putStrLn "\x1b[1;32m(* Optimised: *)\x1b[0m"
      putDoc (pretty optm)
      putStrLn "\x1b[1;32m(* Compiled: *)\x1b[0m"
      putDoc (pretty (compileProgram env optm))
      pure (Just (core, env))
    CParse es -> Nothing <$ traverse_ (flip reportS fs) es
    CResolve e -> Nothing <$ reportR e fs
    CInfer e -> Nothing <$ reportI e fs

testLexer :: [(FilePath, T.Text)] -> IO ()
testLexer fs = for_ fs $ \(name, file) ->
  case runLexer name (L.fromStrict file) lexerScan of
    POK _ toks -> print (map (\(Token t _) -> t) toks)
    PFailed es -> traverse_ (flip reportS fs) es

testTc :: [(FilePath, T.Text)] -> IO (Maybe ([Stmt CoVar], Env))
testTc fs = do
  putStrLn "\x1b[1;32m(* Program: *)\x1b[0m"
  case compile fs of
    CSuccess ast core _ env -> do
      putDoc (pretty ast)
      putStrLn "\x1b[1;32m(* Type inference: *)\x1b[0m"
      ifor_ (difference env builtinsEnv ^. values . to toMap) . curry $ \(k :: Var Resolved, t :: Type Typed) ->
        putDoc (pretty k <+> colon <+> pretty t)
      putStrLn "\x1b[1;32m(* Kind inference: *)\x1b[0m"
      ifor_ (difference env builtinsEnv ^. types . to toMap) . curry $ \(k :: Var Resolved, t) ->
        putDoc (pretty k <+> colon <+> pretty t)
      pure (Just (core, env))
    CParse es -> Nothing <$ traverse_ (flip reportS fs) es
    CResolve e -> Nothing <$ reportR e fs
    CInfer e -> Nothing <$ reportI e fs

data CompilerOption = Test | TestTc | TestLex | Out String
  deriving (Show)

flags :: [OptDescr CompilerOption]
flags = [ Option ['t'] ["test"] (NoArg Test)
          "Provides additional debug information on the output"
        , Option [] ["test-tc"] (NoArg TestTc)
          "Provides additional type check information on the output"
        , Option [] ["test-lex"] (NoArg TestLex)
          "Simply prints the result of lexing the file"
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

    ([TestLex], files, []) -> do
      files' <- traverse T.readFile files
      _ <- testLexer (zip files files')
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
