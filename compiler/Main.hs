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
import Data.Functor

import Data.Functor.Identity
import Data.Position (SourceName)

import Control.Monad.Infer (Env, TypeError, nameSupply)
import Control.Monad.Namey
import Backend.Lua

import Types.Infer (inferProgram, builtinsEnv)

import Syntax.Resolve (ResolveError, resolveProgram)
import qualified Syntax.Resolve.Scope as RS
import Syntax.Resolve.Toplevel (extractToplevels)
import Syntax.Desugar (desugarProgram)
import Syntax.Verify
import Syntax (Toplevel, Typed)

import Core.Simplify (optimise)
import Core.Lower (runLowerT, lowerProg)
import Core.Core (Stmt)
import Core.Var (CoVar)

import Text.Pretty.Semantic
import Text.Pretty.Note

import Parser.Wrapper (runParser)
import Parser.Error (ParseError)
import Parser (parseTops)

import Errors (reportS)
import qualified Debug as D
import Repl

data CompileResult
  = CSuccess [VerifyError] [Toplevel Typed] [Stmt CoVar] [Stmt CoVar] LuaStmt Env
  | CParse   [ParseError]
  | CResolve [ResolveError]
  | CInfer   [TypeError]

compile :: [(SourceName, T.Text)] -> CompileResult
compile [] = error "Cannot compile empty input"
compile (file:files) = runIdentity . flip evalNameyT nameSupply $ do
  file' <- go (Right ([], [], RS.builtinScope, RS.emptyModules, builtinsEnv)) file
  files' <- foldlM go file' files
  case files' of
    Right (ve, prg, _, _, env) -> do
      lower <- runLowerT (lowerProg prg)
      optm <- optimise lower
      pure (CSuccess ve prg lower optm (compileProgram env optm) env)

    Left err -> pure err

  where
    go (Right (errs, tops, scope, modScope, env)) (name, file) =
      case runParser name (L.fromStrict file) parseTops of
        (Just parsed, _) -> do
          resolved <- resolveProgram scope modScope parsed
          case resolved of
            Right (resolved, modScope') -> do
              desugared <- desugarProgram resolved
              infered <- inferProgram env desugared
              case infered of
                Right (prog, env') ->
                  let x = runVerify (verifyProgram prog)
                      (var, tys) = extractToplevels parsed
                      (var', tys') = extractToplevels resolved
                      errs' = case x of
                        Left es -> toList es
                        Right () -> []
                   in pure $ Right ( errs ++ errs'
                                   , tops ++ prog
                                   , scope { RS.varScope = RS.insertN (RS.varScope scope) (zip var var')
                                           , RS.tyScope  = RS.insertN (RS.tyScope scope)  (zip tys tys')
                                           }
                                   , modScope'
                                   , env')
                Left e -> pure $ Left $ CInfer e
            Left e -> pure $ Left $ CResolve e
        (Nothing, es) -> pure $ Left $ CParse es
    go x _ = pure x


compileFromTo :: [(FilePath, T.Text)]
              -> (forall a. Pretty a => a -> IO ())
              -> IO ()
compileFromTo fs emit =
  case compile fs of
    CSuccess es _ _ _ lua _ -> do
      traverse_ (`reportS` fs) es
      if any isError es
         then pure ()
         else emit lua
    CParse es -> traverse_ (`reportS` fs) es
    CResolve es -> traverse_ (`reportS` fs) es
    CInfer es -> traverse_ (`reportS` fs) es

test :: D.DebugMode -> [(FilePath, T.Text)] -> IO (Maybe ([Stmt CoVar], Env))
test mode fs =
  case compile fs of
    CSuccess es ast core opt lua env -> do
      traverse_ (`reportS` fs) es
      if any isError es
         then pure Nothing
         else D.dump mode ast core opt lua builtinsEnv env $> Just (core, env)
    CParse es -> Nothing <$ traverse_ (`reportS` fs) es
    CResolve es -> Nothing <$ traverse_ (`reportS` fs) es
    CInfer es -> Nothing <$ traverse_ (`reportS` fs) es

data CompilerOption = Test | TestTc | Out String
  deriving (Show)

isError :: Note a b => a -> Bool
isError x = diagnosticKind x == ErrorMessage

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
    ([] , [], [])       -> repl D.Void
    ([Test] , [], [])   -> repl D.Test
    ([TestTc] , [], []) -> repl D.TestTc

    ([], files, []) -> do
      files' <- traverse T.readFile files
      compileFromTo (zip files files') (putDoc . pretty)
      pure ()

    ([Test], files, []) -> do
      files' <- traverse T.readFile files
      _ <- test D.Test (zip files files')
      pure ()

    ([TestTc], files, []) -> do
      files' <- traverse T.readFile files
      _ <- test D.TestTc (zip files files')
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
