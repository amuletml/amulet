{-# LANGUAGE RankNTypes, OverloadedStrings, ScopedTypeVariables, FlexibleContexts, LambdaCase #-}
module Main where

import System.Exit (ExitCode(..), exitWith)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import System.Console.GetOpt

import qualified Data.Text.Lazy as L
import qualified Data.Text.IO as T
import qualified Data.Text as T
import Data.Position (SourceName)
import Data.Functor.Identity
import Data.Foldable
import Data.These

import Control.Monad.Infer (Env, TypeError, firstName)
import Control.Monad.Namey
import Control.Monad.State

import Language.Lua.Syntax
import Backend.Lua

import Types.Infer (inferProgram, builtinsEnv)

import Syntax.Resolve (ResolveError, resolveProgram)
import qualified Syntax.Resolve.Scope as RS
import Syntax.Resolve.Toplevel (extractToplevels)
import Syntax.Desugar (desugarProgram)
import Syntax.Verify
import Syntax.Var (Typed)
import Syntax (Toplevel)

import Core.Optimise.Reduce (reducePass)
import Core.Optimise.DeadCode (deadCodePass)
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
  = CSuccess [VerifyError] [TypeError] [Toplevel Typed] [Stmt CoVar] [Stmt CoVar] LuaStmt Env
  | CParse   [ParseError]
  | CResolve [ResolveError]
  | CInfer   [TypeError]


compile :: DoOptimise -> [(SourceName, T.Text)] -> CompileResult
compile _ [] = error "Cannot compile empty input"
compile opt (file:files) = runIdentity . flip evalNameyT firstName $ do
  file' <- go (Right ([], [], [], RS.builtinScope, RS.emptyModules, builtinsEnv)) file
  files' <- foldlM go file' files
  case files' of
    Right (ve, te, prg, _, _, env) -> do
      lower <- runLowerT (lowerProg prg)
      optm <- case opt of
                Do -> optimise lower
                Don't -> deadCodePass <$> reducePass lower
      pure (CSuccess ve te prg lower optm (compileProgram optm) env)

    Left err -> pure err

  where
    go (Right (errs, tyerrs, tops, scope, modScope, env)) (name, file) =
      case runParser name (L.fromStrict file) parseTops of
        (Just parsed, _) -> do
          resolved <- resolveProgram scope modScope parsed
          case resolved of
            Right (resolved, modScope') -> do
              desugared <- desugarProgram resolved
              infered <- inferProgram env desugared
              case infered of
                That (prog, env') ->
                  let x = runVerify (verifyProgram prog)
                      (var, tys) = extractToplevels parsed
                      (var', tys') = extractToplevels resolved
                      errs' = case x of
                        Left es -> toList es
                        Right () -> []
                   in pure $ Right ( errs ++ errs'
                                   , tyerrs
                                   , tops ++ prog
                                   , scope { RS.varScope = RS.insertN' (RS.varScope scope) (zip var var')
                                           , RS.tyScope  = RS.insertN' (RS.tyScope scope)  (zip tys tys')
                                           }
                                   , modScope'
                                   , env')
                These errors (_, _) | any isError errors -> pure (Left (CInfer errors))
                These errors (prog, env') ->
                  let x = runVerify (verifyProgram prog)
                      (var, tys) = extractToplevels parsed
                      (var', tys') = extractToplevels resolved
                      errs' = case x of
                        Left es -> toList es
                        Right () -> []
                   in pure $ Right ( errs ++ errs'
                                   , tyerrs ++ errors
                                   , tops ++ prog
                                   , scope { RS.varScope = RS.insertN' (RS.varScope scope) (zip var var')
                                           , RS.tyScope  = RS.insertN' (RS.tyScope scope)  (zip tys tys')
                                           }
                                   , modScope'
                                   , env')
                This e -> pure $ Left $ CInfer e
            Left e -> pure $ Left $ CResolve e
        (Nothing, es) -> pure $ Left $ CParse es
    go x _ = pure x


compileFromTo :: DoOptimise
              -> [(FilePath, T.Text)]
              -> (forall a. Pretty a => a -> IO ())
              -> IO ()
compileFromTo opt fs emit =
  case compile opt fs of
    CSuccess es tes _ _ _ lua _ -> do
      traverse_ (`reportS` fs) es
      traverse_ (`reportS` fs) tes
      if any isError es || any isError tes
         then pure ()
         else emit lua
    CParse es -> traverse_ (`reportS` fs) es
    CResolve es -> traverse_ (`reportS` fs) es
    CInfer es -> traverse_ (`reportS` fs) es

test :: D.DebugMode -> [(FilePath, T.Text)] -> IO (Maybe ([Stmt CoVar], Env))
test mode fs =
  case compile (if mode == D.TestTc then Don't else Do) fs of
    CSuccess es tes ast core opt lua env -> do
      traverse_ (`reportS` fs) es
      traverse_ (`reportS` fs) tes
      guard (all (not . isError) es)
      guard (all (not . isError) tes)
      D.dump mode ast core opt lua builtinsEnv env

      pure (pure (core, env))
    CParse es -> Nothing <$ traverse_ (`reportS` fs) es
    CResolve es -> Nothing <$ traverse_ (`reportS` fs) es
    CInfer es -> Nothing <$ traverse_ (`reportS` fs) es

data DoOptimise = Do | Don't

data CompilerOption = Test | TestTc | Out String | Optl Int
  deriving (Show, Eq)

isError :: Note a b => a -> Bool
isError x = diagnosticKind x == ErrorMessage

flags :: [OptDescr CompilerOption]
flags = [ Option ['t'] ["test"] (NoArg Test)
          "Provides additional debug information on the output"
        , Option [] ["test-tc"] (NoArg TestTc)
          "Provides additional type check information on the output"
        , Option ['o'] ["out"]  (ReqArg Out "[output]")
          "Writes the generated Lua to a specific file."
        , Option ['O'] ["optl"] (ReqArg (Optl . read) "[level]")
          "Controls the optimisation level."
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
      compileFromTo Do (zip files files') (putDoc . pretty)
      pure ()

    ([Optl n], files, []) -> do
      let opt = if n == 0 then Don't else Do
      files' <- traverse T.readFile files
      compileFromTo opt (zip files files') (putDoc . pretty)
      pure ()

    ([Test], files, []) -> do
      files' <- traverse T.readFile files
      _ <- test D.Test (zip files files')
      pure ()

    ([TestTc], files, []) -> do
      files' <- traverse T.readFile files
      _ <- test D.TestTc (zip files files')
      pure ()

    (opts, files, []) | Just o <- findOut opts -> do
      let opt = if Optl 0 `elem` opts then Don't else Do

      when (o `elem` files) $ do
        hPutStrLn stderr ("error: refusing to overwrite input file " ++ o)
        exitWith (ExitFailure 1)

      files' <- traverse T.readFile files
      compileFromTo opt (zip files files') (T.writeFile o . T.pack . show . pretty)
      pure ()

    (_, _, []) -> do
      hPutStrLn stderr (usageInfo "Invalid combination of flags" flags)
      exitWith (ExitFailure 1)

    (_, _, errs) -> do
      hPutStrLn stderr (concat errs ++ usageInfo "amc: The Amulet compiler" flags)
      exitWith (ExitFailure 1)

 where
   findOut :: [CompilerOption] -> Maybe String
   findOut = fmap (\(Out x) -> x) . find (\case { Out{} -> True; _ -> False })
