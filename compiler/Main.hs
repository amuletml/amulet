{-# LANGUAGE RankNTypes, OverloadedStrings, ScopedTypeVariables, FlexibleContexts #-}
module Main where

import System.Exit (ExitCode(..), exitWith)
import System.IO (hPutStrLn, stderr)
import Options.Applicative hiding (ParseError)

import qualified Data.Text.Lazy as L
import qualified Data.Text.IO as T
import qualified Data.Text as T
import Data.Position (SourceName)
import Data.Foldable
import Data.These

import Control.Monad.Infer (Env, TypeError, firstName)
import Control.Monad.Namey
import Control.Monad.State

import Language.Lua.Syntax
import Backend.Lua

import Types.Infer (inferProgram)

import Syntax.Resolve (ResolveError, resolveProgram)
import qualified Syntax.Resolve.Scope as RS
import Syntax.Resolve.Toplevel (extractToplevels)
import Syntax.Desugar (desugarProgram)
import qualified Syntax.Builtin as Bi
import Syntax.Verify
import Syntax.Var (Typed)
import Syntax (Toplevel)

import Core.Optimise.Reduce (reducePass)
import Core.Optimise.Newtype (killNewtypePass)
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
compile opt (file:files) =
  let (res, name) = flip runNamey firstName $ do
        file' <- go (Right ([], [], [], Bi.builtinResolve, Bi.builtinModules, Bi.builtinEnv)) file
        foldlM go file' files
  in case res of
       Right (ve, te, prg, _, _, env) ->
         -- We run these outside the main namey monad to allow these to be lazily evaluated.
         let (lower, name') = flip runNamey name $ runLowerT (lowerProg prg)
             (optm, _) = flip runNamey name' $ case opt of
                Do -> optimise lower
                Don't -> do
                  noNewtype <- killNewtypePass lower
                  reduce <- reducePass noNewtype
                  pure (deadCodePass reduce)
             lua = compileProgram optm
         in CSuccess ve te prg lower optm lua env
       Left err -> err

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
                That (prog, env') -> do
                  verifyV <- genName
                  let x = runVerify env' verifyV (verifyProgram prog)
                      (var, tys) = extractToplevels parsed
                      (var', tys') = extractToplevels resolved
                      errs' = case x of
                        Left es -> toList es
                        Right () -> []
                  pure $ Right ( errs ++ errs'
                               , tyerrs
                               , tops ++ prog
                               , scope { RS.varScope = RS.insertN' (RS.varScope scope) (zip var var')
                                       , RS.tyScope  = RS.insertN' (RS.tyScope scope)  (zip tys tys')
                                       }
                               , modScope'
                               , env' )
                These errors (_, _) | any isError errors -> pure (Left (CInfer errors))
                These errors (prog, env') -> do
                  verifyV <- genName
                  let x = runVerify env' verifyV (verifyProgram prog)
                      (var, tys) = extractToplevels parsed
                      (var', tys') = extractToplevels resolved
                      errs' = case x of
                        Left es -> toList es
                        Right () -> []
                  pure $ Right ( errs ++ errs'
                               , tyerrs ++ errors
                               , tops ++ prog
                               , scope { RS.varScope = RS.insertN' (RS.varScope scope) (zip var var')
                                       , RS.tyScope  = RS.insertN' (RS.tyScope scope)  (zip tys tys')
                                       }
                               , modScope'
                               , env' )
                This e -> pure $ Left $ CInfer e
            Left e -> pure $ Left $ CResolve e
        (Nothing, es) -> pure $ Left $ CParse es
    go x _ = pure x


compileFromTo :: DoOptimise -> D.DebugMode
              -> [(FilePath, T.Text)]
              -> (forall a. Pretty a => a -> IO ())
              -> IO ()
compileFromTo opt dbg fs emit =
  case compile opt fs of
    CSuccess es tes ast core opt lua env -> do
      traverse_ (`reportS` fs) es
      traverse_ (`reportS` fs) tes
      D.dump dbg ast core opt lua Bi.builtinEnv env
      if any isError es || any isError tes
         then pure ()
         else emit lua
    CParse es -> traverse_ (`reportS` fs) es
    CResolve es -> traverse_ (`reportS` fs) es
    CInfer es -> traverse_ (`reportS` fs) es

test :: DoOptimise -> D.DebugMode
     -> [(FilePath, T.Text)]
     -> IO (Maybe ([Stmt CoVar], Env))
test opt mode fs =
  case compile opt fs of
    CSuccess es tes ast core opt lua env -> do
      traverse_ (`reportS` fs) es
      traverse_ (`reportS` fs) tes
      guard (all (not . isError) es)
      guard (all (not . isError) tes)
      D.dump mode ast core opt lua Bi.builtinEnv env

      pure (pure (core, env))
    CParse es -> Nothing <$ traverse_ (`reportS` fs) es
    CResolve es -> Nothing <$ traverse_ (`reportS` fs) es
    CInfer es -> Nothing <$ traverse_ (`reportS` fs) es

data DoOptimise = Do | Don't

data CompilerOptions
  = CompilerOptions
    { debugMode   :: D.DebugMode
    , output      :: Maybe FilePath
    , forceRepl   :: Bool
    , optLevel    :: Int
    , replCommand :: Maybe String
    , serverPort  :: Int
    , files       :: [FilePath]
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

   <*> (Just <$>
         option str
         ( long "out" <> short 'o' <> metavar "FILE"
        <> help "Write the generated Lua to a specific file." )
     <|> pure Nothing)

   <*> switch ( long "repl" <> short 'r' <> help "Go to the REPL after loading each file" )

   <*> option auto ( long "opt" <> short 'O' <> metavar "LEVEL" <> value 1 <> help "Controls the optimisation level." )

   <*> (Just <$>
         option str
         ( long "client" <> short 'c' <> metavar "COMMAND"
        <> help "Connect to another running REPL to execute the command" )
     <|> pure Nothing)

   <*> option auto ( long "port" <> metavar "PORT" <> value 5478 <> help "Port to use for the REPL server. (Default: 5478)" )

   <*> many (argument str (metavar "FILES..."))

main :: IO ()
main = do
  options <- execParser flags
  case options of
    CompilerOptions { replCommand = Just str, serverPort = i } -> runRemoteReplCommand i str
    CompilerOptions { debugMode = db, files = [], serverPort = i } -> repl i db
    CompilerOptions { debugMode = db, forceRepl = True, files = fs, serverPort = i } -> replFrom i db fs

    CompilerOptions { output = Just out, files = fs }
      | out `elem` fs -> do
          hPutStrLn stderr ("Cannot overwrite input file " ++ out)
          exitWith (ExitFailure 1)

    CompilerOptions { debugMode = db, optLevel = opt, output = Nothing, files = fs } | db /= D.Void -> do
      fs' <- traverse T.readFile fs
      let opt' = if opt >= 1 then Do else Don't
      _ <- test opt' db (zip fs fs')
      pure ()

    CompilerOptions { debugMode = db, optLevel = opt, output = out, files = fs } -> do
      let opt' = if opt >= 1 then Do else Don't
          out' :: Pretty a => a -> IO ()
          out' = case out of
                   Nothing -> putDoc . pretty
                   Just f -> T.writeFile f . T.pack . show . pretty
      fs' <- traverse T.readFile fs
      compileFromTo opt' db (zip fs fs') out'
