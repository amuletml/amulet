{-# LANGUAGE TemplateHaskell, OverloadedStrings, FlexibleContexts #-}
module Test.Core.Lint (tests) where

import Pretty (pretty)

import qualified Data.ByteString.Builder as B
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Text as T
import Data.Foldable

import Control.Monad
import Control.Monad.Gen

import Data.Position (SourceName)
import Data.Span (Span)

import Control.Monad.Infer (TypeError)

import Types.Infer (inferProgram, builtinsEnv)

import Syntax.Resolve (ResolveError, resolveProgram)
import qualified Syntax.Resolve.Scope as RS
import Syntax.Resolve.Toplevel (extractToplevels)
import Syntax.Desugar (desugarProgram)
import Syntax (Var, Resolved)

import Core.Lower (runLowerT, lowerProg)
import Core.Core (Stmt)
import Core.Lint

import Pretty (pretty, render)

import Parser.Wrapper (ParseResult(POK, PFailed), runParser)
import Parser (parseInput)

import Test.Tasty
import Test.Tasty.HUnit

data CompileResult
  = CSuccess [Stmt (Var Resolved)]
  | CParse   String Span
  | CResolve ResolveError
  | CInfer   TypeError

compile :: MonadGen Int m => [(SourceName, T.Text)] -> m CompileResult
compile [] = error "Cannot compile empty input"
compile (file:files) = do
  file' <- go (Right ([], RS.builtinScope, RS.emptyModules, builtinsEnv)) file
  files' <- foldlM go file' files
  case files' of
    Right (prg, _, _, _) -> CSuccess <$> runLowerT (lowerProg prg)
    Left err -> pure err

  where
    go (Right (tops, scope, modScope, env)) (name, file) =
      case runParser name (B.toLazyByteString $ T.encodeUtf8Builder file) parseInput of
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
        PFailed msg sp -> pure $ Left $ CParse msg sp
    go x _ = pure x



testLintLower :: String -> Assertion
testLintLower file = do
  contents <- T.readFile ("examples/" ++ file)
  case runGen (compile [(file, contents)]) of
    CSuccess i -> case runLintOK (checkStmt emptyScope i) of
                    Right _ -> pure ()
                    Left es -> assertFailure ("Core lint failed: " ++ render (pretty es))
    CParse e s -> assertFailure ("Parse error: " ++ e ++ " at " ++ render (pretty s))
    CResolve e -> assertFailure ("Resolution error: " ++ render (pretty e))
    CInfer e -> assertFailure ("Type error: " ++ render (pretty e))

tests :: TestTree
tests = testGroup "Test.Core.Lint" (map (ap testCase testLintLower) files)

files :: [String]
files =
  [ "gadt/vector.ml"
  , "gadt/term.ml"
  , "gadt/existential.ml"
  , "church-lists.ml"
  , "coroutines.ml"
  , "guess.ml"
  , "id.ml"
  , "lists.ml"
  , "modules.ml"
  -- , "opt.ml"
  , "peano.ml"
  , "pipe.ml"
  , "polymorphic-recursion.ml"
  -- , "skolems.ml"
  , "unsound/reference.ml"
  ]
