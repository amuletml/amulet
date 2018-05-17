{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Test.Core.Lint (tests) where

import qualified Data.Text.Lazy as L
import qualified Data.Text.IO as T
import qualified Data.Text as T
import Data.Position (SourceName)
import Data.Foldable
import Data.Spanned

import Control.Monad.Infer (TypeError)
import Control.Monad.Gen

import Types.Infer (inferProgram, builtinsEnv)

import Syntax.Resolve (ResolveError, resolveProgram)
import qualified Syntax.Resolve.Scope as RS
import Syntax.Resolve.Toplevel (extractToplevels)
import Syntax.Desugar (desugarProgram)

import Core.Lower (runLowerT, lowerProg)
import Core.Core (Stmt)
import Core.Simplify
import Core.Lint
import Core.Var

import Pretty

import Parser.Wrapper (runParser)
import Parser (parseInput)
import Parser.Error

import Test.Tasty
import Test.Tasty.HUnit

import System.Directory

data CompileResult
  = CSuccess [Stmt CoVar]
  | CParse   [ParseError]
  | CResolve [ResolveError]
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
      case runParser name (L.fromStrict file) parseInput of
        (Just parsed, _) -> do
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
        (Nothing, es) -> pure $ Left $ CParse es
    go x _ = pure x


testLint :: ([Stmt CoVar] -> Gen Int [Stmt CoVar]) -> String -> Assertion
testLint f file = do
  contents <- T.readFile file
  runGen $ do
    s <- compile [(file, contents)]
    case s of
      CSuccess c -> do
        c' <- f c
        case runLintOK (checkStmt emptyScope c') of
          Right _ -> pure $ pure ()
          Left es -> pure $ assertFailure $ "Core lint failed: " ++ render (pretty es)
      CParse es -> pure $ assertFailure $ render $ vsep $ map (\e -> string "Parse error: " <+> pretty e <+> " at " <+> pretty (annotation e)) es
      CResolve e -> pure $ assertFailure $ "Resolution error: " ++ render (pretty e)
      CInfer e -> pure $ assertFailure $ "Type error: " ++ render (pretty e)

testLintLower, testLintSimplify :: String -> Assertion
testLintLower = testLint pure
testLintSimplify = testLint optimise

tests :: IO TestTree
tests = do
  folderLint <- map (testCase <*> testLintSimplify . ("tests/lint/"++)) <$> listDirectory "tests/lint/"

  pure $ testGroup "Test.Core.Lint"
    [ testGroup "Examples" [ testGroup "Lower" (map (testCase <*> testLintLower . ("examples/"++)) files)
                           , testGroup "Simplify" (map (testCase <*> testLintSimplify . ("examples/"++)) files) ]
    , testGroup "Test folder" folderLint
    ]

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
  , "opt.ml"
  , "parsing.ml"
  , "peano.ml"
  , "pipe.ml"
  , "polymorphic-recursion.ml"
  -- , "skolems.ml"
  , "unsound/reference.ml"
  ]
