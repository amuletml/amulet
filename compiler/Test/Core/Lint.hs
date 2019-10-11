{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Test.Core.Lint (tests) where

import qualified Data.Text.Lazy as L
import qualified Data.Text.IO as T
import qualified Data.Text as T
import Data.Position (SourceName)
import Data.Spanned
import Data.These
import Data.List

import Control.Monad.Infer (TypeError, firstName)
import Control.Monad.Namey

import Types.Infer (inferProgram)

import Syntax.Resolve (ResolveError, ResolveResult(..), resolveProgram)
import Syntax.Resolve.Import (runNullImport)
import Syntax.Desugar (desugarProgram)
import Syntax.Builtin

import Core.Lower (runLowerT, lowerProg)
import Core.Core (Stmt)
import Core.Simplify
import Core.Lint
import Core.Var

import Text.Pretty.Semantic

import Parser.Wrapper (runParser)
import Parser (parseTops)
import Parser.Error

import Test.Tasty
import Test.Tasty.HUnit

import System.Directory

data CompileResult
  = CSuccess [Stmt CoVar]
  | CParse   [ParseError]
  | CResolve [ResolveError]
  | CInfer   [TypeError]

toEither :: These [a] b -> Either [a] b
toEither (This e) = Left e
toEither (These [] x) = Right x
toEither (These e _) = Left e
toEither (That x) = Right x

compile :: MonadNamey m => SourceName -> T.Text -> m CompileResult
compile name file =
  case runParser name (L.fromStrict file) parseTops of
    (Nothing, es) -> pure $ CParse es
    (Just parsed, _) -> do
      resolved <- runNullImport $ resolveProgram builtinResolve parsed
      case resolved of
        Left es -> pure $ CResolve es
        Right (ResolveResult resolved _ _) -> do
          desugared <- desugarProgram resolved
          infered <- toEither <$> inferProgram builtinEnv desugared
          case infered of
            Left es -> pure $ CInfer es
            Right (prg, _) -> CSuccess <$> runLowerT (lowerProg prg)


testLint :: ([Stmt CoVar] -> Namey [Stmt CoVar]) -> String -> Assertion
testLint f file = do
  contents <- T.readFile file
  fst . flip runNamey firstName $ do
    s <- compile file contents
    case s of
      CSuccess c -> do
        c' <- f c
        case runLintOK (checkStmt emptyScope c') of
          Nothing -> pure $ pure ()
          Just (_, es) -> pure $ assertFailure $ "Core lint failed: " ++ displayS (pretty es)
      CParse es -> pure $ assertFailure $ displayS $ vsep $ map (\e -> string "Parse error: " <+> pretty e <+> " at " <+> pretty (annotation e)) es
      CResolve e -> pure $ assertFailure $ "Resolution error: " ++ displayS (pretty e)
      CInfer e -> pure $ assertFailure $ "Type error: " ++ displayS (pretty e)

testLintLower, testLintSimplify :: String -> Assertion
testLintLower = testLint pure
testLintSimplify = testLint optimise

tests :: IO TestTree
tests = do
  folderLint <- map (testCase <*> testLintSimplify . ("tests/lint/"++)) . sort <$> listDirectory "tests/lint/"

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
  , "lazy-list.ml"
  , "lists.ml"
  , "modules.ml"
  , "peano.ml"
  , "pipe.ml"
  , "polymorphic-recursion.ml"
  , "mini-servant.ml"
  ]
