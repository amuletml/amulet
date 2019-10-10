module Test.Types.Check (tests) where

import Test.Tasty
import Test.Util

import Control.Monad.Infer (names, firstName)
import Control.Monad.Namey
import Control.Lens ((^.), to, runIdentity)

import qualified Data.Text.Lazy as L
import qualified Data.Text as T
import qualified Data.Map as Map

import Parser.Wrapper (runParser)
import Parser

import Syntax.Resolve (ResolveResult(..), resolveProgram)
import Types.Infer (inferProgram)
import Syntax.Desugar (desugarProgram)
import Syntax.Types (difference, toMap)
import Syntax.Builtin

import qualified Text.Pretty.Note as N
import Text.Pretty.Semantic

result :: String -> T.Text -> T.Text
result f c = runIdentity . flip evalNameyT firstName $ do
  let parsed = requireJust f c $ runParser f (L.fromStrict c) parseTops
  ResolveResult resolved _ _ <- requireRight f c <$> resolveProgram builtinResolve parsed

  desugared <- desugarProgram resolved
  inferred <- inferProgram builtinEnv desugared

  pure . displayPlain
       . either prettyErrs ((Right<$>) . reportEnv . snd)
       . toEither $ inferred

  where
    reportEnv env =
      let env' = difference env builtinEnv
      in vsep $
        map reportComponent (Map.toList (env' ^. names . to toMap))

    reportComponent (v, t) = pretty v <+> colon <+> pretty t

    prettyErrs = vsep . map (N.format (N.fileSpans [(f, c)] N.defaultHighlight))

tests :: IO TestTree
tests = do
  inference <- testGroup "Type inference tests" <$> goldenDir result "tests/types/" ".ml"
  gadts <- testGroup "GADT inference tests" <$> goldenDir result "tests/types/gadt/" ".ml"
  rankn <- testGroup "Rank-N inference tests" <$> goldenDir result "tests/types/rankn/" ".ml"
  lazy <- testGroup "Automatic laziness tests" <$> goldenDir result "tests/types/lazy/" ".ml"
  letgen <- testGroup "Let generalization tests" <$> goldenDir result "tests/types/letgen/" ".ml"
  wild <- testGroup "Type wildcard tests" <$> goldenDir result "tests/types/wildcards/" ".ml"
  clss <- testGroup "Type class tests" <$> goldenDir result "tests/types/class/" ".ml"
  vta <- testGroup "Visible type application tests" <$> goldenDir result "tests/types/vta/" ".ml"
  syn <- testGroup "Type synonym tests " <$> goldenDir result "tests/types/synonym/" ".ml"
  tyfun <- testGroup "Type function tests " <$> goldenDir result "tests/types/tyfun/" ".ml"
  pure (testGroup "Type inference" [ inference, gadts, rankn, lazy, letgen, wild, clss, vta, syn, tyfun ])
