module Test.Syntax.Verify (tests) where

import Test.Tasty
import Test.Util

import Control.Monad.Infer

import qualified Data.Text.Lazy as L
import qualified Data.Text as T
import Data.Foldable

import Parser.Wrapper (runParser)
import Parser

import Syntax.Resolve (ResolveResult(..), resolveProgram)
import Syntax.Resolve.Import (runNullImport)
import Syntax.Desugar (desugarProgram)
import Syntax.Verify
import Syntax.Builtin

import Types.Infer (inferProgram)

import qualified Text.Pretty.Note as N
import Text.Pretty.Semantic

result :: String -> T.Text -> T.Text
result f c = fst . flip runNamey firstName $ do
  let parsed = requireJust f c $ runParser f (L.fromStrict c) parseTops
      prettyErrs = vsep . map (N.format (N.fileSpans [(f, c)] N.defaultHighlight))

  ResolveResult resolved _ _ <- requireRight f c <$> runNullImport (resolveProgram builtinResolve parsed)
  desugared <- desugarProgram resolved
  (inferred, env) <- requireThat f c <$> inferProgram builtinEnv desugared
  v <- genName
  let (_, es) = runVerify env v (verifyProgram inferred)
  pure $ if null es then T.empty else displayPlain (prettyErrs (toList es))

tests :: IO TestTree
tests = do
  verify <- testGroup "Verification tests" <$> goldenDir result "tests/verify/" ".ml"
  pmcheck <- testGroup "Pattern-matching checking" <$> goldenDir result "tests/verify/pmcheck/" ".ml"
  pure (testGroup "'Verify' tests" [verify, pmcheck])
