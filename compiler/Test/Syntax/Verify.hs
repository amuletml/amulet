module Test.Syntax.Verify (tests) where

import Test.Tasty
import Test.Util

import Control.Monad.Infer

import qualified Data.Text.Lazy as L
import qualified Data.Text as T
import Data.Foldable

import Parser.Wrapper (runParser)
import Parser

import qualified Syntax.Resolve.Scope as RS
import Syntax.Resolve (resolveProgram)
import Syntax.Desugar (desugarProgram)
import Syntax.Verify

import Syntax.Pretty()

import Types.Infer (inferProgram, builtinsEnv)

import qualified Text.Pretty.Note as N
import Text.Pretty.Semantic

result :: String -> T.Text -> T.Text
result f c = fst . flip runNamey firstName $ do
  let parsed = requireJust f c $ runParser f (L.fromStrict c) parseTops
      prettyErrs = vsep . map (N.format (N.fileSpans [(f, c)] N.defaultHighlight))

  (resolved, _) <- requireRight f c <$> resolveProgram RS.builtinScope RS.emptyModules parsed
  desugared <- desugarProgram resolved
  (inferred, env) <- requireThat f c <$> inferProgram builtinsEnv desugared
  v <- genName
  case runVerify env v (verifyProgram inferred) of
    Left es -> pure (displayPlain (prettyErrs (toList es)))
    Right () -> pure T.empty

tests :: IO TestTree
tests = testGroup "Verification tests" <$> goldenDir result "tests/verify/" ".ml"
