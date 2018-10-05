module Test.Syntax.Verify (tests) where

import Test.Tasty
import Test.Util

import Control.Monad.Infer

import qualified Data.Text.Lazy as L
import qualified Data.Text as T
import Data.Foldable
import Data.These

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

toEither :: These a b -> Either a b
toEither (This e) = Left e
toEither (These e _) = Left e
toEither (That x) = Right x


result :: String -> T.Text -> T.Text
result file contents = fst . flip runNamey firstName $ do
  let (Just parsed, _) = runParser file (L.fromStrict contents) parseTops
      prettyErrs = vsep . map (N.format (N.fileSpans [(file, contents)] N.defaultHighlight))
  Right (resolved, _) <- resolveProgram RS.builtinScope RS.emptyModules parsed
  desugared <- desugarProgram resolved
  Right (inferred, _) <- toEither <$> inferProgram builtinsEnv desugared
  case runVerify (verifyProgram inferred) of
    Left es -> pure (displayPlain (prettyErrs (toList es)))
    Right () -> pure T.empty

tests :: IO TestTree
tests = testGroup "Verification tests" <$> goldenDir result "tests/verify/" ".ml"
