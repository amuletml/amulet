module Test.Syntax.Resolve (tests) where

import Test.Tasty
import Test.Util

import Control.Monad.Infer

import qualified Data.Text.Lazy as L
import qualified Data.Text as T

import Parser.Wrapper (runParser)
import Parser

import qualified Syntax.Resolve.Scope as RS
import Syntax.Resolve (resolveProgram)
import Syntax.Pretty()

import qualified Text.Pretty.Note as N
import Text.Pretty.Semantic

result :: String -> T.Text -> T.Text
result f c = fst . flip runNamey firstName $ do
  let parsed = requireJust f c $ runParser f (L.fromStrict c) parseTops
  resolved <- resolveProgram RS.builtinScope RS.emptyModules parsed
  pure . displayPlainVerbose . either prettyErrs ((Right<$>) . pretty . fst) $ resolved

  where prettyErrs = vsep . map (N.format (N.fileSpans [(f, c)] N.defaultHighlight))

tests :: IO TestTree
tests = testGroup "Tests.Syntax.Resolve" <$> goldenDir result "tests/resolve/" ".ml"
