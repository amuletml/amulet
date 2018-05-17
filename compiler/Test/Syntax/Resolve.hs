module Test.Syntax.Resolve (tests) where

import Test.Tasty
import Test.Util

import Control.Monad.Gen

import qualified Data.Text.Lazy as L
import qualified Data.Text as T

import Parser.Wrapper (ParseResult(..), runParser)
import Parser

import qualified Syntax.Resolve.Scope as RS
import Syntax.Resolve (resolveProgram)

import Syntax.Pretty()
import Pretty

result :: String -> T.Text -> String
result file contents = runGen $ do
  let POK _ parsed = runParser file (L.fromStrict contents) parseInput
  resolved <- resolveProgram RS.builtinScope RS.emptyModules parsed
  pure . display . renderPretty 0.8 120 . (<##>empty)
       . either (vsep . map pretty) (pretty . fst) $ resolved

tests :: IO TestTree
tests = testGroup "Tests.Syntax.Resolve" <$> goldenDir result "tests/resolve/" ".ml"
