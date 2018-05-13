module Test.Syntax.Resolve (tests) where

import Test.Tasty
import Test.Util

import Control.Applicative hiding (empty)
import Control.Monad.Gen

import qualified Data.Text.Lazy as L
import qualified Data.Text as T
import Data.Maybe

import Parser.Wrapper (ParseResult(..), runParser)
import Parser

import Syntax.Resolve (ResolveError(..), resolveProgram)
import qualified Syntax.Resolve.Scope as RS

import Syntax.Pretty()
import Pretty

result :: String -> T.Text -> String
result file contents = runGen $ do
  let POK _ parsed = runParser file (L.fromStrict contents) parseInput
  resolved <- resolveProgram RS.builtinScope RS.emptyModules parsed
  pure . display . renderPretty 0.8 120 . (<##>empty)
       . either (pretty . reportR) (pretty . fst) $ resolved

  where
    reportR :: ResolveError -> ResolveError
    reportR err = fromMaybe err (innermostError err)

    innermostError e@(ArisingFrom err _) = innermostError err <|> Just e
    innermostError e@(ArisingFromTop err _) = innermostError err <|> Just e
    innermostError _ = Nothing

tests :: IO TestTree
tests = testGroup "Tests.Syntax.Resolve" <$> goldenDir result "tests/resolve/" ".ml"
