module Test.Syntax.Resolve (tests) where

import Test.Tasty
import Test.Util

import Control.Applicative
import Control.Monad.Gen

import qualified Data.ByteString.Builder as B
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import Data.Spanned
import Data.Maybe

import Parser.Wrapper (ParseResult(..), runParser)
import Parser

import Syntax.Resolve (ResolveError(..), resolveProgram)
import qualified Syntax.Resolve.Scope as RS

import Syntax.Pretty()
import Pretty

result :: String -> T.Text -> String
result file contents =
  case runParser file (B.toLazyByteString $ T.encodeUtf8Builder contents) parseInput of
    PFailed es -> show $ vsep (map (\e -> pretty (annotation e) <> colon <+> pretty e) es) </> string ""
    POK _ parsed ->
      let resolved = runGen (resolveProgram RS.builtinScope RS.emptyModules parsed)
      in case resolved of
           Left e -> render (pretty (reportR e))
           Right (r, _) -> render (pretty r)

  where
    render = display . renderPretty 0.8 120

    reportR :: ResolveError -> ResolveError
    reportR err = fromMaybe err (innermostError err)
      where
        innermostError e@(ArisingFrom err _) = innermostError err <|> Just e
        innermostError e@(ArisingFromTop err _) = innermostError err <|> Just e
        innermostError _ = Nothing




tests :: TestTree
tests = testGroup "Test.Syntax.Resolve" (map (goldenFile result "tests/resolve/") files)

files :: [String]
files =
  [ "fail_let_ambiguity.ml"
  , "fail_pattern_ambiguity.ml"
  , "fail_modules.ml"
  , "pass_modules.ml"
  , "pass_shadowing.ml"
  ]
