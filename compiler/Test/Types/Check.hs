module Test.Types.Check (tests) where

import Test.Tasty
import Test.Util

import Control.Monad.Infer (names, firstName)
import Control.Monad.Namey
import Control.Lens ((^.), to, runIdentity)

import qualified Data.Text.Lazy as L
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.These

import Parser.Wrapper (runParser)
import Parser

import Syntax.Resolve (resolveProgram)
import Types.Infer (inferProgram, builtinsEnv)
import qualified Syntax.Resolve.Scope as RS
import Syntax.Desugar (desugarProgram)
import Syntax.Types (difference, toMap)

import Syntax.Pretty()

import qualified Text.Pretty.Note as N
import Text.Pretty.Semantic

toEither :: These a b -> Either a b
toEither (This e) = Left e
toEither (These e _) = Left e
toEither (That x) = Right x

result :: String -> T.Text -> T.Text
result file contents = runIdentity . flip evalNameyT firstName $ do
  let (Just parsed, _) = runParser file (L.fromStrict contents) parseTops
  Right (resolved, _) <- resolveProgram RS.builtinScope RS.emptyModules parsed
  desugared <- desugarProgram resolved
  inferred <- inferProgram builtinsEnv desugared

  pure . displayPlain
       . either prettyErrs ((Right<$>) . reportEnv . snd)
       . toEither $ inferred

  where
    reportEnv env =
      let env' = difference env builtinsEnv
      in  vsep $
          map reportComponent (Map.toList (env' ^. names . to toMap))

    reportComponent (v, t) = pretty v <+> colon <+> pretty t

    prettyErrs = vsep . map (N.format (N.fileSpans [(file, contents)]))

tests :: IO TestTree
tests = do
  inference <- testGroup "Type inference tests" <$> goldenDir result "tests/types/" ".ml"
  gadts <- testGroup "GADT inference tests" <$> goldenDir result "tests/types/gadt/" ".ml"
  rankn <- testGroup "Rank-N inference tests" <$> goldenDir result "tests/types/rankn/" ".ml"
  lazy <- testGroup "Automatic laziness tests" <$> goldenDir result "tests/types/lazy/" ".ml"
  letgen <- testGroup "Let generalization tests" <$> goldenDir result "tests/types/letgen/" ".ml"
  wild <- testGroup "Type wildcard tests" <$> goldenDir result "tests/types/wildcards/" ".ml"
  pure (testGroup "Type inference" [ inference, gadts, rankn, lazy, letgen, wild ])
