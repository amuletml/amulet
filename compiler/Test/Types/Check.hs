module Test.Types.Check (tests) where

import Test.Tasty
import Test.Util

import Control.Monad.Infer (values, types)
import Control.Monad.Gen
import Control.Lens ((^.), to)

import qualified Data.Text.Lazy as L
import qualified Data.Text as T
import qualified Data.Map as Map

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

result :: String -> T.Text -> T.Text
result file contents = runGen $ do
  let (Just parsed, _) = runParser file (L.fromStrict contents) parseTops
  Right (resolved, _) <- resolveProgram RS.builtinScope RS.emptyModules parsed
  desugared <- desugarProgram resolved
  inferred <- inferProgram builtinsEnv desugared

  pure . displayPlain
       . either prettyErrs ((Right<$>) . reportEnv . snd) $ inferred

  where
    reportEnv env =
      let env' = difference env builtinsEnv
      in  vsep $
          map reportComponent (Map.toList (env' ^. values . to toMap)) ++
          map reportComponent (Map.toList (env' ^. types . to toMap))

    reportComponent (v, t) = pretty v <+> colon <+> pretty t

    prettyErrs = vsep . map (N.format (N.fileSpans [(file, contents)]))

tests :: IO TestTree
tests = do
  inference <- testGroup "Type inference tests" <$> goldenDir result "tests/types/" ".ml"
  gadts <- testGroup "GADT inference tests" <$> goldenDir result "tests/gadt/" ".ml"
  rankn <- testGroup "Rank-N inference tests" <$> goldenDir result "tests/rankn/" ".ml"
  visinst <- testGroup "Visible Instantiation inference tests" <$> goldenDir result "tests/visinst/" ".ml"
  lazy <- testGroup "Automatic laziness tests" <$> goldenDir result "tests/lazy/" ".ml"
  pure (testGroup "Type inference" [ inference, gadts, rankn, visinst, lazy ])
