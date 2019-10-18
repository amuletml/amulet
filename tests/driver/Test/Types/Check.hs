module Test.Types.Check (tests) where

import Test.Tasty.HUnit
import Test.Util

import Control.Monad.Infer (names, firstName)
import Control.Monad.IO.Class
import Control.Monad.Namey
import Control.Lens ((^.), to, runIdentity)

import qualified Data.Text.Lazy as L
import qualified Data.Text.IO as T
import qualified Data.Text as T
import qualified Data.Map as Map

import Parser.Wrapper (runParser)
import Parser

import Syntax.Resolve (ResolveResult(..), resolveProgram)
import Syntax.Resolve.Import (runNullImport)
import Types.Infer (inferProgram)
import Syntax.Desugar (desugarProgram)
import Syntax.Types (difference, toMap)
import Syntax.Builtin

import Core.Lower
import Core.Lint

import System.Directory
import System.FilePath

import qualified Text.Pretty.Note as N
import Text.Pretty.Semantic hiding ((</>))

result :: String -> T.Text -> T.Text
result f c = runIdentity . flip evalNameyT firstName $ do
  let parsed = requireJust f c $ runParser f (L.fromStrict c) parseTops
  ResolveResult resolved _ _ <- requireRight f c <$> runNullImport (resolveProgram builtinResolve parsed)

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

checkLint :: String -> Assertion
checkLint file = flip evalNameyT firstName $ do
  c <- liftIO (T.readFile file)
  let parsed = requireJust file c $ runParser file (L.fromStrict c) parseTops
  ResolveResult resolved _ _ <- requireRight file c <$> runNullImport (resolveProgram builtinResolve parsed)

  desugared <- desugarProgram resolved
  inferred <- toEither <$> inferProgram builtinEnv desugared
  case inferred of
    Left _ -> pure ()
    Right (prg, _) -> do
      lowered <- runLowerT (lowerProg prg)
      case runLintOK (checkStmt emptyScope lowered) of
        Nothing -> pure ()
        Just (_, es) -> liftIO $ assertFailure ("Core lint failed:\n" ++ T.unpack (displayDetailed (pretty es)))

tests :: IO TestTree
tests = do
  golden <- traverse (\(group, dir) -> testGroup group <$> goldenDir result dir ".ml") groups
  lint <- traverse makeLint groups
  pure $ testGroup "Types" [ testGroup "Inference" golden, testGroup "Lint" lint ]
  where
    makeLint (group, dir) = do
      files <- filter (isExtensionOf "ml") <$> listDirectory dir
      pure $ testGroup group (map (\x -> testCase x (checkLint (dir </> x))) files)
    groups =
      [ ("Default tests", "tests/types/")
      , ("GADT tests", "tests/types/gadt/")
      , ("Rank-N tests", "tests/types/rankn/")
      , ("Automatic laziness tests", "tests/types/lazy/")
      , ("Let generalization tests", "tests/types/letgen/")
      , ("Type wildcard tests", "tests/types/wildcards/")
      , ("Type class tests", "tests/types/class/")
      , ("Visible type application tests", "tests/types/vta/")
      , ("Type synonym tests ", "tests/types/synonym/")
      , ("Type function tests ", "tests/types/tyfun/")
      ]
