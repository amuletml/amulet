module Test.Types.Check (tests) where

import Test.Tasty.HUnit
import Test.Util

import Control.Monad.Infer (names, firstName)
import Control.Monad.IO.Class
import Control.Monad.Namey
import Control.Monad
import Control.Lens ((^.), to, runIdentity)

import qualified Data.Text.Lazy as L
import qualified Data.Text.IO as T
import qualified Data.Text as T
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Functor
import Data.Maybe

import Parser.Wrapper (runParser)
import Parser

import Syntax.Resolve (ResolveResult(..), resolveProgram)
import Syntax.Resolve.Import (runNullImport)
import Types.Infer (inferProgram)
import Syntax.Desugar (desugarProgram)
import Syntax.Types (difference, toMap)
import Syntax.Builtin

import Core.Lower
import Core.Simplify
import Core.Lint

import System.Directory
import System.FilePath

import qualified Text.Pretty.Note as N
import Text.Pretty.Semantic hiding ((</>))

result :: String -> T.Text -> T.Text
result file contents = runIdentity . flip evalNameyT firstName $ do
  let parsed = requireJust name contents $ runParser name (L.fromStrict contents) parseTops
  ResolveResult resolved _ _ <- requireRight name contents <$> runNullImport (resolveProgram builtinResolve parsed)

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

    name = T.pack file
    prettyErrs = vsep . map (N.format (N.fileSpans [(name, contents)] N.defaultHighlight))

checkLint :: Bool -> String -> Assertion
checkLint optm file = flip evalNameyT firstName $ do
  contents <- liftIO (T.readFile file)
  let name = T.pack file
      parsed = requireJust name contents $ runParser name (L.fromStrict contents) parseTops
  ResolveResult resolved _ _ <- requireRight name contents <$> runNullImport (resolveProgram builtinResolve parsed)

  desugared <- desugarProgram resolved
  inferred <- toEither <$> inferProgram builtinEnv desugared
  case inferred of
    Left _ -> pure ()
    Right (prg, _) -> do
      lowered <- runLowerT (lowerProg prg)

      case runLintOK (checkStmt emptyScope lowered) of
        Nothing -> pure ()
        Just (_, es) -> liftIO $ assertFailure ("Core lint failed:\n" ++ T.unpack (displayDetailed (pretty es)))
      -- Optimise runs lower by default
      when optm (optimise defaultInfo { useLint = True } lowered $> ())

tests :: IO TestTree
tests = do
  golden <- traverse (\(group, dir) -> testGroup group <$> goldenDir result dir ".ml") groups
  lint <- traverse makeLint groups
  pure $ testGroup "Types" [ testGroup "Inference" golden, testGroup "Lint" lint ]
  where
    makeLint (group, dir) =
          testGroup group
        . mapMaybe (\x ->
            let full = dir </> x in
            if Set.member full exclude
            then Nothing
            else Just (testCase x (checkLint False full)))
        . filter (isExtensionOf "ml")
      <$> listDirectory dir
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
    exclude = Set.fromList
      [ "tests/types/class/type-sk.ml"
      , "tests/types/class/n-queens.ml"
      ]
