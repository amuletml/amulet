module Test.Types.Check (tests) where

import Test.Tasty
import Test.Util

import Control.Monad.Infer (TypeError(..), values, types)
import Control.Applicative hiding (empty)
import Control.Monad.Gen
import Control.Lens ((^.), to)

import qualified Data.ByteString.Builder as B
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Maybe

import Parser.Wrapper (ParseResult(..), runParser)
import Parser

import Syntax.Resolve (resolveProgram)
import Types.Infer (inferProgram, builtinsEnv)
import qualified Syntax.Resolve.Scope as RS
import Syntax.Desugar (desugarProgram)
import Syntax.Types (difference, toMap)

import Syntax.Pretty()
import Pretty

result :: String -> T.Text -> String
result file contents = runGen $ do
  let POK _ parsed = runParser file (B.toLazyByteString $ T.encodeUtf8Builder contents) parseInput
  Right (resolved, _) <- resolveProgram RS.builtinScope RS.emptyModules parsed
  desugared <- desugarProgram resolved
  inferred <- inferProgram builtinsEnv desugared

  pure . displayDecorated decoratePlain . renderPretty 0.8 120 . (<##>empty)
       . either (pretty . reportT) (reportEnv . snd) $ inferred

  where
    reportT :: TypeError -> TypeError
    reportT err = fromMaybe err (innermostError err)

    reportEnv env =
      let env' = difference env builtinsEnv
      in  vsep $
          map reportComponent (Map.toList (env' ^. values . to toMap)) ++
          map reportComponent (Map.toList (env' ^. types . to toMap))

    reportComponent (v, t) = pretty v <+> colon <+> pretty t


    innermostError e@(ArisingFrom err _) = innermostError err <|> Just e
    innermostError _ = Nothing

tests :: IO TestTree
tests = testGroup "Tests.Types.Check" <$> goldenDir result "tests/types/" ".ml"
