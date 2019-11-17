module Test.Tasty.Lsp
  ( module M
  , assertIn
  , lspSessionWith
  , lspSession
  , workspace
  , range
  , resolveCodeLens
  ) where

import Control.Monad.IO.Class
import Control.Exception

import Data.Aeson.Types
import Data.Default

import Language.Haskell.LSP.Types.Capabilities as M
import Language.Haskell.LSP.Types as M
import Language.Haskell.LSP.Test as M

import Test.Tasty.HUnitPretty as M
import Test.Tasty.Providers

-- | Test an LSP session, starting in a given folder with some configuration options.
lspSessionWith :: FilePath -> Maybe Value -> ClientCapabilities -> TestName -> Session () -> TestTree
lspSessionWith root config caps name session = testCase name $
  runSessionWithConfig cfg program caps root session
  where
    program = "amulet-lsp --log=-"
    cfg = def
      { lspConfig = config
      , messageTimeout = 2
      -- , logStdErr = True
      }

-- | Test an LSP session with the default workspace and configuration options.
lspSession :: TestName -> Session () -> TestTree
lspSession = lspSessionWith workspace Nothing fullCaps

-- | Run an assertion within the session.
assertIn :: Assertion -> Session ()
assertIn = liftIO

-- | The default workspace
workspace :: FilePath
workspace = "tests/editor/workspace"

-- | A helper method for constructing ranges
range :: Int -> Int -> Int -> Int -> Range
range l1 c1 l2 c2 = Range (Position l1 c1) (Position l2 c2)

-- | Checks the response for errors and throws an exception if needed.
-- Returns the result if successful.
getResponseResult :: ResponseMessage a -> a
getResponseResult (ResponseMessage _ _ (Just res) _) = res
getResponseResult (ResponseMessage _ id Nothing (Just err)) = throw (UnexpectedResponseError id err)
getResponseResult (ResponseMessage _ _ Nothing Nothing) = error "No response or error given"

-- | Resolve a code lens.
resolveCodeLens :: CodeLens -> Session CodeLens
resolveCodeLens lens = getResponseResult <$> request CodeLensResolve lens
