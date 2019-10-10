{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving #-}
module Syntax.Resolve.Import
  ( ImportResult(..)
  , MonadImporter(..)
  , NullImporter
  , runNullImport
  ) where

import Control.Monad.Trans.Class
import Control.Monad.Chronicles
import Control.Monad.Reader
import Control.Monad.Namey

import qualified Data.List.NonEmpty as E
import Data.Text (Text)
import Data.Span

import Syntax.Resolve.Scope
import Syntax.Var

data ImportResult
  -- | The module was successfully imported. This contains the "name" of
  -- the module, and its contents.
  = Imported (Var Resolved) Signature
  | Errored -- ^ The module errored while importing.
  -- | An import loop, with the a list of import locations and what they
  -- try to load.
  | ImportCycle (E.NonEmpty (FilePath, Span))
  | NotFound -- ^ The module could not be found while importing.
  deriving Show

-- | A monad under which other modules may be imported.
class Monad m => MonadImporter m where
  -- | Import a module with a given name (usually a file path).
  --
  -- The 'Span' should be the location of the import node - this is used
  -- for error reporting in the event of an import cycle.
  importModule :: Span -> Text -> m ImportResult

instance MonadImporter m => MonadImporter (ReaderT e m) where
  importModule s path = lift (importModule s path)

instance (Semigroup c, MonadImporter m) => MonadImporter (ChronicleT c m) where
  importModule s path = lift (importModule s path)

newtype NullImporter m a = Null { runNullImport :: m a }
  deriving newtype (Functor, Applicative, Monad, MonadNamey)

instance Monad m => MonadImporter (NullImporter m) where
  importModule _ _ = pure NotFound

instance MonadTrans NullImporter where
  lift = Null
