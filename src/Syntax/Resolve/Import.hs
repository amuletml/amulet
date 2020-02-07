{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving #-}
module Syntax.Resolve.Import
  ( SearchedIn(..)
  , ImportResult(..)
  , MonadImport(..)
  , NullImport
  , runNullImport
  ) where

import Control.Monad.Trans.Class
import Control.Monad.Chronicles
import Control.Monad.Reader
import Control.Monad.Namey

import qualified Data.List.NonEmpty as E
import Data.Text (Text)
import Data.Position
import Data.Span

import Syntax.Resolve.Scope
import Syntax.Var

-- | Where a library was searched for, in the event that it could not be
-- located.
data SearchedIn
  = Relative FilePath -- ^ This was a relative import, resolving to a single location.
  | LibraryPath [FilePath] -- ^ The list of paths along the library path that were searched.
  deriving Show

data ImportResult
  -- | The module was successfully imported. This contains the "name" of
  -- the module, and its contents.
  = Imported (Var Resolved) Signature
  | Errored -- ^ The module errored while importing.
  -- | An import loop, with the a list of import locations and what they
  -- try to load.
  | ImportCycle (E.NonEmpty (SourceName, Span))
  | NotFound SearchedIn -- ^ The module could not be found while importing.
  deriving Show

-- | A monad with which other modules may be imported.
class Monad m => MonadImport m where
  -- | Import a module with a given name (usually a file path).
  --
  -- The 'Span' should be the location of the import node - this is used
  -- for error reporting in the event of an import cycle.
  importModule :: Span -> Text -> m ImportResult

instance MonadImport m => MonadImport (ReaderT e m) where
  importModule s path = lift (importModule s path)

instance (Semigroup c, MonadImport m) => MonadImport (ChronicleT c m) where
  importModule s path = lift (importModule s path)

newtype NullImport m a = Null { runNullImport :: m a }
  deriving newtype (Functor, Applicative, Monad, MonadNamey)

instance Monad m => MonadImport (NullImport m) where
  importModule _ _ = pure (NotFound (LibraryPath []))

instance MonadTrans NullImport where
  lift = Null
