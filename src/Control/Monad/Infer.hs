module Control.Monad.Infer
  ( module M
  , InferM
  , TypeError(..)
  , Env(..)
  , lookupTy, fresh, runInfer, extend
  )
  where

import Control.Monad.Writer.Strict as M
import Control.Monad.Reader as M
import Control.Monad.Except as M
import Control.Monad.Gen as M
import qualified Data.Map.Strict as Map

import Data.Semigroup

import Syntax

type InferM = GenT Int (ReaderT Env (WriterT [Constraint] (Except TypeError)))

data Env
  = Env { scope :: Map.Map Var Type }
  deriving (Eq, Show, Ord)

instance Monoid Env where
  Env a `mappend` Env b = Env (a `mappend` b)
  mempty = Env mempty

instance Semigroup Env where
  (<>) = mappend

data TypeError
  = NotEqual Type Type
  | Occurs String Type
  | NotInScope Var
  deriving (Eq, Show, Ord)

lookupTy :: (MonadError TypeError m, MonadReader Env m) => Var -> m Type
lookupTy x = do
  rs <- asks (Map.lookup x . scope)
  case rs of
    Just t -> pure t
    Nothing -> throwError (NotInScope x)

runInfer :: Env -> InferM a -> Either TypeError (a, [Constraint])
runInfer ct ac = runExcept (runWriterT (runReaderT (runGenT ac) ct))

fresh :: MonadGen Int m => m String
fresh = do
  x <- gen
  pure (alpha !! x)

extend :: MonadReader Env m => (Var, Type) -> m a -> m a
extend (v, t) = local (Env . Map.insert v t . scope)

alpha :: [String]
alpha = [1..] >>= flip replicateM ['a'..'z']
