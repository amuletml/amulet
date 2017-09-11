{-# LANGUAGE FlexibleContexts #-}
module Control.Monad.Infer
  ( module M
  , InferM
  , TypeError(..)
  , Env(..)
  , lookupTy, fresh, runInfer, extend
  , lookupKind, extendKind
  )
  where

import Control.Monad.Writer.Strict as M
import Control.Monad.Reader as M
import Control.Monad.Except as M
import Control.Monad.Gen as M
import qualified Data.Map.Strict as Map

import Data.Semigroup

import Syntax
import Syntax.Subst

type InferM = GenT Int (ReaderT Env (WriterT [Constraint] (Except TypeError)))

data Env
  = Env { values :: Map.Map Var Type
        , types  :: Map.Map Var Kind
        }
  deriving (Eq, Show, Ord)

instance Substitutable Env where
  ftv (Env { values = s }) = ftv (Map.elems s)
  apply s (env@Env { values = e}) = env { values = Map.map (apply s) e}

instance Monoid Env where
  Env a b `mappend` Env a' b' = Env (a `mappend` a') (b `mappend` b')
  mempty = Env mempty mempty

instance Semigroup Env where
  (<>) = mappend

data TypeError
  = NotEqual Type Type
  | Occurs String Type
  | NotInScope Var
  | EmptyMatch Expr
  | EmptyBegin

  | KindsNotEqual Kind Kind
  | ExpectedArrowKind Kind
  deriving (Eq, Show, Ord)

lookupTy :: (MonadError TypeError m, MonadReader Env m, MonadGen Int m) => Var -> m Type
lookupTy x = do
  rs <- asks (Map.lookup x . values)
  case rs of
    Just t -> instantiate t
    Nothing -> throwError (NotInScope x)

lookupKind :: (MonadError TypeError m, MonadReader Env m, MonadGen Int m) => Var -> m Kind
lookupKind x = do
  rs <- asks (Map.lookup x . types)
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
extend (v, t) = local (\x -> x { values = Map.insert v t (values x) })

extendKind :: MonadReader Env m => (Var, Kind) -> m a -> m a
extendKind (v, t) = local (\x -> x { types = Map.insert v t (types x) })

alpha :: [String]
alpha = [1..] >>= flip replicateM ['a'..'z']

instantiate :: MonadGen Int m => Type -> m Type
instantiate (TyForall vs _ ty) = do
  f <- map TyVar <$> mapM (const fresh) vs
  instantiate (apply (Map.fromList (zip vs f)) ty)
instantiate ty = pure ty
