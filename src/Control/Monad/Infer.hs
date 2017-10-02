{-# LANGUAGE FlexibleContexts #-}
module Control.Monad.Infer
  ( module M
  , InferT, Infer
  , TypeError(..)
  , Env(..)
  , lookupTy, fresh, runInferT, runInfer, extend
  , lookupKind, extendKind
  )
  where

import Control.Monad.Writer.Strict as M
import Control.Monad.Reader as M
import Control.Monad.Except as M
import Control.Monad.Gen as M
import Control.Monad.Identity
import Control.Comonad (extract)

import qualified Data.Map.Strict as Map

import Data.Semigroup

import qualified Data.Text as T
import Data.Text (Text)

import Syntax.Subst
import Syntax

import Pretty (prettyPrint, Pretty)

import Text.Printf (printf)

type InferT a m = GenT Int (ReaderT Env (WriterT [Constraint a] (ExceptT (TypeError a) m)))
type Infer a = InferT a Identity

data Env
  = Env { values :: Map.Map (Var 'ParsePhase) (Type 'TypedPhase)
        , types  :: Map.Map (Var 'ParsePhase) Kind
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

data TypeError a
  = NotEqual (Type 'TypedPhase) (Type 'TypedPhase)
  | Occurs (Var 'TypedPhase) (Type 'TypedPhase)
  | NotInScope (Var 'ParsePhase)
  | EmptyMatch (Expr 'ParsePhase a)
  | EmptyBegin (Expr 'ParsePhase a)
  | EmptyMultiWayIf (Expr 'ParsePhase a)
  | ArisingFrom (TypeError a) (Expr 'ParsePhase a)

  | KindsNotEqual Kind Kind
  | ExpectedArrowKind Kind
  deriving (Eq, Ord)

lookupTy :: (MonadError (TypeError a) m, MonadReader Env m, MonadGen Int m) => Var 'ParsePhase -> m (Type 'TypedPhase)
lookupTy x = do
  rs <- asks (Map.lookup x . values)
  case rs of
    Just t -> instantiate t
    Nothing -> throwError (NotInScope x)

lookupKind :: (MonadError (TypeError a) m, MonadReader Env m, MonadGen Int m) => Var 'ParsePhase -> m Kind
lookupKind x = do
  rs <- asks (Map.lookup x . types)
  case rs of
    Just t -> pure t
    Nothing -> throwError (NotInScope x)

runInfer :: Env -> Infer a b -> Either (TypeError a) (b, [Constraint a])
runInfer ct ac = runExcept (runWriterT (runReaderT (runGenT ac) ct))

runInferT :: Monad m => Env -> InferT a m b -> m (Either (TypeError a) (b, [Constraint a]))
runInferT ct ac = runExceptT (runWriterT (runReaderT (runGenT ac) ct))

fresh :: MonadGen Int m => m Text
fresh = do
  x <- gen
  pure (alpha !! x)

extend :: MonadReader Env m => (Var 'TypedPhase, Type 'TypedPhase) -> m a -> m a
extend (v, t) = local (\x -> x { values = Map.insert v t (values x) })

extendKind :: MonadReader Env m => (Var 'TypedPhase, Kind) -> m a -> m a
extendKind (v, t) = local (\x -> x { types = Map.insert v t (types x) })

alpha :: [Text]
alpha = map T.pack $ [1..] >>= flip replicateM ['a'..'z']

instantiate :: MonadGen Int m => Type 'TypedPhase -> m (Type 'TypedPhase)
instantiate (TyForall vs _ ty) = do
  f <- map TyVar <$> mapM (const (Name <$> fresh)) vs
  instantiate (apply (Map.fromList (zip vs f)) ty)
instantiate ty = pure ty

instance (Pretty a) => Show (TypeError a) where
  show (NotEqual a b) = printf "Type error: failed to unify `%s` with `%s`" (prettyPrint a) (prettyPrint b)
  show (Occurs v t) = printf "Occurs check: Variable `%s` occurs in `%s`" (prettyPrint v) (prettyPrint t)
  show (NotInScope e) = printf "Variable not in scope: `%s`" (prettyPrint e)
  show (EmptyMatch e) = printf "Empty match expression:\n%s" (prettyPrint e)
  show (EmptyBegin v) = printf "%s: Empty match expression" (prettyPrint (extract v))
  show (EmptyMultiWayIf v) = printf "Empty multi-way if expression" (prettyPrint (extract v))

  show (ArisingFrom t v) = printf "%s: %s\n · Arising from use of `%s`" (prettyPrint (extract v)) (show t) (prettyPrint v)

  show (KindsNotEqual a b) = printf "Kind error: failed to unify `%s` with `%s`" (prettyPrint a) (prettyPrint b)
  show (ExpectedArrowKind a) = printf "Kind error: expected `type -> k`, but got `%s`" (prettyPrint a)
