{-# LANGUAGE FlexibleContexts, UndecidableInstances, GADTs #-}
module Control.Monad.Infer
  ( module M
  , InferT, Infer
  , TypeError(..)
  , Constraint(..)
  , Env(..)
  , lookupTy, fresh, runInferT, runInfer, extend
  , lookupKind, extendKind
  )
  where

import Control.Monad.Except as M
import Control.Monad.Gen as M
import Control.Monad.Identity
import Control.Monad.Reader as M
import Control.Monad.Writer.Strict as M hiding ((<>))

import Data.Semigroup
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Data.Text (Text)
import Text.Printf (printf)
import qualified Data.Text as T

import Pretty hiding (local, (<>))
import Syntax
import Syntax.Subst

type InferT p m = GenT Int (ReaderT Env (WriterT [Constraint p] (ExceptT TypeError m)))
type Infer p = InferT p Identity

data Env
  = Env { values :: Map.Map (Var 'ParsePhase) (Type 'TypedPhase)
        , types  :: Map.Map (Var 'ParsePhase) (Type 'TypedPhase)
        }
  deriving (Eq, Show, Ord)

instance Substitutable Env where
  ftv Env{ values = s } = ftv (Map.elems s)
  apply s env@Env{ values = e} = env { values = Map.map (apply s) e}

instance Monoid Env where
  mappend = (<>)
  mempty = Env mempty mempty

instance Semigroup Env where
  Env a b <> Env a' b' = Env (a `mappend` a') (b `mappend` b')

data Constraint p
  = ConUnify (Expr p) (Type p) (Type p)

data TypeError where
  NotEqual :: Pretty (Var p)
           => Type p -> Type p -> TypeError
  Occurs   :: Pretty (Var p)
           => Var p -> Type p -> TypeError
  NotInScope :: Var 'ParsePhase -> TypeError
  EmptyMatch :: Pretty (Var p)
             => Expr p -> TypeError
  EmptyBegin :: ( Pretty (Var p)
                , Pretty (Ann p) )
             => Expr p -> TypeError
  ArisingFrom :: (Pretty (Ann p), Pretty (Var p))
              => TypeError -> Expr p -> TypeError
  ExpectedArrow :: Pretty (Var p) => Type p -> TypeError

lookupTy :: (MonadError TypeError m, MonadReader Env m, MonadGen Int m) => Var 'ParsePhase -> m (Type 'TypedPhase)
lookupTy x = do
  rs <- asks (Map.lookup x . values)
  case rs of
    Just t -> instantiate t
    Nothing -> throwError (NotInScope x)

lookupKind :: (MonadError TypeError m, MonadReader Env m) => Var 'ParsePhase -> m (Type 'TypedPhase)
lookupKind x = do
  rs <- asks (Map.lookup x . types)
  case rs of
    Just t -> pure t
    Nothing -> throwError (NotInScope x)

runInfer :: Env -> Infer a b -> Either TypeError (b, [Constraint a])
runInfer ct ac = runExcept (runWriterT (runReaderT (runGenT ac) ct))

runInferT :: Monad m => Env -> InferT a m b -> m (Either TypeError (b, [Constraint a]))
runInferT ct ac = runExceptT (runWriterT (runReaderT (runGenT ac) ct))

fresh :: MonadGen Int m => m Text
fresh = do
  x <- gen
  pure (alpha !! x)

extend :: MonadReader Env m => (Var 'TypedPhase, Type 'TypedPhase) -> m a -> m a
extend (v, t) = local (\x -> x { values = Map.insert (lowerVar v) t (values x) })

extendKind :: MonadReader Env m => (Var 'TypedPhase, Type 'TypedPhase) -> m a -> m a
extendKind (v, t) = local (\x -> x { types = Map.insert (lowerVar v) t (types x) })

alpha :: [Text]
alpha = map T.pack $ [1..] >>= flip replicateM ['a'..'z']

instantiate :: MonadGen Int m => Type 'TypedPhase -> m (Type 'TypedPhase)
instantiate (TyForall vs _ ty) = do
  f <- map TyVar <$> mapM (const (flip TvName internalTyVar <$> fresh)) vs
  instantiate (apply (Map.fromList (zip vs f)) ty)
instantiate ty = pure ty

lowerVar :: Var 'TypedPhase -> Var 'ParsePhase
lowerVar (TvName x _) = Name x
lowerVar (TvRefresh k _) = lowerVar k

instance Substitutable (Type p) => Substitutable (Constraint p) where
  ftv (ConUnify _ a b) = ftv a `Set.union` ftv b
  apply s (ConUnify e a b) = ConUnify e (apply s a) (apply s b)

instance Pretty (Var p) => Pretty (Constraint p) where
  pprint (ConUnify e a b) = e <+> opClr " <=> " <+> a <+> opClr " ~ " <+> b

instance Show TypeError where
  show (NotEqual a b) = printf "Type error: failed to unify `%s` with `%s`" (prettyPrint a) (prettyPrint b)
  show (Occurs v t) = printf "Occurs check: Variable `%s` occurs in `%s`" (prettyPrint v) (prettyPrint t)
  show (NotInScope e) = printf "Variable not in scope: `%s`" (prettyPrint e)
  show (EmptyMatch e) = printf "Empty match expression:\n%s" (prettyPrint e)
  show (EmptyBegin v) = printf "%s: Empty match expression" (prettyPrint (extract v))
  show (ArisingFrom t v) = printf "%s: %s\n · Arising from use of `%s`" (prettyPrint (extract v)) (show t) (prettyPrint v)
  show (ExpectedArrow a) = printf "Kind error: expected `type -> k`, but got `%s`" (prettyPrint a)

