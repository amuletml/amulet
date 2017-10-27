{-# LANGUAGE FlexibleContexts, UndecidableInstances, GADTs #-}
{-# LANGUAGE StandaloneDeriving, OverloadedStrings #-}
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

import Control.Monad.Writer.Strict as M hiding ((<>))
import Control.Monad.Reader as M
import Control.Monad.Except as M
import Control.Monad.Identity
import Control.Monad.Gen as M

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Semigroup

import Data.Span (internal)

import qualified Data.Text as T
import Text.Printf (printf)
import Data.Text (Text)

import Data.Function

import Pretty hiding (local, (<>))

import Syntax.Subst
import Syntax

type InferT p m = GenT Int (ReaderT Env (WriterT [Constraint p] (ExceptT TypeError m)))
type Infer p = InferT p Identity

data Env
  = Env { values :: Map.Map (Var Parsed) (Type Typed)
        , types  :: Map.Map (Var Parsed) (Type Typed)
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
deriving instance (Show (Var p), Show (Expr p), Show (Type p)) => Show (Constraint p)

data TypeError where
  NotEqual :: Pretty (Var p) => Type p -> Type p -> TypeError
  Occurs   :: Pretty (Var p) => Var p -> Type p -> TypeError
  NotInScope :: Var Parsed -> TypeError
  EmptyMatch :: Pretty (Var p) => Expr p -> TypeError
  EmptyBegin :: ( Pretty (Var p)
                , Pretty (Ann p) )
             => Expr p -> TypeError
  FoundHole :: [Expr Typed] -> TypeError
  ArisingFrom :: (Pretty (Ann p), Pretty (Var p))
              => TypeError -> Expr p -> TypeError
  ArisingFromT :: (Pretty (Ann p), Pretty (Var p))
               => TypeError -> Type p -> TypeError
  ExpectedArrow :: (Pretty (Var p'), Pretty (Var p))
                => Type p' -> Type p -> Type p -> TypeError
  NotPresent :: (Pretty (Var p), Pretty (Var p'))
             => Var p -> [(Var p', Type p')] -> TypeError
  NoOverlap :: (Pretty (Var p), Eq (Var p), Ord (Var p), p ~ Typed) => Type p -> Type p -> TypeError
  Note :: TypeError -> String -> TypeError
  CanNotInstance :: Pretty (Var p)
                 => Type p -> Type p -> Type p -> TypeError

lookupTy :: (MonadError TypeError m, MonadReader Env m, MonadGen Int m) => Var Parsed -> m (Type Typed)
lookupTy x = do
  rs <- asks (Map.lookup x . values)
  case rs of
    Just t -> instantiate t
    Nothing -> throwError (NotInScope x)

lookupKind :: (MonadError TypeError m, MonadReader Env m) => Var Parsed -> m (Type Typed)
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

extend :: MonadReader Env m => (Var Typed, Type Typed) -> m a -> m a
extend (v, t) = local (\x -> x { values = Map.insert (eraseVarTy v) t (values x) })

extendKind :: MonadReader Env m => (Var Typed, Type Typed) -> m a -> m a
extendKind (v, t) = local (\x -> x { types = Map.insert (eraseVarTy v) t (types x) })

alpha :: [Text]
alpha = map T.pack $ [1..] >>= flip replicateM ['a'..'z']

instantiate :: MonadGen Int m => Type Typed -> m (Type Typed)
instantiate (TyForall vs ty _) = do
  f <- map (flip TyVar internal)
        <$> mapM (const (flip TvName internalTyVar <$> fresh)) vs
  instantiate (apply (Map.fromList (zip vs f)) ty)
instantiate ty = pure ty

instance Substitutable (Type p) => Substitutable (Constraint p) where
  ftv (ConUnify _ a b) = ftv a `Set.union` ftv b
  apply s (ConUnify e a b) = ConUnify e (apply s a) (apply s b)

instance Pretty (Var p) => Pretty (Constraint p) where
  pprint (ConUnify e a b) = e
                        <+> opClr (" <=> " :: String)
                        <+> a
                        <+> opClr (" ~ " :: String) <+> b

prettyRows :: (Pretty (Var p)) => [(Var p, Type p)] -> PrettyP
prettyRows = braces
           . interleave (", " :: String)
           . map (\(x, y) -> x <+> opClr (" : " :: String) <+> y)

instance Show TypeError where
  show (NotEqual a b) = printf "Type error: failed to unify `%s` with `%s`" (prettyPrint a) (prettyPrint b)
  show (Occurs v t) = printf "Occurs check: Variable `%s` occurs in `%s`" (prettyPrint v) (prettyPrint t)
  show (NotInScope e) = printf "Variable not in scope: `%s`" (prettyPrint e)
  show (EmptyMatch e) = printf "Empty match expression:\n%s" (prettyPrint e)
  show (EmptyBegin v) = printf "%s: Empty match expression" (prettyPrint (annotation v))
  show (ArisingFrom t v) = printf "%s: %s\n · Arising from use of `%s`" (prettyPrint (annotation v)) (show t) (prettyPrint v)
  show (ArisingFromT t v) = printf "%s: %s\n · Arising from type `%s`" (prettyPrint (annotation v)) (show t) (prettyPrint v)
  show (ExpectedArrow ap k v)
    = printf "Kind error: In application '%s'\n · expected arrow kind, but got `%s` (kind of `%s`)"
      (prettyPrint ap) (prettyPrint k) (prettyPrint v)
  show (NotPresent v r) = printf "Row type `%s` does not have element `%s`" (prettyPrint (prettyRows r))
                                                                            (prettyPrint v)
  show (FoundHole xs) = unlines $ map prnt xs where
    prnt (Hole v s) = printf "%s: Found typed hole `%s` (of type `%s`)" (prettyPrint s)  (prettyPrint v) (prettyPrint (varType v))
    prnt _ = undefined
  show (Note te m) = printf "%s\n · Note: %s" (show te) m
  show (CanNotInstance rho new rec)
    | prettyPrint rho == prettyPrint new
    = printf "Can not instance hole of record type `%s` to type %s" (prettyPrint rec) (prettyPrint rho)
    | otherwise
    = printf "Can not instance hole `%s` (in record type %s) to type %s" (prettyPrint rho) (prettyPrint new) (prettyPrint rec)
  show (NoOverlap ta@(TyExactRows ra _) tb@(TyRows _ rb _))
    = printf "No overlap between exact record `%s` and polymorphic record `%s`\n %s"
        (prettyPrint ta) (prettyPrint tb) (missing ra rb) 
  show (NoOverlap tb@(TyRows _ rb _) ta@(TyExactRows ra _))
    = printf "No overlap between exact record `%s` and polymorphic record `%s`\n %s"
        (prettyPrint ta) (prettyPrint tb) (missing ra rb) 
  show (NoOverlap tb@(TyExactRows rb _) ta@(TyExactRows ra _))
    = printf "No unification between exact records `%s` and `%s`\n %s"
        (prettyPrint ta) (prettyPrint tb) (missing ra rb) 
  show (NoOverlap ta tb) = printf "\x1b[1;32minternal compiler error\x1b[0m: NoOverlap %s %s" (prettyPrint ta) (prettyPrint tb)


missing :: [(Text, b)] -> [(Text, b)] -> Text
missing ra rb
  | length ra < length rb
  = "· Namely, the following fields are missing: " <> T.intercalate ", "
       (map (prettyPrint . tvClr . fst)
            (deleteFirstsBy ((==) `on` fst) rb ra))
  | length ra > length rb
  = "· Namely, the following fields should not be present: " <> T.intercalate ", "
      (map (prettyPrint . tvClr . fst)
           (deleteFirstsBy ((==) `on` fst) ra rb))
  | length ra == length rb
  = "· No fields match (or the compiler is *very* confused)"
missing _ _ = undefined -- freaking GHC

varType :: Var Typed -> Type Typed
varType (TvName _ x) = x
varType (TvRefresh v _) = varType v
