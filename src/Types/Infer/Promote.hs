{-# LANGUAGE FlexibleContexts #-}
module Types.Infer.Promote where

import qualified Data.Set as Set

import Control.Monad.Infer
import Control.Applicative
import Control.Lens

import Syntax.Pretty
import Syntax.Types

import Pretty

promote :: MonadInfer Typed m => Expr Typed -> m (Type Typed)
promote ex = do
  x <- isValue ex
  if not x
     then throwError (NotPromotable ex (string "is not a value"))
     else go ex

go :: MonadReader Env m => Expr Typed -> m (Type Typed)
go (Cast e _ _) = go e
go var@(VarRef v _) = do
  x <- view constructors
  if unTvName v `Set.member` x
     then pure (TyTerm var)
     else pure (TyVar v)
go (App f x _) = TyApp <$> go f <*> go x
go xs = pure (TyTerm xs)


promotePat :: MonadInfer Typed m => Pattern Resolved -> m (Type Resolved)
promotePat (Capture v _) = pure (TyVar v)
promotePat (Destructure f Nothing an) = pure (TyTerm (VarRef f an))
promotePat (Destructure f (Just p) an) = TyApp (TyTerm (VarRef f an)) <$> promotePat p
promotePat Wildcard{} = TyVar <$> fresh
promotePat x = throwError (NotPromotable x (string "this too is wip"))


isValue, isConcrete :: MonadReader Env m => Expr Typed -> m Bool
isValue VarRef{} = pure True
isValue x = isConcrete x

isConcrete (Fun _ e _) = isConcrete e
isConcrete (Tuple es _) = and <$> traverse isConcrete es
isConcrete (VarRef v _) = do
  x <- view constructors
  pure (unTvName v `Set.member` x)
isConcrete (TypeApp f _ _) = isConcrete f
isConcrete Literal{} = pure True
isConcrete (App f x _) = liftA2 (&&) (isConcrete f) (isValue x)
isConcrete (Record rs _) = and <$> traverse (isConcrete . snd) rs
isConcrete (RecordExt x rs _) = liftA2 (&&) (and <$> traverse (isValue . snd) rs) (isValue x)
isConcrete Let{} = pure False
isConcrete If{} = pure False
isConcrete Begin{} = pure False
isConcrete Match{} = pure False
isConcrete Function{} = pure False
isConcrete BinOp{} = pure False
isConcrete Hole{} = pure True
isConcrete Ascription{} = pure False
isConcrete Access{} = pure False
isConcrete LeftSection{} = pure False
isConcrete RightSection{} = pure False
isConcrete BothSection{} = pure False
isConcrete AccessSection{} = pure False
isConcrete (Parens e _) = isConcrete e
isConcrete TupleSection{} = pure False
isConcrete (Cast e _ _) = isConcrete e
