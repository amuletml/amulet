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

go :: (MonadReader Env m, MonadGen Int m) => Expr Typed -> m (Type Typed)
go (App f x _) = TyApp <$> go f <*> go x
go Hole{} = freshTV
go xs = pure (TyTerm xs)

promotePat :: MonadInfer Typed m => Pattern Resolved -> m (Type Resolved)
promotePat (Capture v _) = pure (TyVar v)
promotePat (Destructure f Nothing an) = pure (TyTerm (VarRef f an))
promotePat (Destructure f (Just p) an) = TyApp (TyTerm (VarRef f an)) <$> promotePat p
promotePat Wildcard{} = TyVar <$> fresh
promotePat x = throwError (NotPromotable x (string "this too is wip"))

isValue :: MonadReader Env m => Expr Typed -> m Bool
isValue (Fun _ e _) = isValue e
isValue (Tuple es _) = and <$> traverse isValue es
isValue (VarRef v _) = liftA2 (||) (fmap (v' `inScope`) (view relevantTVs)) (fmap (v' `Set.member`) (view constructors)) where
  v' = unTvName v
isValue Literal{} = pure True
isValue (App f x _) = liftA2 (&&) (isValue f) (isValue x)
isValue (Record rs _) = and <$> traverse (isValue . snd) rs
isValue (RecordExt x rs _) = liftA2 (&&) (and <$> traverse (isValue . snd) rs) (isValue x)
isValue Let{} = pure False
isValue If{} = pure False
isValue Begin{} = pure False
isValue Match{} = pure False
isValue Function{} = pure False
isValue BinOp{} = pure False
isValue Hole{} = pure True
isValue Ascription{} = pure False
isValue Access{} = pure False
isValue LeftSection{} = pure False
isValue RightSection{} = pure False
isValue BothSection{} = pure False
isValue AccessSection{} = pure False
isValue (Parens e _) = isValue e
isValue TupleSection{} = pure False
isValue (ExprWrapper _ e _) = isValue e
