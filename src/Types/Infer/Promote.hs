{-# LANGUAGE FlexibleContexts #-}
module Types.Infer.Promote where

import Control.Monad.Infer

import Syntax.Pretty

import Pretty

promote :: MonadInfer Typed m => Expr Resolved -> m (Type Resolved)
promote (VarRef v _) = pure (TyPromotedCon v)
promote (App f x _) = TyApp <$> promote f <*> promote x
promote x = throwError (NotPromotable' x (string "this is too wip"))

promotePat :: MonadInfer Typed m => Pattern Resolved -> m (Type Resolved)
promotePat (Capture v _) = pure (TyVar v)
promotePat (Destructure f Nothing _) = pure (TyPromotedCon f)
promotePat (Destructure f (Just p) _) = TyApp (TyPromotedCon f) <$> promotePat p
promotePat Wildcard{} = TyVar <$> fresh
promotePat x = throwError (NotPromotable' x (string "this too is wip"))
