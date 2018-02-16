{-# LANGUAGE FlexibleContexts
  , ConstraintKinds
  , TupleSections #-}

module Syntax.Resolve
  ( resolveProgram
  , runResolve
  , ResolveError(..)
  ) where

import qualified Data.Map.Strict as Map
import Data.Triple

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Gen

import Data.Traversable
import Data.Foldable

import Syntax.Resolve.Scope
import Syntax.Subst
import Syntax

data ResolveError
  = NotInScope (Var Parsed)
  | EmptyMatch (Expr Parsed)
  | EmptyBegin (Expr Parsed)

  | ArisingFrom ResolveError (Expr Parsed)
  | ArisingFromTop ResolveError (Toplevel Parsed)
  deriving (Eq, Ord, Show)

type MonadResolve m = (MonadError ResolveError m, MonadReader Scope m, MonadGen Int m)

resolveProgram :: MonadGen Int m => [Toplevel Parsed] -> m (Either ResolveError [Toplevel Resolved])
resolveProgram = runResolve . resolveModule

runResolve :: MonadGen Int m => ReaderT Scope (ExceptT ResolveError m) a -> m (Either ResolveError a)
runResolve = runExceptT . flip runReaderT builtinScope

resolveModule :: MonadResolve m => [Toplevel Parsed] -> m [Toplevel Resolved]
resolveModule [] = pure []
resolveModule (r:rs) = flip catchError (throwError . flip ArisingFromTop r)
  $ case r of
      LetStmt vs -> do
        let vars = map fst3 vs
        vars' <- traverse tagVar vars
        extendN (zip vars vars') $ (:)
          <$> (LetStmt
                <$> traverse (\(v, e, a) -> (,,) <$> lookupEx v <*> reExpr e <*> pure a) vs
                )
          <*> resolveModule rs
      ForeignVal v t ty a -> do
        v' <- tagVar v
        extend (v, v') $ (:)
          <$> (ForeignVal
               <$> lookupEx v
                <*> pure t
                <*> reType (wrap ty)
                <*> pure a)
          <*> resolveModule rs
      TypeDecl t vs cs -> do
        t'  <- tagVar t
        vs' <- traverse tagVar vs
        let c = map extractCons cs
        c' <- traverse tagVar c
        extendTy (t, t') $ extendN (zip c c') $ (:)
          <$> extendTyN (zip vs vs') (TypeDecl t' vs' <$> traverse resolveCons cs)
          <*> resolveModule rs
     where resolveCons (UnitCon v a) = UnitCon <$> lookupEx v <*> pure a
           resolveCons (ArgCon v t a) = ArgCon <$> lookupEx v <*> reType t <*> pure a

           extractCons (UnitCon v _) = v
           extractCons (ArgCon v _ _) = v

           wrap x = TyForall (toList (ftv x)) x

lookupEx :: MonadResolve m => Var Parsed -> m (Var Resolved)
lookupEx v = do
  env <- ask
  case Map.lookup v (varScope env) of
    Nothing -> throwError (NotInScope v)
    Just x -> pure x

lookupTy :: MonadResolve m => Var Parsed -> m (Var Resolved)
lookupTy v = do
  env <- ask
  case Map.lookup v (tyScope env) of
    Nothing -> throwError (NotInScope v)
    Just x -> pure x

reExpr :: MonadResolve m => Expr Parsed -> m (Expr Resolved)
reExpr r@(VarRef v a) = VarRef
                    <$> catchError (lookupEx v) (throwError . flip ArisingFrom r)
                    <*> pure a
reExpr (Let vs c a) = do
  let vars = map fst3 vs
  vars' <- traverse tagVar vars
  extendN (zip vars vars') $ Let <$> traverse (\(v, e, a) -> (,,) <$> lookupEx v
                                                                  <*> reExpr e
                                                                  <*> pure a)
                                              vs
                                 <*> reExpr c
                                 <*> pure a
reExpr (If c t b a) = If <$> reExpr c <*> reExpr t <*> reExpr b <*> pure a
reExpr (App f p a) = App <$> reExpr f <*> reExpr p <*> pure a
reExpr (Fun p e a) = do
  (p', vs, ts) <- rePattern p
  extendTyN ts . extendN vs $ Fun p' <$> reExpr e <*> pure a
reExpr r@(Begin [] _) = throwError (EmptyBegin r)
reExpr (Begin es a) = Begin <$> traverse reExpr es <*> pure a
reExpr (Literal l a) = pure (Literal l a)
reExpr r@(Match _ [] _) = throwError (EmptyMatch r)
reExpr r@(Match e ps a) = do
  e' <- reExpr e
  ps' <- traverse (\(p, b) -> do
                  (p', vs, ts) <- catchError (rePattern p) (throwError . flip ArisingFrom r)
                  (p',) <$> extendTyN ts (extendN vs (reExpr b)))
              ps
  pure (Match e' ps' a)
reExpr (BinOp l o r a) = BinOp <$> reExpr l <*> reExpr o <*> reExpr r <*> pure a
reExpr (Hole v a) = Hole <$> tagVar v <*> pure a
reExpr r@(Ascription e t a) = Ascription
                          <$> reExpr e
                          <*> catchError (reType t) (throwError . flip ArisingFrom r)
                          <*> pure a
reExpr (Record fs a) = Record <$> traverse (traverse reExpr) fs <*> pure a
reExpr (RecordExt e fs a) = RecordExt
                        <$> reExpr e
                        <*> traverse (traverse reExpr) fs
                        <*> pure a
reExpr (Access e t a) = Access <$> reExpr e <*> pure t <*> pure a
reExpr (LeftSection o r a) = LeftSection <$> reExpr o <*> reExpr r <*> pure a
reExpr (RightSection l o a) = RightSection <$> reExpr l <*> reExpr o <*> pure a
reExpr (BothSection o a) = BothSection <$> reExpr o <*> pure a
reExpr (AccessSection t a) = pure (AccessSection t a)

reExpr (Tuple es a) = Tuple <$> traverse reExpr es <*> pure a
reExpr (TypeApp f x a) = TypeApp <$> reExpr f <*> reType x <*> pure a

reType :: MonadResolve m => Type Parsed -> m (Type Resolved)
reType (TyCon v) = TyCon <$> lookupTy v
reType (TyVar v) = TyVar <$> lookupTy v
reType (TySkol v) = error ("impossible! resolving skol " ++ show v)
reType (TyForall vs ty) = do
  vs' <- traverse tagVar vs
  ty' <- extendTyN (zip vs vs') $ reType ty
  pure (TyForall vs' ty')
reType (TyArr l r) = TyArr <$> reType l <*> reType r
reType (TyApp l r) = TyApp <$> reType l <*> reType r
reType (TyRows t r) = TyRows <$> reType t
                               <*> traverse (\(a, b) -> (a,) <$> reType b) r
reType (TyExactRows r) = TyExactRows <$> traverse (\(a, b) -> (a,) <$> reType b) r
reType (TyTuple ta tb) = TyTuple <$> reType ta <*> reType tb

rePattern :: MonadResolve m
          => Pattern Parsed
          -> m (Pattern Resolved, [(Var Parsed, Var Resolved)]
                                , [(Var Parsed, Var Resolved)])
rePattern (Wildcard a) = pure (Wildcard a, [], [])
rePattern (Capture v a) = do
  v' <- tagVar v
  pure (Capture v' a, [(v, v')], [])
rePattern (Destructure v Nothing a) = do
  v' <- lookupEx v
  pure (Destructure v' Nothing a, [(v, v')], [])
rePattern (Destructure v p a) = do
  v' <- lookupEx v
  (p', vs, ts) <- case p of
    Nothing -> pure (Nothing, [], [])
    Just pat -> do
      (p', vs, ts) <- rePattern pat
      pure (Just p', vs, ts)
  pure (Destructure v' p' a, (v, v') : vs, ts)
rePattern (PType p t a) = do
  (p', vs, ts) <- rePattern p
  let fvs = toList (ftv t)
  fresh <- for fvs $ \x -> do
    new <- tagVar x
    lookupTy x `catchError` const (pure new)
  t' <- extendTyN (zip fvs fresh) (reType t)
  pure (PType p' t' a, vs, zip fvs fresh ++ ts)
rePattern (PRecord f a) = do
  (f', vss, tss) <- unzip3 <$> traverse (\(n, p) -> do
                                       (p', vs, ts) <- rePattern p
                                       pure ((n, p'), vs, ts))
                              f
  pure (PRecord f' a, concat vss, concat tss)
rePattern (PTuple ps a) = do
  (ps', vss, tss) <- unzip3 <$> traverse rePattern ps
  pure (PTuple ps' a, concat vss, concat tss)
