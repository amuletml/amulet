{-# LANGUAGE FlexibleContexts #-}
module Types.Infer where

import qualified Data.Set as S

import Control.Monad.Infer
import Syntax.Subst
import Syntax

import Types.Unify
import Control.Arrow
import Debug.Trace

-- Solve for the type of an expression
inferExpr :: Env -> Expr -> Either TypeError Type
inferExpr ct e = do
  (ty, c) <- runInfer ct (infer e)
  traceShow c $ pure ()
  subst <- solve mempty c
  pure . closeOver . apply subst $ ty

-- Solve for the types of lets in a program
inferProgram :: [Toplevel] -> Either TypeError Env
inferProgram ct = fst <$> runInfer mempty (inferProg ct)

tyBool, tyInt, tyString :: Type
tyInt = TyCon (Name "int")
tyString = TyCon (Name "string")
tyBool = TyCon (Name "bool")

infer :: Expr -> InferM Type
infer x
  = case x of
      VarRef k -> lookupTy k
      Literal c -> case c of
                     LiInt _ -> pure tyInt
                     LiStr _ -> pure tyString
                     LiBool _ -> pure tyBool
      Fun c b -> do
        tc <- TyVar <$> fresh
        tb <- extend (c, tc) $ infer b
        pure (TyArr tc tb)
      Begin xs -> do
        last <$> mapM infer xs
      If c t e -> do
        (tc, tt, te) <- (,,) <$> infer c <*> infer t <*> infer e
        tyBool `unify` tc
        tt `unify` te
        pure te
      App e1 e2 -> do
        (t1, t2, tv) <- (,,) <$> infer e1 <*> infer e2 <*> (TyVar <$> fresh)
        unify t1 (TyArr t2 tv)
        pure tv
      Let ns b -> do
        ks <- forM ns $ \(a, _) -> do
          tv <- TyVar <$> fresh
          pure (a, tv)
        -- We add each binding to scope with a fresh type variable
        extendMany ks $ do
          -- Then infer the actual types
          ts <- forM ns $ \(a, t) -> do
            t <- infer t
            pure (a, t)
          -- And finally infer the body
          extendMany ts (infer b)

inferProg :: [Toplevel] -> InferM Env
inferProg (LetStmt ns:prg) = do
  ks <- forM ns $ \(a, _) -> do
    tv <- TyVar <$> fresh
    pure (a, tv)
  extendMany ks $ do
    ts <- forM ns $ \(a, t) -> do
      -- We need the normalised, generalised type
      ctx <- ask
      let tp = inferExpr ctx t
      case tp of
        Left e -> throwError e
        Right x -> pure (a, x)
    extendMany ts (inferProg prg)
inferProg (ValStmt v t:prg) = extend (v, t) $ inferProg prg
inferProg (ForeignVal v _ t:prg) = extend (v, t) $ inferProg prg
inferProg [] = ask

extendMany :: MonadReader Env m => [(Var, Type)] -> m a -> m a
extendMany ((v, t):xs) b = extend (v, t) $ extendMany xs b
extendMany [] b = b

closeOver :: Type -> Type
closeOver a = forall fv a where
  fv = S.toList . ftv $ a
  forall [] a = a
  forall vs a = TyForall vs [] a
