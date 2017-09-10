module Types.Infer where

import qualified Data.Set as S

import Control.Monad.Infer
import Syntax.Subst
import Syntax

import Types.Unify
import Control.Arrow

-- Solve for the type of an expression
inferExpr :: Env -> Expr -> Either TypeError Type
inferExpr ct e = do
  (ty, c) <- runInfer ct (infer e)
  subst <- solve c
  pure . closeOver . apply subst $ ty

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
        unify tv t2
        pure tv
      Let ns b -> do
        ks <- forM ns $ \(a, _) -> do
          tv <- TyVar <$> fresh
          pure (a, tv)
        extendMany ks $ do
          ts <- forM ns $ \(a, t) -> do
            t <- infer t
            pure (a, t)
          extendMany ts (infer b)

extendMany :: MonadReader Env m => [(Var, Type)] -> m a -> m a
extendMany ((v, t):xs) b = extend (v, t) $ extendMany xs b
extendMany [] b = b

closeOver :: Type -> Type
closeOver a = forall fv a where
  fv = S.toList . ftv $ a
  forall [] a = a
  forall vs a = TyForall vs [] a
