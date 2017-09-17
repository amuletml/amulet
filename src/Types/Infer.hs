{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Types.Infer where

import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Control.Monad.Infer
import Control.Arrow

import Syntax.Subst
import Syntax

import Types.Unify

import qualified Data.Text as T

-- Solve for the types of lets in a program
inferProgram :: [Toplevel a] -> Either (TypeError a) Env
inferProgram ct = fst <$> runInfer builtinsEnv (inferProg ct)

tyUnit, tyBool, tyInt, tyString :: Type
tyInt = TyCon (Name "int")
tyString = TyCon (Name "string")
tyBool = TyCon (Name "bool")
tyUnit = TyCon (Name "unit")

builtinsEnv :: Env
builtinsEnv = Env (M.fromList ops) (M.fromList tps) where
  op x t = (Name x, t)
  tp x = (Name x, KiType)
  intOp = tyInt `TyArr` (tyInt `TyArr` tyInt)
  stringOp = tyString `TyArr` (tyString `TyArr` tyString)
  intCmp = tyInt `TyArr` (tyInt `TyArr` tyBool)
  cmp = TyForall ["a"] [] $ TyVar "a" `TyArr` (TyVar "a" `TyArr` tyBool)
  ops = [ op "+" intOp, op "-" intOp, op "*" intOp, op "/" intOp, op "**" intOp
        , op "^" stringOp
        , op "<" intCmp, op ">" intCmp, op ">=" intCmp, op "<=" intCmp
        , op "==" cmp, op "<>" cmp ]
  tps = [ tp "int", tp "string", tp "bool", tp "unit" ]

unify :: Expr a ->  Type -> Type -> Infer a ()
unify e a b = tell [ConUnify e a b]

infer :: Expr a -> Infer a Type
infer expr
  = case expr of
      VarRef k _ -> lookupTy k
      Literal c _ -> case c of
                       LiInt _ -> pure tyInt
                       LiStr _ -> pure tyString
                       LiBool _ -> pure tyBool
                       LiUnit -> pure tyUnit
      Fun p b _ -> do
        (tc, ms) <- inferPattern (unify expr) p
        tb <- extendMany ms $ infer b
        pure (TyArr tc tb)
      Begin [] _ -> throwError (EmptyBegin expr)
      Begin xs _ -> last <$> mapM infer xs
      If c t e _ -> do
        (tc, tt, te) <- (,,) <$> infer c <*> infer t <*> infer e
        unify c tyBool tc
        unify expr tt te
        pure te
      App e1 e2 _ -> do
        (t1, t2, tv) <- (,,) <$> infer e1 <*> infer e2 <*> (TyVar <$> fresh)
        unify expr t1 (TyArr t2 tv)
        pure tv
      Let ns b _ -> do
        ks <- forM ns $ \(a, _) -> do
          tv <- TyVar <$> fresh
          pure (a, tv)
        -- We add each binding to scope with a fresh type variable
        extendMany ks $ do
          -- Then infer the actual types
          ts <- forM ns $ \(a, t) -> do
            t' <- infer t
            pure (a, t')
          -- And finally infer the body
          extendMany ts (infer b)
      Match t ps _ -> do
        tt <- infer t
        tbs <- forM ps $ \(p, e) -> do
          (pt, ks) <- inferPattern (unify expr) p
          unify expr tt pt
          extendMany ks $ infer e
        case tbs of
          [] -> throwError (EmptyMatch expr)
          [x] -> pure x
          (x:xs) -> do
            mapM_ (unify expr x) xs
            pure x
      BinOp l o r p -> do
        infer (App (App o l p) r p)

inferKind :: Type -> Infer a Kind
inferKind (TyVar v) = lookupKind (Name v) `catchError` const (pure KiType)
inferKind (TyCon v) = lookupKind v
inferKind (TyForall vs _ k) = extendManyK (zip (map Name vs) (repeat KiType)) $ inferKind k
inferKind (TyArr a b) = do
  _ <- inferKind a
  _ <- inferKind b
  pure KiType
inferKind (TyApp a b) = do
  x <- inferKind a
  case x of
    KiArr t bd -> do
      xb <- inferKind b
      when (t /= xb) $ throwError (KindsNotEqual t xb)
      pure bd
    _ -> throwError (ExpectedArrowKind x)

-- Returns: Type of the overall thing * type of captures
inferPattern :: (Type -> Type -> Infer a ()) -> Pattern -> Infer a (Type, [(Var, Type)])
inferPattern _ Wildcard = do
  x <- TyVar <$> fresh
  pure (x, [])
inferPattern _ (Capture v) = do
  x <- TyVar <$> fresh
  pure (x, [(v, x)])
inferPattern unify (Destructure cns ps) = do
  pty <- lookupTy cns
  let args (TyArr a b) = a:args b
      args k = [k]
      tys = init (args pty)
      res = last (args pty)
  ptts <- mapM (inferPattern unify) ps
  zipWithM_ unify (map fst ptts) tys
  pure (res, concatMap snd ptts)
inferPattern unify (PType p t) = do
  (t', xs) <- inferPattern unify p
  unify t' t
  pure (t, xs)

inferProg :: [Toplevel a] -> Infer a Env
inferProg (LetStmt ns:prg) = do
  ks <- forM ns $ \(a, _) -> do
    tv <- TyVar <$> fresh
    vl <- lookupTy a `catchError` const (pure tv)
    pure (a, vl)
  extendMany ks $ do
    ts <- inferLetTy ks ns
    extendMany ts (inferProg prg)
inferProg (ValStmt v t:prg) = extend (v, closeOver t) $ inferProg prg
inferProg (ForeignVal v _ t:prg) = extend (v, closeOver t) $ inferProg prg
inferProg (TypeDecl n tvs cs:prg) =
  let mkk [] = KiType
      mkk (_:xs) = KiArr KiType (mkk xs)
      mkt [] = foldl TyApp (TyCon n) (TyVar . T.pack <$> tvs)
      mkt (x:xs) = TyArr x (mkt xs)
   in extendKind (n, mkk tvs) $
      extendMany (map (second mkt) cs) $
        inferProg prg
inferProg [] = ask

inferLetTy :: [(Var, Type)] -> [(Var, Expr a)] -> Infer a [(Var, Type)]
inferLetTy ks [] = pure ks
inferLetTy ks ((va, ve):xs) = extendMany ks $ do
  (ty, c) <- censor (const mempty) (listen (infer ve))
  vt <- case solve mempty c of
          Left e -> throwError e
          Right x -> let ty' = apply x ty
                      in do _ <- inferKind ty'
                            pure ty'
  inferLetTy (updateAlist va vt ks) xs

updateAlist :: Eq a => a -> b -> [(a, b)] -> [(a, b)]
updateAlist n v ((n', x):xs)
  | n == n' = (n, v):updateAlist n v xs
  | otherwise = (n', x):updateAlist n v xs
updateAlist _ _ [] = []

extendMany :: MonadReader Env m => [(Var, Type)] -> m a -> m a
extendMany ((v, t):xs) b = extend (v, t) $ extendMany xs b
extendMany [] b = b

extendManyK :: MonadReader Env m => [(Var, Kind)] -> m a -> m a
extendManyK ((v, t):xs) b = extendKind (v, t) $ extendManyK xs b
extendManyK [] b = b

closeOver :: Type -> Type
closeOver a = forall fv a where
  fv = S.toList . ftv $ a
  forall [] a = a
  forall vs a = TyForall vs [] a
