{-# LANGUAGE FlexibleContexts, OverloadedStrings, TupleSections #-}
module Types.Infer(inferProgram) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Control.Monad.Infer

import Syntax.Subst
import Syntax

import Types.Unify

-- Solve for the types of lets in a program
inferProgram :: [Toplevel 'ParsePhase a] -> Either (TypeError a) ([Toplevel 'TypedPhase a], Env)
inferProgram ct = fst <$> runInfer builtinsEnv (inferProg ct)

tyUnit, tyBool, tyInt, tyString :: Type 'TypedPhase
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
  cmp = TyForall [Name "a"] [] $ TyVar (Name "a") `TyArr` (TyVar (Name "a") `TyArr` tyBool)
  ops = [ op "+" intOp, op "-" intOp, op "*" intOp, op "/" intOp, op "**" intOp
        , op "^" stringOp
        , op "<" intCmp, op ">" intCmp, op ">=" intCmp, op "<=" intCmp
        , op "==" cmp, op "<>" cmp ]
  tps = [ tp "int", tp "string", tp "bool", tp "unit" ]

unify :: Expr 'ParsePhase a ->  Type 'TypedPhase -> Type 'TypedPhase -> Infer a ()
unify e a b = tell [ConUnify e a b]

infer :: Expr 'ParsePhase a -> Infer a (Expr 'TypedPhase a, Type 'TypedPhase)
infer expr
  = case expr of
      VarRef k a -> (VarRef k a,) <$> lookupTy k
      Literal c a -> case c of
                       LiInt _ -> pure (Literal c a, tyInt)
                       LiStr _ -> pure (Literal c a, tyString)
                       LiBool _ -> pure (Literal c a, tyBool)
                       LiUnit   -> pure (Literal c a, tyUnit)
      Fun p b a -> do
        (p', tc, ms) <- inferPattern (unify expr) p
        (b', tb) <- extendMany ms $ infer b
        pure (Fun p' b' a, TyArr tc tb)
      Begin [] _ -> throwError (EmptyBegin expr)
      Begin xs a -> do
        (xs', txs) <- unzip <$> mapM infer xs
        pure (Begin xs' a, last txs)
      If c t e a -> do
        (c', tc) <- infer c
        (t', tt) <- infer t
        (e', te) <- infer e
        unify c tyBool tc
        unify expr tt te
        pure (If c' t' e' a, te)
      App e1 e2 a -> do
        (e1', t1) <- infer e1
        (e2', t2) <- infer e2
        tv <- TyVar <$> Name <$> fresh
        unify expr t1 (TyArr t2 tv)
        pure (App e1' e2' a, tv)
      Let ns b a -> do
        ks <- forM ns $ \(v, _) -> do
          tv <- TyVar <$> Name <$> fresh
          pure (v, tv)
        -- We add each binding to scope with a fresh type variable
        extendMany ks $ do
          -- Then infer the actual types
          (ns', ts) <- unzip <$> forM ns (\(v, t) -> do
            (t', tt) <- infer t
            pure ((v, t'), (v, tt)))
          -- And finally infer the body
          -- TODO: Return the correct thing from here
          (b', ty) <- extendMany ts (infer b)
          pure (Let ns' b' a, ty)
      Match t ps a -> do
        (t', tt) <- infer t
        (ps', tbs) <- unzip <$> (forM ps $ \(p, e) -> do
          (p', pt, ks) <- inferPattern (unify expr) p
          unify expr tt pt
          (e', ty) <- extendMany ks (infer e)
          pure ((p', e'), ty))
        ty <- case tbs of
                [] -> throwError (EmptyMatch expr)
                [x] -> pure x
                (ty:xs) -> do
                  mapM_ (unify expr ty) xs
                  pure ty
        pure (Match t' ps' a, ty)
      BinOp l o r a -> do
        (l', tl) <- infer l
        (o', to) <- infer o
        (r', tr) <- infer r
        tv <- TyVar <$> Name <$> fresh
        unify expr to (TyArr tl (TyArr tr tv))
        pure (BinOp l' o' r' a, tv)

inferKind :: Type 'ParsePhase -> Infer a (Type 'TypedPhase, Kind)
inferKind (TyVar v) = (TyVar v,) <$> lookupKind v `catchError` const (pure KiType)
inferKind (TyCon v) = (TyCon v,) <$> lookupKind v
inferKind (TyForall vs c k) = do
  (k, t') <- extendManyK (zip vs (repeat KiType)) $ inferKind k
  c' <- map fst <$> mapM inferKind c
  pure (TyForall vs c' k, t')
inferKind (TyArr a b) = do
  (a', ka) <- inferKind a
  (b', kb) <- inferKind b
  -- TODO: A "proper" unification system
  if ka /= KiType then throwError (KindsNotEqual KiType ka) else pure ()
  if kb /= KiType then throwError (KindsNotEqual KiType kb) else pure ()
  pure (TyArr a' b', KiType)
inferKind (TyApp a b) = do
  (a', x) <- inferKind a
  case x of
    KiArr t bd -> do
      (b', xb) <- inferKind b
      when (t /= xb) $ throwError (KindsNotEqual t xb)
      pure (TyApp a' b', bd)
    _ -> throwError (ExpectedArrowKind x)

-- Returns: Type of the overall thing * type of captures
inferPattern :: (Type 'TypedPhase -> Type 'TypedPhase -> Infer a ())
             -> Pattern 'ParsePhase
             -> Infer a (Pattern 'TypedPhase, Type 'TypedPhase, [(Var 'TypedPhase, Type 'TypedPhase)])
inferPattern _ Wildcard = do
  x <- TyVar <$> Name <$> fresh
  pure (Wildcard, x, [])
inferPattern _ (Capture v) = do
  x <- TyVar <$> Name <$> fresh
  pure (Capture v, x, [(v, x)])
inferPattern unify (Destructure cns ps) = do
  pty <- lookupTy cns
  let args (TyArr a b) = a:args b
      args k = [k]
      tys = init (args pty)
      res = last (args pty)
  (ps', ptys, pvs) <- unzip3 <$> mapM (inferPattern unify) ps
  zipWithM_ unify ptys tys
  pure (Destructure cns ps', res, concat pvs)
inferPattern unify (PType p t) = do
  (p', pt, vs) <- inferPattern unify p
  (t', _) <- inferKind t
  unify pt t'
  pure (PType p' t', pt, vs)

inferProg :: [Toplevel 'ParsePhase a] -> Infer a ([Toplevel 'TypedPhase a], Env)
inferProg (LetStmt ns:prg) = do
  ks <- forM ns $ \(a, _) -> do
    tv <- TyVar <$> Name <$> fresh
    vl <- lookupTy a `catchError` const (pure tv)
    pure (a, vl)
  extendMany ks $ do
    (ns', ts) <- inferLetTy ks ns
    consFst (LetStmt ns') $ extendMany ts (inferProg prg)
inferProg (ValStmt v t:prg) = do
  (t', _) <- inferKind t
  consFst (ValStmt v t') $ extend (v, closeOver t') $ inferProg prg
inferProg (ForeignVal v d t:prg) = do
  (t', _) <- inferKind t
  consFst (ForeignVal v d t') $ extend (v, closeOver t') $ inferProg prg
inferProg (TypeDecl n tvs cs:prg) =
  let mkk [] = KiType
      mkk (_:xs) = KiArr KiType (mkk xs)
      mkt [] = foldl TyApp (TyCon n) (TyVar <$> tvs :: [Type 'TypedPhase])
      mkt (x:xs) = TyArr x (mkt xs)
   in extendKind (n, mkk tvs) $ do
      cs' <- forM cs (\(v, ty) -> do
                         (ty', _) <- unzip <$> mapM inferKind ty
                         pure (v, ty'))
      consFst (TypeDecl n tvs cs') $ extendMany (map (fmap mkt) cs') $
        inferProg prg
inferProg [] = ([],) <$> ask

inferLetTy :: [(Var 'TypedPhase, Type 'TypedPhase)]
           -> [(Var 'ParsePhase, Expr 'ParsePhase a)]
           -> Infer a ([(Var 'TypedPhase, Expr 'TypedPhase a)], [(Var 'TypedPhase, Type 'TypedPhase)])
inferLetTy ks [] = pure ([], ks)
inferLetTy ks ((va, ve):xs) = extendMany ks $ do
  ((ve', ty), c) <- censor (const mempty) (listen (infer ve))
  vt <- case solve mempty c of
          Left e -> throwError e
          Right x -> pure (apply x ty)
  consFst (va, ve') $ inferLetTy (updateAlist va vt ks) xs

updateAlist :: Eq a => a -> b -> [(a, b)] -> [(a, b)]
updateAlist n v (x@(n', _):xs)
  | n == n' = (n, v):updateAlist n v xs
  | otherwise = x:updateAlist n v xs
updateAlist _ _ [] = []

extendMany :: MonadReader Env m => [(Var 'TypedPhase, Type 'TypedPhase)] -> m a -> m a
extendMany ((v, t):xs) b = extend (v, t) $ extendMany xs b
extendMany [] b = b

extendManyK :: MonadReader Env m => [(Var 'TypedPhase, Kind)] -> m a -> m a
extendManyK ((v, t):xs) b = extendKind (v, t) $ extendManyK xs b
extendManyK [] b = b

closeOver :: Type 'TypedPhase -> Type 'TypedPhase
closeOver a = forall fv a where
  fv = S.toList . ftv $ a
  forall [] a = a
  forall vs a = TyForall vs [] a

consFst :: Functor m => a -> m ([a], b) -> m ([a], b)
consFst a m = (\(x, y) -> (a:x, y)) <$> m
