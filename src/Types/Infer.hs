{-# LANGUAGE FlexibleContexts, OverloadedStrings, TupleSections, GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Types.Infer(inferProgram) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Set as S

import Control.Monad.Infer
import Control.Arrow

import Syntax.Subst
import Syntax

import Types.Unify

-- Solve for the types of lets in a program
inferProgram :: [Toplevel 'ParsePhase] -> Either TypeError ([Toplevel 'TypedPhase], Env)
inferProgram ct = fst <$> runInfer builtinsEnv (inferProg ct)

tyUnit, tyBool, tyInt, tyString :: Type 'TypedPhase
tyInt = TyCon (TvName "int" TyStar)
tyString = TyCon (TvName "string" TyStar)
tyBool = TyCon (TvName "bool" TyStar)
tyUnit = TyCon (TvName "unit" TyStar)

builtinsEnv :: Env
builtinsEnv = Env (M.fromList ops) (M.fromList tps) where
  op :: T.Text -> b -> (BoundVar, b)
  op x t = (Name x, t)
  tp :: T.Text -> (BoundVar, Type p)
  tp x = (Name x, TyStar)
  intOp = tyInt `TyArr` (tyInt `TyArr` tyInt)
  stringOp = tyString `TyArr` (tyString `TyArr` tyString)
  intCmp = tyInt `TyArr` (tyInt `TyArr` tyBool)
  cmp = TyForall [TvName "a" TyStar] [] $ TyVar (TvName "a" TyStar) `TyArr` (TyVar (TvName "a" TyStar) `TyArr` tyBool)
  ops = [ op "+" intOp, op "-" intOp, op "*" intOp, op "/" intOp, op "**" intOp
        , op "^" stringOp
        , op "<" intCmp, op ">" intCmp, op ">=" intCmp, op "<=" intCmp
        , op "==" cmp, op "<>" cmp ]
  tps :: [(BoundVar, Type p)]
  tps = [ tp "int", tp "string", tp "bool", tp "unit" ]

unify :: Expr 'ParsePhase -> Type 'TypedPhase -> Type 'TypedPhase -> Infer 'TypedPhase ()
unify e a b = tell [ConUnify (raiseE (`tag` internalTyVar) id e) a b]

tag :: Var 'ParsePhase -> Type 'TypedPhase -> Var 'TypedPhase
tag (Name v) t = TvName v t
tag (Refresh k a) t = TvRefresh (tag k t) a

infer :: Expr 'ParsePhase -> Infer 'TypedPhase (Expr 'TypedPhase, Type 'TypedPhase)
infer expr
  = case expr of
      VarRef k a -> do
        x <- lookupTy k
        pure (VarRef (tag k x) a, x)
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
        tv <- TyVar . flip TvName TyStar <$> fresh
        unify expr t1 (TyArr t2 tv)
        pure (App e1' e2' a, tv)
      Let ns b a -> do
        ks <- forM ns $ \(a, _) -> do
          tv <- TyVar . flip TvName TyStar <$> fresh
          pure (tag a tv, tv)
        extendMany ks $ do
          (ns', ts) <- inferLetTy ks ns
          (b', ty) <- extendMany ts (infer b)
          pure (Let ns' b' a, ty)
      Match t ps a -> do
        (t', tt) <- infer t
        (ps', tbs) <- unzip <$> forM ps
            (\ (p, e) ->
               do (p', pt, ks) <- inferPattern (unify expr) p
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
        tv <- TyVar . flip TvName TyStar <$> fresh
        unify expr to (TyArr tl (TyArr tr tv))
        pure (BinOp l' o' r' a, tv)

inferKind :: Type 'ParsePhase -> Infer a (Type 'TypedPhase, Type 'TypedPhase)
inferKind TyStar = pure (TyStar, TyStar)
inferKind (TyVar v) = do
  x <- lookupKind v `catchError` const (pure TyStar)
  pure (TyVar (tag v x), x)
inferKind (TyCon v) = do
  x <- lookupKind v `catchError` const (pure TyStar)
  pure (TyCon (tag v x), x)
inferKind (TyForall vs c k) = do
  (k, t') <- extendManyK (zip (map (`tag` TyStar) vs) (repeat TyStar)) $ inferKind k
  c' <- map fst <$> mapM inferKind c
  pure (TyForall (map (`tag` TyStar) vs) c' k, t')
inferKind (TyArr a b) = do
  (a', ka) <- inferKind a
  (b', kb) <- inferKind b
  -- TODO: A "proper" unification system
  when (ka /= TyStar) $ throwError (NotEqual ka TyStar)
  when (kb /= TyStar) $ throwError (NotEqual kb TyStar)
  pure (TyArr a' b', TyStar)
inferKind (TyApp a b) = do
  (a', x) <- inferKind a
  case x of
    TyArr t bd -> do
      (b', xb) <- inferKind b
      when (t /= xb) $ throwError (NotEqual t xb)
      pure (TyApp a' b', bd)
    _ -> throwError (ExpectedArrow x)

-- Returns: Type of the overall thing * type of captures
inferPattern :: (Type 'TypedPhase -> Type 'TypedPhase -> Infer a ())
             -> Pattern 'ParsePhase
             -> Infer a (Pattern 'TypedPhase, Type 'TypedPhase, [(Var 'TypedPhase, Type 'TypedPhase)])
inferPattern _ Wildcard = do
  x <- TyVar . flip TvName TyStar <$> fresh
  pure (Wildcard, x, [])
inferPattern _ (Capture v) = do
  x <- TyVar . flip TvName TyStar <$> fresh
  pure (Capture (tag v x), x, [(tag v x, x)])
inferPattern unify (Destructure cns ps) = do
  pty <- lookupTy cns
  let args :: Type p -> [Type p]
      args (TyArr a b) = a:args b
      args k = [k]
      tys = init (args pty)
      res = last (args pty)
  (ps', ptys, pvs) <- unzip3 <$> mapM (inferPattern unify) ps
  zipWithM_ unify ptys tys
  pure (Destructure (tag cns pty) ps', res, concat pvs)
inferPattern unify (PType p t) = do
  (p', pt, vs) <- inferPattern unify p
  (t', _) <- inferKind t
  unify pt t'
  pure (PType p' t', pt, vs)

inferProg :: [Toplevel 'ParsePhase] -> Infer 'TypedPhase ([Toplevel 'TypedPhase], Env)
inferProg (LetStmt ns:prg) = do
  ks <- forM ns $ \(a, _) -> do
    tv <- TyVar . flip TvName TyStar <$> fresh
    vl <- lookupTy a `catchError` const (pure tv)
    pure (tag a tv, vl)
  extendMany ks $ do
    (ns', ts) <- inferLetTy ks ns
    extendMany ts $
      consFst (LetStmt ns') $ inferProg prg
inferProg (ValStmt v t:prg) = do
  (t', _) <- inferKind t
  extend (tag v t', closeOver t') $
    consFst (ValStmt (tag v t') t') $ inferProg prg
inferProg (ForeignVal v d t:prg) = do
  (t', _) <- inferKind t
  extend (tag v t', closeOver t') $
    consFst (ForeignVal (tag v t') d t') $ inferProg prg
inferProg (TypeDecl n tvs cs:prg) =
  let mkk :: [a] -> Type p
      mkk [] = TyStar
      mkk (_:xs) = TyArr TyStar (mkk xs)
      mkt = foldr TyArr (foldl TyApp (TyCon (n `tag` TyStar)) (map (TyVar . flip tag TyStar) tvs))
      n' = tag n (mkk tvs)
   in extendKind (n', mkk tvs) $ do
      cs' <- forM cs (\(v, ty) -> do
                         (ty', _) <- unzip <$> mapM inferKind ty
                         pure (tag v (mkt ty'), ty'))
      extendMany (map (fmap mkt) cs') $
        consFst (TypeDecl n' (map (`tag` TyStar) tvs) cs') $
          inferProg prg
inferProg [] = ([],) <$> ask

inferLetTy :: (t ~ 'TypedPhase, p ~ 'ParsePhase)
           => [(Var t, Type t)]
           -> [(Var p, Expr p)]
           -> Infer t ( [(Var t, Expr t)]
                      , [(Var t, Type t)])
inferLetTy ks [] = pure ([], ks)
inferLetTy ks ((va, ve):xs) = extendMany ks $ do
  ((ve', ty), c) <- censor (const mempty) (listen (infer ve))
  (x, vt) <- case solve mempty c of
               Left e -> throwError e
               Right x -> pure (x, apply x ty)
  let r (TvName n t) = TvName n (apply x t)
      r (TvRefresh k a) = TvRefresh (r k) a
      ex = raiseE r id ve'
  consFst (tag va vt, ex) $ inferLetTy (updateAlist (tag va vt) vt ks) xs

-- Monomorphic so we can use "close enough" equality
updateAlist :: Var 'TypedPhase
            -> b
            -> [(Var 'TypedPhase, b)] -> [(Var 'TypedPhase, b)]
updateAlist n v (x@(n', _):xs)
  | n == n' = (n, v):updateAlist n v xs
  | n `closeEnough` n' = (n, v):updateAlist n v xs
  | otherwise = x:updateAlist n v xs
  where 
    closeEnough (TvName a _) (TvName b _) = a == b
    closeEnough (TvRefresh a b) (TvRefresh a' b')
      = a `closeEnough` a' && b' >= b
    closeEnough _ _ = False
updateAlist _ _ [] = []


extendMany :: MonadReader Env m => [(Var 'TypedPhase, Type 'TypedPhase)] -> m a -> m a
extendMany ((v, t):xs) b = extend (v, t) $ extendMany xs b
extendMany [] b = b

extendManyK :: MonadReader Env m => [(Var 'TypedPhase, Type 'TypedPhase)] -> m a -> m a
extendManyK ((v, t):xs) b = extendKind (v, t) $ extendManyK xs b
extendManyK [] b = b

closeOver :: Type 'TypedPhase -> Type 'TypedPhase
closeOver a = forall fv a where
  fv = S.toList . ftv $ a
  forall :: [Var p] -> Type p -> Type p
  forall [] a = a
  forall vs a = TyForall vs [] a

consFst :: Functor m => a -> m ([a], b) -> m ([a], b)
consFst a = fmap (first (a:))
