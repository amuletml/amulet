{-# OPTIONS_GHC -Wno-missing-local-signatures #-}
{-# LANGUAGE FlexibleContexts, OverloadedStrings, TupleSections, GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Types.Infer(inferProgram) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Set as S
import Data.Span (internal, Span)
import Data.Semigroup ((<>))

import Control.Monad.Infer
import Control.Arrow (first)

import Syntax.Subst
import Syntax

import Types.Wellformed
import Types.Unify
import Types.Holes

import Data.List

-- Solve for the types of lets in a program
inferProgram :: [Toplevel Parsed] -> Either TypeError ([Toplevel Typed], Env)
inferProgram ct = fst <$> runInfer builtinsEnv (inferAndCheck ct) where
  inferAndCheck prg = do
    (prg', env) <- inferProg prg
    case findHoles prg' of
      xs@(_:_) -> throwError (FoundHole xs)
      [] -> pure (prg', env)

tyUnit, tyBool, tyInt, tyString :: Type Typed
tyInt = TyCon (TvName "int" (TyStar internal)) internal
tyString = TyCon (TvName "string" (TyStar internal)) internal
tyBool = TyCon (TvName "bool" (TyStar internal)) internal
tyUnit = TyCon (TvName "unit" (TyStar internal)) internal

star :: Ann p ~ Span => Type p
star = TyStar internal

forall :: Ann p ~ Span => [Var p] -> Type p -> Type p
forall a c = TyForall a c internal

app, arr :: Ann p ~ Span => Type p -> Type p -> Type p
arr a b = TyArr a b internal
app a b = TyApp a b internal

var, con :: Ann p ~ Span => Var p -> Type p
var x = TyVar x internal
con x = TyCon x internal

builtinsEnv :: Env
builtinsEnv = Env (M.fromList ops) (M.fromList tps) where
  op :: T.Text -> Type Typed -> (Var Parsed, Type Typed)
  op x t = (Name x, t)
  tp :: T.Text -> (Var Parsed, Type Typed)
  tp x = (Name x, star)

  intOp = tyInt `arr` (tyInt `arr` tyInt)
  stringOp = tyString `arr` (tyString `arr` tyString)
  intCmp = tyInt `arr` (tyInt `arr` tyBool)

  cmp = forall [TvName "a" star] $ var (TvName "a" star) `arr` (var (TvName "a" star) `arr` tyBool)
  ops = [ op "+" intOp, op "-" intOp, op "*" intOp, op "/" intOp, op "**" intOp
        , op "^" stringOp
        , op "<" intCmp, op ">" intCmp, op ">=" intCmp, op "<=" intCmp
        , op "==" cmp, op "<>" cmp ]
  tps :: [(Var Parsed, Type Typed)]
  tps = [ tp "int", tp "string", tp "bool", tp "unit" ]

unify :: Expr Parsed -> Type Typed -> Type Typed -> Infer Typed ()
unify e a b = tell [ConUnify (raiseE (`tag` internalTyVar) id e) a b]

tag :: Var Parsed -> Type Typed -> Var Typed
tag (Name v) t = TvName v t
tag (Refresh k a) t = TvRefresh (tag k t) a

infer :: Expr Parsed -> Infer Typed (Expr Typed, Type Typed)
infer expr
  = case expr of
      VarRef k a -> do
        x <- lookupTy k
        pure (VarRef (tag k x) a, x)
      Hole v ann -> do
       tv <- flip TyVar ann . flip TvName (TyStar ann) <$> fresh
       pure (Hole (tag v tv) ann, tv)
      Literal c a -> case c of
                       LiInt _ -> pure (Literal c a, tyInt)
                       LiStr _ -> pure (Literal c a, tyString)
                       LiBool _ -> pure (Literal c a, tyBool)
                       LiUnit   -> pure (Literal c a, tyUnit)
      Fun p b a -> do
        (p', tc, ms) <- inferPattern (unify expr) p
        (b', tb) <- extendMany ms $ infer b
        pure (Fun p' b' a, TyArr tc tb a)
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
        tv <- freshTV (annotation e1 <> annotation e2)
        unify expr t1 (TyArr t2 tv (annotation t1 <> annotation t2))
        pure (App e1' e2' a, tv)
      Let ns b ann -> do
        ks <- forM ns $ \(a, _) -> do
          tv <- freshTV ann
          pure (tag a tv, tv)
        extendMany ks $ do
          (ns', ts) <- inferLetTy ks ns
          (b', ty) <- extendMany ts (infer b)
          pure (Let ns' b' ann, ty)
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
        tv <- freshTV a
        unify expr to (TyArr tl (TyArr tr tv a) a)
        pure (BinOp l' o' r' a, tv)
      Record rows a -> do
        itps <- inferRows rows
        let (rows', rowts) = unzip itps
        pure (Record rows' a, TyExactRows rowts a)
      RecordExt rec rows a -> do
        itps <- inferRows rows
        let (rows', newTypes) = unzip itps
        (rec', rho) <- infer rec
        pure (RecordExt rec' rows' a, TyRows rho newTypes a)
      Access rec key a -> do
       rho <- freshTV a
       ktp <- freshTV a
       (rec', tp) <- infer rec
       let rows = (TyRows rho [(key, ktp)] a)
       unify expr tp rows
       pure (Access rec' key a, ktp)
      -- Section handling is quite a hack: We generate an appropriate
      -- lambda and check that instead
      BothSection op _ -> infer op
      LeftSection op vl a -> do
        var <- Name <$> fresh
        infer (Fun (Capture var a) (BinOp (VarRef var a) op vl a) a)
      RightSection vl op a -> do
        var <- Name <$> fresh
        infer (Fun (Capture var a) (BinOp vl op (VarRef var a) a) a)
      AccessSection k a -> do
        var <- Name <$> fresh
        infer (Fun (Capture var a) (Access (VarRef var a) k a) a)

inferRows :: [(T.Text, Expr Parsed)] -> Infer Typed [((T.Text, Expr Typed), (T.Text, Type Typed))]
inferRows rows = forM rows $ \(var', val) -> do
  (val', typ) <- infer val
  pure ((var', val'), (var', typ))

freshTV :: MonadGen Int m => Span -> m (Type Typed)
freshTV a = flip TyVar a . flip TvName (TyStar a) <$> fresh

inferKind :: Type Parsed -> Infer a (Type Typed, Type Typed)
inferKind (TyStar a) = pure (TyStar a, TyStar a)
inferKind (TyVar v a) = do
  x <- lookupKind v `catchError` const (pure (TyStar a))
  pure (TyVar (tag v x) a, x)
inferKind (TyCon v a) = do
  x <- lookupKind v `catchError` const (pure (TyStar a))
  pure (TyCon (tag v x) a, x)
inferKind (TyForall vs k a) = do
  (k, t') <- extendManyK (zip (map (`tag` TyStar a) vs) (repeat (TyStar a))) $
    inferKind k
  pure (TyForall (map (`tag` TyStar a) vs) k a, t')
inferKind (TyArr a b ann) = do
  (a', ka) <- inferKind a
  (b', kb) <- inferKind b
  -- TODO: A "proper" unification system
  when (ka /= TyStar (annotation ka)) $ throwError (NotEqual ka (TyStar ann))
  when (kb /= TyStar (annotation kb)) $ throwError (NotEqual kb (TyStar ann))
  pure (TyArr a' b' ann, TyStar ann)
inferKind tp@(TyRows rho rows ann) = do
  wellformed tp
  case rho of
    TyRows rho' rows' ann' -> inferKind (TyRows rho' (rows' `union` rows) ann')
    TyExactRows rows' ann' -> inferKind (TyExactRows (rows' `union` rows) ann')
    TyVar{} -> do
      (rho', _) <- inferKind rho
      ks <- forM rows $ \(var, typ) -> do
        (typ', _) <- inferKind typ
        pure (var, typ')
      pure (TyRows rho' ks ann, TyStar ann)
    _ -> error "wellformedness check rejects this"
inferKind (TyExactRows rows ann) = do
  ks <- forM rows $ \(var, typ) -> do
    (typ', _) <- inferKind typ
    pure (var, typ')
  pure (TyExactRows ks ann, TyStar ann)
inferKind ap@(TyApp a b ann) = do
  (a', x) <- inferKind a
  case x of
    TyArr t bd _ -> do
      (b', xb) <- inferKind b
      when (crush t /= crush xb) $ throwError (NotEqual t xb)
      pure (TyApp a' b' ann, bd)
    _ -> throwError (ExpectedArrow ap x a')
  where crush = raiseT smush (const internal)

-- Returns: Type of the overall thing * type of captures
inferPattern :: (Type Typed -> Type Typed -> Infer a ())
             -> Pattern Parsed
             -> Infer a (Pattern Typed, Type Typed, [(Var Typed, Type Typed)])
inferPattern _ (Wildcard ann) = do
  x <- flip TyVar ann . flip TvName (TyStar ann) <$> fresh
  pure (Wildcard ann, x, [])
inferPattern _ (Capture v ann) = do
  x <- flip TyVar ann . flip TvName (TyStar ann) <$> fresh
  pure (Capture (tag v x) ann, x, [(tag v x, x)])
inferPattern unify (Destructure cns ps ann) = do
  pty <- lookupTy cns
  let args :: Type p -> [Type p]
      args (TyArr a b _) = a:args b
      args k = [k]
      tys = init (args pty)
      res = last (args pty)
  (ps', ptys, pvs) <- unzip3 <$> mapM (inferPattern unify) ps
  zipWithM_ unify ptys tys
  pure (Destructure (tag cns pty) ps' ann, res, concat pvs)
inferPattern unify (PRecord rows ann) = do
  rho <- freshTV ann
  (rowps, rowts, caps) <- unzip3 <$> forM rows (\(var, pat) -> do
    (p', t, caps) <- inferPattern unify pat
    pure ((var, p'), (var, t), caps))
  pure (PRecord rowps ann, TyRows rho rowts ann, concat caps)

inferPattern unify (PType p t ann) = do
  (p', pt, vs) <- inferPattern unify p
  (t', _) <- inferKind t `catchError` \x -> throwError (ArisingFrom x t)
  unify pt t'
  pure (PType p' t' ann, pt, vs)

inferProg :: [Toplevel Parsed] -> Infer Typed ([Toplevel Typed], Env)
inferProg (LetStmt ns ann:prg) = do
  ks <- forM ns $ \(a, _) -> do
    tv <- flip TyVar internal . flip TvName (TyStar internal) <$> fresh
    vl <- lookupTy a `catchError` const (pure tv)
    pure (tag a tv, vl)
  extendMany ks $ do
    (ns', ts) <- inferLetTy ks ns
    extendMany ts $
      consFst (LetStmt ns' ann) $ inferProg prg
inferProg (ValStmt v t ann:prg) = do
  (t', _) <- inferKind t `catchError` \x -> throwError (ArisingFrom x t)
  extend (tag v t', closeOver t') $
    consFst (ValStmt (tag v t') t' ann) $ inferProg prg
inferProg (ForeignVal v d t ann:prg) = do
  (t', _) <- inferKind t
  extend (tag v t', closeOver t') $
    consFst (ForeignVal (tag v t') d t' ann) $ inferProg prg
inferProg (TypeDecl n tvs cs ann:prg) =
  let mkk :: [a] -> Type Typed
      mkk [] = star
      mkk (_:xs) = arr star (mkk xs)
      mkt = foldr arr (foldl app (con (n `tag` star)) (map (var . flip tag star) tvs))
      n' = tag n (mkk tvs)
      arisingFromConstructor = "Perhaps you're missing a * between two types of a constructor?"
   in extendKind (n', mkk tvs) $ do
      cs' <- forM cs (\(v, ty) -> do
                         (ty', _) <- unzip <$> mapM inferKind ty
                         pure (tag v (mkt ty'), ty'))
        `catchError` \x -> throwError (Note (ArisingFrom x (TyVar n' ann)) arisingFromConstructor)
      extendMany (map (fmap mkt) cs') $
        consFst (TypeDecl n' (map (`tag` star) tvs) cs' ann) $
          inferProg prg
inferProg [] = do
  let ann = internal
  (_, c) <- censor (const mempty) . listen $ do
    x <- lookupTy (Name "main")
    b <- flip TyVar ann . flip TvName (TyStar ann) <$> fresh
    unify (VarRef (Name "main") ann) x (TyArr tyUnit b ann)
  x <- gen
  case solve x mempty c of
    Left e -> throwError (Note e "main must be a function from unit to some type")
    Right _ -> ([],) <$> ask

inferLetTy :: (t ~ Typed, p ~ Parsed)
           => [(Var t, Type t)]
           -> [(Var p, Expr p)]
           -> Infer t ( [(Var t, Expr t)]
                      , [(Var t, Type t)])
inferLetTy ks [] = pure ([], ks)
inferLetTy ks ((va, ve):xs) = extendMany ks $ do
  ((ve', ty), c) <- censor (const mempty) (listen (infer ve))
  cur <- gen
  (x, vt) <- case solve cur mempty c of
               Left e -> throwError e
               Right x -> pure (x, closeOver (apply x ty))
  let r (TvName n t) = TvName n (apply x t)
      r (TvRefresh k a) = TvRefresh (r k) a
      ex = raiseE r id ve'
  (vt', _) <- inferKind (raiseT eraseVarTy id vt)
  consFst (tag va vt', ex) $ inferLetTy (updateAlist (tag va vt') vt' ks) xs

-- Monomorphic so we can use "close enough" equality
updateAlist :: Var Typed
            -> b
            -> [(Var Typed, b)] -> [(Var Typed, b)]
updateAlist n v (x@(n', _):xs)
  | n == n' = (n, v):updateAlist n v xs
  | n `closeEnough` n' = (n, v):updateAlist n v xs
  | otherwise = x:updateAlist n v xs
updateAlist _ _ [] = []



extendMany :: MonadReader Env m => [(Var Typed, Type Typed)] -> m a -> m a
extendMany ((v, t):xs) b = extend (v, t) $ extendMany xs b
extendMany [] b = b

extendManyK :: MonadReader Env m => [(Var Typed, Type Typed)] -> m a -> m a
extendManyK ((v, t):xs) b = extendKind (v, t) $ extendManyK xs b
extendManyK [] b = b

closeOver :: Type Typed -> Type Typed
closeOver a = forall fv a where
  fv = S.toList . ftv $ a
  forall :: [Var p] -> Type p -> Type p
  forall [] a = a
  forall vs a = TyForall vs a (annotation a)

consFst :: Functor m => a -> m ([a], b) -> m ([a], b)
consFst a = fmap (first (a:))
