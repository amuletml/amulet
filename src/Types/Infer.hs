{-# LANGUAGE FlexibleContexts, TupleSections, GADTs #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, RecordWildCards #-}
module Types.Infer
  ( inferProgram
  , builtinsEnv
  , closeOver
  , tyString, tyInt, tyBool, tyUnit, tyFloat

  , infer, check
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Set as Set
import Data.Traversable
import Data.Spanned
import Data.Triple
import Data.Graph
import Data.Span

import Control.Monad.State
import Control.Monad.Infer
import Control.Arrow (first, second, (***))
import Control.Lens

import Syntax.Resolve.Toplevel
import Syntax.Transform
import Syntax.Subst
import Syntax.Types
import Syntax.Let
import Syntax

import Types.Kinds
import Types.Infer.Constructor
import Types.Infer.Pattern
import Types.Infer.Builtin
import Types.Wellformed
import Types.Unify

import Pretty

-- Solve for the types of lets in a program
inferProgram :: MonadGen Int m => Env -> [Toplevel Resolved] -> m (Either TypeError ([Toplevel Typed], Env))
inferProgram env ct = fmap fst <$> runInfer env (inferProg ct)

check :: forall m. MonadInfer Typed m => Expr Resolved -> Type Typed -> m (Expr Typed)
check e ty@TyForall{} = do -- This is rule Decl∀L from [Complete and Easy]
  (wrap, e) <- secondA (check e) =<< skolemise (ByAscription ty) ty -- gotta be polymorphic - don't allow instantiation
  pure (ExprWrapper wrap e (annotation e, ty))

check (Hole v a) t = do
  tell (Seq.singleton (ConFail (TvName v) t))
  pure (Hole (TvName v) (a, t))

check (Begin [] _) _ = error "impossible: check empty Begin"
check (Begin xs a) t = do
  let start = init xs
      end = last xs
  start <- traverse (fmap fst . infer) start
  end <- check end t
  pure (Begin (start ++ [end]) (a, t))

check ex@(Let ns b an) t = do
  (ns, ts) <- inferLetTy (annotateKind (BecauseOf ex)) ns
  local (values %~ focus ts) $ do
    b <- check b t
    pure (Let ns b (an, t))

check ex@(Fun pat e an) ty = do
  (dom, cod, _) <- quantifier ex ty
  let domain = _tyBinderType dom

  (p, vs, cs) <- checkPattern pat domain
  let tvs = Set.map unTvName (boundTvs p vs)

  implies (Arm pat e) domain cs $ do
    e <- local (typeVars %~ Set.union tvs) . local (values %~ focus vs) $
      check e cod
    pure (Fun p e (an, ty))

check (If c t e an) ty = If <$> check c tyBool <*> check t ty <*> check e ty <*> pure (an, ty)

check (Match t ps a) ty = do
  (t, tt) <- infer t

  ps <- for ps $ \(p, e) -> do
    (p', ms, cs) <- checkPattern p tt
    let tvs = Set.map unTvName (boundTvs p' ms)

    bd <- implies (Arm p e) tt cs $
      local (typeVars %~ Set.union tvs) $
        local (values %~ focus ms) (check e ty)
    pure (p', bd)
  pure (Match t ps (a, ty))

check (Access rc key a) ty = do
  rho <- freshTV
  Access <$> check rc (TyRows rho [(key, ty)]) <*> pure key <*> pure (a, ty)

-- This is _very_ annoying, but we need it for nested ascriptions
check ex@(Ascription e ty an) goal = do
  ty <- resolveKind (BecauseOf ex) ty
  e <- check e ty
  (_, c) <- subsumes ex ty goal
  pure (Ascription (ExprWrapper c e (an, ty)) ty (an, goal))

check e ty = do
  (e', t) <- infer e
  (_, c) <- subsumes e ty t
  pure (ExprWrapper c e' (annotation e, ty))

-- [Complete and Easy]: See https://www.cl.cam.ac.uk/~nk480/bidir.pdf

infer :: MonadInfer Typed m => Expr Resolved -> m (Expr Typed, Type Typed)
infer (VarRef k a) = do
  (cont, old, new) <- third3A (discharge (VarRef k a)) =<< lookupTy' k
  case cont of
    Nothing -> pure (VarRef (TvName k) (a, old), old)
    Just cont -> pure (cont (VarRef (TvName k) (a, old)), new)

infer (Fun p e an) = let blame = Arm p e in do
  (p, dom, ms, cs) <- inferPattern p
  let tvs = boundTvs p ms
  
  _ <- leakEqualities blame cs
  (e, cod) <- local (typeVars %~ Set.union (Set.map unTvName tvs)) $
    local (values %~ focus ms) $ infer e
  pure (Fun p e (an, TyArr dom cod), TyArr dom cod)

infer (Literal l an) = pure (Literal l (an, ty), ty) where
  ty = litTy l

infer ex@(Ascription e ty an) = do
  ty <- resolveKind (BecauseOf ex) ty
  e <- check e ty
  pure (Ascription (correct ty e) ty (an, ty), ty)

infer ex@(App f x a) = do
  (f, (dom, c, k)) <- secondA (quantifier ex) =<< infer f
  case dom of
    Anon d -> do
      x <- check x d
      pure (App (k f) x (a, c), c)
    Implicit{} -> error "invalid implicit quantification in App"

infer ex@(BinOp l o r a) = do
  (o, (ld, c, k1)) <- secondA (decompose ex _TyArr) =<< infer o
  (rd, c', k2) <- decompose ex _TyArr c
  (l, r) <- (,) <$> check l ld <*> check r rd
  pure (App (k2 (App (k1 o) l (a, c))) r (a, c'), c')

infer ex@(Match t ps a) = do
  (t', tt) <- infer t
  ty <- freshTV
  ps' <- for ps $ \(p, e) -> do
    (p', ms, cs) <- checkPattern p tt
    let tvs = Set.map unTvName (boundTvs p' ms)
    leakEqualities ex cs
    e' <- local (typeVars %~ Set.union tvs) $
      local (values %~ focus ms) $
        check e ty
    pure (p', e')
  pure (Match t' ps' (a, ty), ty)

infer (Record rows a) = do
  (rows, rowts) <- unzip <$> inferRows rows
  let ty = TyExactRows rowts
   in pure (Record rows (a, ty), ty)

infer (RecordExt rec rows a) = do
  (rec, rho) <- infer rec
  (rows, newts) <- unzip <$> inferRows rows
  let ty = TyRows rho newts
   in pure (RecordExt rec rows (a, ty), ty)

infer (Tuple xs an) =
  let go [x] = first (:[]) <$> infer x
      go (x:xs) = do
        (x', t) <- infer x
        (xs, t') <- go xs
        pure (x':xs, TyTuple t t')
      go [] = error "wot in tarnation"
   in do
     (ex, t) <- go xs
     pure (Tuple ex (an, t), t)

infer ex = do
  x <- freshTV
  ex' <- check ex x
  pure (ex', x)

inferRows :: MonadInfer Typed m
          => [(T.Text, Expr Resolved)]
          -> m [((T.Text, Expr Typed), (T.Text, Type Typed))]
inferRows rows = for rows $ \(var', val) -> do
  (val', typ) <- infer val
  pure ((var', val'), (var', typ))

inferProg :: MonadInfer Typed m
          => [Toplevel Resolved] -> m ([Toplevel Typed], Env)
inferProg (stmt@(LetStmt ns):prg) = do
  (ns', ts) <- inferLetTy (closeOver (BecauseOf stmt)) ns
                   `catchError` (throwError . flip ArisingFrom (BecauseOf stmt))
  local (values %~ focus ts) $
    consFst (LetStmt ns') $
      inferProg prg
inferProg (st@(ForeignVal v d t ann):prg) = do
  t' <- resolveKind (BecauseOf st) t
  local (values %~ focus (one v t')) $
    consFst (ForeignVal (TvName v) d t' (ann, t')) $
      inferProg prg
inferProg (decl@(TypeDecl n tvs cs):prg) = do
  kind <- resolveTyDeclKind (BecauseOf decl) n tvs cs
  let retTy = foldl TyApp (TyCon (TvName n)) (map (TyVar . TvName) tvs)
   in extendKind (TvName n, kind) $ do
     (ts, cs') <- unzip <$> for cs (\con ->
       inferCon retTy con `catchError` \x -> throwError (ArisingFrom x (BecauseOf con)))
     local (values %~ focus (teleFromList ts)) . local (constructors %~ Set.union (Set.fromList (map (unTvName . fst) ts))) $
       consFst (TypeDecl (TvName n) (map TvName tvs) cs') $
         inferProg prg
inferProg (Open mod pre:prg) =
  -- Currently open doesn't need to do anything as we'll be in scope anyway
  consFst (Open (TvName mod) pre) $ inferProg prg
inferProg (Module name body:prg) = do
  (body', env) <- inferProg body

  let (vars, tys) = extractToplevels body
      vars' = map (\x -> (TvName x, env ^. values . at x . non (error ("value: " ++ show x)))) vars
      tys' = map (\x -> (TvName x, env ^. types . at x . non (error ("type: " ++ show x)))) tys

  local (values %~ focus (teleFromList vars')) . extendManyK tys' $
    consFst (Module (TvName name) body') $
    inferProg prg

inferProg [] = asks ([],)

-- For polymorphic recursion, mostly
approxType :: MonadInfer Typed m => Expr Resolved -> StateT Origin m (Type Typed)
approxType (Fun r@(PType _ t _) e _) = TyArr <$> resolveKind (BecauseOf r) t <*> approxType e
approxType r@(Ascription _ t _) = resolveKind (BecauseOf r) t
approxType (Fun _ e _) = do
  put Guessed
  TyArr <$> freshTV <*> approxType e
approxType _ = do
  put Guessed
  lift freshTV

inferLetTy :: forall m. MonadInfer Typed m
           => (Type Typed -> m (Type Typed))
           -> [(Var Resolved, Expr Resolved, Ann Resolved)]
           -> m ( [(Var Typed, Expr Typed, Ann Typed)]
                , Telescope Typed
                )
inferLetTy closeOver vs =
  let sccs = depOrder vs

      wasGuessed :: Origin -> Bool
      wasGuessed Guessed = True
      wasGuessed _ = False

      blameSkol :: TypeError -> (Var Resolved, SomeReason) -> TypeError
      blameSkol e (v, r) = ArisingFrom (Note e (string "in the inferred type for" <+> pretty v)) r

      figureOut :: (Var Resolved, SomeReason) -> Type Typed -> Seq.Seq (Constraint Typed) -> m (Type Typed, Expr Typed -> Expr Typed)
      figureOut blame ty cs = do
        cur <- gen
        (x, co, vt) <- case solve cur cs of
          Right (x, co) -> do
            ty' <- closeOver (apply x ty)
            pure (x, co, ty')
          Left e -> throwError (ArisingFrom e (snd blame))
        skolCheck (TvName (fst blame)) (snd blame) vt
        pure (vt, solveEx vt x co)

      generalise :: SomeReason -> Type Typed -> m (Type Typed)
      generalise r ty =
        let fv = ftv ty
         in do
           env <- Set.map TvName <$> view typeVars
           case Set.toList (fv `Set.difference` env) of
             [] -> pure ty
             vs -> annotateKind r $ foldr (flip TyForall Nothing) ty vs

      approximate :: (Var Resolved, Expr Resolved, Span)
                  -> m (Origin, (Var Typed, Type Typed))
      approximate (v, e, _) = do
        (ty, st) <- runStateT (approxType e) Supplied
        ty' <- generalise (BecauseOf e) ty
        pure (st, (TvName v, if not (wasGuessed st) then ty' else ty))

      skolCheck :: Var Typed -> SomeReason -> Type Typed -> m ()
      skolCheck var exp ty = do
        env <- view typeVars
        let sks = Set.map (^. skolIdent . to unTvName) (skols ty)
        unless (null (sks `Set.difference` env)) $
          throwError (blameSkol (EscapedSkolems (Set.toList (skols ty)) ty) (unTvName var, exp))

      tcOne :: SCC (Var Resolved, Expr Resolved, Ann Resolved)
            -> m ( [(Var Typed, Expr Typed, Ann Typed)]
                 , Telescope Typed )

      tcOne (AcyclicSCC decl@(var, exp, ann)) = do
        (origin, tv) <- approximate decl
        ((exp', ty), cs) <- listen . local (values %~ focus (uncurry one tv)) $
          case origin of
            Supplied -> do
              let exp' (Ascription e _ _) = exp' e
                  exp' e = e
              exp' <- check (exp' exp) (snd tv)
              pure (exp', snd tv)
            Guessed -> do
              (exp', ty) <- infer exp
              _ <- unify exp ty (snd tv)
              pure (exp', ty)
        (tp, k) <- figureOut (var, BecauseOf exp) ty cs
        pure ( [(TvName var, k exp', (ann, tp))], one var tp )

      tcOne (CyclicSCC vars) = do
        (origins, tvs) <- unzip <$> traverse approximate vars

        (vs, cs) <- listen . local (values %~ focus (teleFromList tvs)) $
          ifor (zip tvs vars) $ \i ((_, tyvar), (var, exp, ann)) ->
            case origins !! i of
              Supplied -> do
                let exp' (Ascription e _ _) = exp' e
                    exp' e = e
                exp <- check (exp' exp) tyvar
                pure (TvName var, exp, ann, tyvar)
              Guessed -> do
                (exp', ty) <- infer exp
                _ <- unify exp ty tyvar
                pure (TvName var, exp', ann, ty)

        cur <- gen
        (solution, cs) <- case solve cur cs of
          Right x -> pure x
          Left e -> throwError e
        let solveOne :: (Var Typed, Expr Typed, Span, Type Typed)
                     -> m ((Var Typed, Expr Typed, Ann Typed), Telescope Typed)
            solveOne (var, exp, ann, given) =
              let figure = apply solution
               in do
                  ty <- closeOver (figure given)
                  skolCheck var (BecauseOf exp) ty
                  pure ( (var, solveEx ty solution cs exp, (ann, ty))
                       , one var ty )
            squish = fmap (second mconcat . unzip)
         in squish . traverse solveOne $ vs

      tc :: [SCC (Var Resolved, Expr Resolved, Ann Resolved)]
         -> m ( [(Var Typed, Expr Typed, Ann Typed)] , Telescope Typed )
      tc (s:cs) = do
        (vs', binds) <- tcOne s
        fmap ((vs' ++) *** (binds <>)) . local (values %~ focus binds) $ tc cs
      tc [] = pure ([], mempty)
   in tc sccs

solveEx :: Type Typed -> Subst Typed -> Map.Map (Var Typed) (Wrapper Typed) -> Expr Typed -> Expr Typed
solveEx _ ss cs = transformExprTyped go id goType where
  go :: Expr Typed -> Expr Typed
  go (ExprWrapper w e a) = ExprWrapper (goWrap w) e a
  go x = x

  goWrap (TypeApp t) = TypeApp (goType t)
  goWrap (Cast c) = case c of
    ReflCo{} -> IdWrap
    AssumedCo a b | a == b -> IdWrap
    _ -> Cast c
  goWrap (TypeLam l t) = TypeLam l (goType t)
  goWrap (x Syntax.:> y) = goWrap x Syntax.:> goWrap y
  goWrap (WrapVar v) = goWrap $ Map.findWithDefault err v cs where
    err = error $ "Unsolved wrapper variable " ++ show v ++ ". This is a bug"
  goWrap IdWrap = IdWrap
  goWrap (WrapFn f) = WrapFn f

  goType = apply ss

consFst :: Functor m => a -> m ([a], b) -> m ([a], b)
consFst = fmap . first . (:)
