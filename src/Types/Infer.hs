{-# LANGUAGE FlexibleContexts, TupleSections, GADTs #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, ViewPatterns #-}
module Types.Infer
  ( inferProgram
  , builtinsEnv
  , closeOver
  , tyString
  , tyInt
  , tyBool
  , tyUnit
  , tyFloat

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
import Data.Char

import Control.Monad.State
import Control.Monad.Infer
import Control.Arrow (first, second, (***))
import Control.Lens

import Syntax.Resolve.Toplevel
import Syntax.Implicits
import Syntax.Transform
import Syntax.Subst
import Syntax.Types
import Syntax.Let
import Syntax.Var
import Syntax

import Types.Kinds
import Types.Infer.Constructor
import Types.Infer.Pattern
import Types.Infer.Builtin
import Types.Wellformed
import Types.Unify

import Text.Pretty.Semantic

-- | Solve for the types of bindings in a problem: Either @TypeDecl@s,
-- @LetStmt@s, or @ForeignVal@s.
inferProgram :: MonadNamey m => Env -> [Toplevel Resolved] -> m (Either [TypeError] ([Toplevel Typed], Env))
inferProgram env ct = fmap fst <$> runInfer env (inferProg ct)

-- | Check an 'Expr'ession against a known 'Type', annotating it with
-- appropriate 'Wrapper's, and performing /some/ level of desugaring.
check :: forall m. MonadInfer Typed m => Expr Resolved -> Type Typed -> m (Expr Typed)
check e ty@TyPi{} | isSkolemisable ty = do -- This is rule Decl∀L from [Complete and Easy]
  (wrap, e) <- secondA (check e) =<< skolemise (ByAscription ty) ty -- gotta be polymorphic - don't allow instantiation
  pure (ExprWrapper wrap e (annotation e, ty))

check (Hole v a) t = do
  tell (Seq.singleton (ConFail (a, t) (TvName v) t))
  pure (Hole (TvName v) (a, t))

check (Begin [] _) _ = error "impossible: check empty Begin"
check (Begin xs a) t = do
  let start = init xs
      end = last xs
  start <- traverse (fmap fst . infer) start
  end <- check end t
  pure (Begin (start ++ [end]) (a, t))

check (Let ns b an) t = do
  bound <- view letBound
  cons <- view constructors
  types <- view names
  let genStrat ex =
        if Set.foldr ((&&) . generalisable) True (freeIn ex)
           then generalise (BecauseOf ex)
           else annotateKind (BecauseOf ex)
      generalisable v = (Set.member v bound || Set.member v cons) && maybe True Set.null (types ^. at (unTvName v) . to (fmap ftv))

  (ns, ts, is) <- inferLetTy genStrat ns
  let bvs = Set.fromList (map TvName (namesInScope (focus ts mempty)))
  local (letBound %~ Set.union bvs) $ local (names %~ focus ts) $ local (implicits %~ mappend is) $ do
    b <- check b t
    pure (Let ns b (an, t))

check ex@(Fun pat e an) ty = do
  (dom, cod, _) <- quantifier ex Don'tSkip ty
  let domain = _tyBinderType dom

  (p, tau, vs, cs) <- inferParameter pat
  _ <- unify ex domain (_tyBinderType tau)
  let tvs = boundTvs (p ^. paramPat) vs

  implies (Arm (pat ^. paramPat) e) domain cs $
    case dom of
      Implicit{} -> do
        (name, body, pat) <- makeImplicitName (annotation e) domain p
        e <- local (typeVars %~ Set.union tvs) $ local (names %~ focus vs) $
          local (implicits %~ insert name domain) $
            check e cod
        pure (Fun pat (body cod e) (an, ty))
      Anon{} -> do
        e <- local (typeVars %~ Set.union tvs) . local (names %~ focus vs) $
          check e cod
        pure (Fun p e (an, ty))
      _ -> error "invalid quantifier in check Fun"

check (If c t e an) ty = If <$> check c tyBool <*> check t ty <*> check e ty <*> pure (an, ty)

check (Match t ps a) ty = do
  (t, tt) <- infer t

  ps <- for ps $ \(p, e) -> do
    (p', ms, cs) <- checkPattern p tt
    let tvs = boundTvs p' ms

    bd <- implies (Arm p e) tt cs $
      local (typeVars %~ Set.union tvs) $
        local (names %~ focus ms) (check e ty)
    pure (p', bd)
  case ps of
    ((p, _):_) -> do
      sc <- case p of
        PWrapper (w, t') _ _ -> pure $ ExprWrapper w t (annotation t, t')
        _ -> pure t
      pure (Match sc ps (a, ty))
    _ -> pure (Match t ps (a, ty))

check (Access rc key a) ty = do
  rho <- freshTV
  Access <$> check rc (TyRows rho [(key, ty)]) <*> pure key <*> pure (a, ty)

-- This is _very_ annoying, but we need it for nested ascriptions
check ex@(Ascription e ty an) goal = do
  ty <- resolveKind (BecauseOf ex) ty
  e <- check e ty
  (_, c) <- subsumes ex ty goal
  pure (ExprWrapper c e (an, goal))

check e ty = do
  (e', t) <- infer e
  (_, c) <- subsumes e t ty
  pure (ExprWrapper c e' (annotation e, ty))

-- [Complete and Easy]: See https://www.cl.cam.ac.uk/~nk480/bidir.pdf

-- | Infer a 'Type' for an 'Expr'ession provided no other information
-- than the environment, producing an 'Expr'ession annotated with
-- 'Wrapper's where nescessary.
infer :: MonadInfer Typed m => Expr Resolved -> m (Expr Typed, Type Typed)
infer (VarRef k a) = do
  (cont, old, (new, cont')) <- third3A (discharge (VarRef k a)) =<< lookupTy' k
  case cont of
    Nothing -> pure (VarRef (TvName k) (a, old), old)
    Just cont -> pure (cont' (cont (VarRef (TvName k) (a, old))), new)

infer (Fun p e an) = let blame = Arm (p ^. paramPat) e in do
  (p, dom, ms, cs) <- inferParameter p
  let tvs = boundTvs (p ^. paramPat) ms
      domain = _tyBinderType dom

  _ <- leakEqualities blame cs
  case p of
    ImplParam{} -> do
      (name, body, pat) <- makeImplicitName an domain p
      (e, cod) <- local (typeVars %~ Set.union tvs) $
        local (names %~ focus ms) $
          local (implicits %~ consider name domain) $
            infer e
      pure (Fun pat (body cod e) (an, TyPi dom cod), TyPi dom cod)
    PatParam _ -> do
      (e, cod) <- local (typeVars %~ Set.union tvs) $
        local (names %~ focus ms) $
          infer e
      pure (Fun p e (an, TyPi dom cod), TyPi dom cod)

infer (Literal l an) = pure (Literal l (an, ty), ty) where
  ty = litTy l

infer ex@(Ascription e ty an) = do
  ty <- resolveKind (BecauseOf ex) ty
  e <- check e ty
  pure (Ascription (correct ty e) ty (an, ty), ty)

-- f ? - just delegate to the other checker
infer ex@(App f hole@(InstHole ha) a) = do
  ftv <- genName
  infer (App f (InstType (TyVar ftv) ha) a) `catchError`
    \err -> case err of
      WrongQuantifier _ ot -> throwError (ArisingFrom (WrongQuantifier hole ot) (BecauseOf ex))
      _ -> throwError err

infer ex@(App f (InstType t ta) a) = do
  (f, ot) <- infer f
  (dom, c, k) <- quantifier ex DoSkip ot
  case dom of
    Explicit v ki -> do
      x <- checkAgainstKind (BecauseOf ex) t ki
      let sub = Map.singleton v x
       in pure (App (k f) (InstType x (ta, ki)) (a, apply sub c), apply sub c)
    Anon{} ->
      throwError . flip WrongQuantifier ot . flip InstType (ta, undefined) =<< resolveKind (BecauseOf ex) t
    Implicit{} -> error "invalid implicit quantification in App"
    Invisible{} -> error "invalid invisible quantification in App"

infer ex@(App f x a) = do
  (f, ot) <- infer f
  (dom, c, k) <- quantifier ex DoSkip ot
  case dom of
    Anon d -> do
      x <- check x d
      pure (App (k f) x (a, c), c)
    Explicit{} -> throwError (ArisingFrom (WrongQuantifier x ot) (BecauseOf ex))
    Implicit{} -> error "invalid implicit quantification in App"
    Invisible{} -> error "invalid invisible quantification in App"

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
    let tvs = boundTvs p' ms
    leakEqualities ex cs
    e' <- local (typeVars %~ Set.union tvs) $
      local (names %~ focus ms) $
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

infer h@InstHole{} = throwError (propagateBlame (BecauseOf h) (NakedInstArtifact h))
infer h@InstType{} = throwError (propagateBlame (BecauseOf h) (NakedInstArtifact h))

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
  (ns', ts, is) <- inferLetTy (const (closeOver (BecauseOf stmt))) ns
                   `catchError` (throwError . propagateBlame (BecauseOf stmt))
  local (names %~ focus ts) . local (implicits %~ mappend is) $
    consFst (LetStmt ns') $
      inferProg prg
inferProg (st@(ForeignVal v d t ann):prg) = do
  t' <- resolveKind (BecauseOf st) t
  local (names %~ focus (one v t')) . local (letBound %~ Set.insert (TvName v)) $
    consFst (ForeignVal (TvName v) d t' (ann, t')) $
      inferProg prg
inferProg (decl@(TypeDecl n tvs cs):prg) = do
  (kind, retTy, tvs) <- resolveTyDeclKind (BecauseOf decl) n tvs cs
  local (names %~ focus (one n (rename kind))) $ do
     (ts, cs') <- unzip <$> for cs (\con ->
       inferCon retTy con `catchError` (throwError . propagateBlame (BecauseOf con)))
     local (names %~ focus (teleFromList ts)) . local (constructors %~ Set.union (Set.fromList (map fst ts))) $
       consFst (TypeDecl (TvName n) tvs cs') $
         inferProg prg
inferProg (Open mod pre:prg) = do
  modImplicits <- view (modules . at (TvName mod) . non undefined)
  local (implicits %~ (<>modImplicits)) $
    consFst (Open (TvName mod) pre) $ inferProg prg
inferProg (Module name body:prg) = do
  (body', env) <- inferProg body

  let (vars, tys) = extractToplevels body
      vars' = map (\x -> (TvName x, env ^. names . at x . non (error ("value: " ++ show x)))) (vars ++ tys)

  -- Extend the current scope and module scope
  local ( (names %~ focus (teleFromList vars'))
        . (modules %~ (Map.insert (TvName name) (env ^. implicits) . (<> (env ^. modules))))) $
    consFst (Module (TvName name) body') $
    inferProg prg

inferProg [] = asks ([],)

-- For polymorphic recursion, mostly
approxType :: MonadInfer Typed m => Expr Resolved -> StateT Origin m (Type Typed)
approxType r@(Fun p e _) = TyPi <$> approxParam p <*> approxType e where
  approxParam (ImplParam (PType _ t _)) = Implicit <$> resolveKind (BecauseOf r) t
  approxParam (PatParam (PType _ t _)) = Anon <$> resolveKind (BecauseOf r) t
  approxParam (ImplParam _) = do
    put Guessed
    Implicit <$> freshTV
  approxParam (PatParam _) = do
    put Guessed
    Anon <$> freshTV
approxType r@(Ascription _ t _) = resolveKind (BecauseOf r) t
approxType _ = do
  put Guessed
  lift freshTV

inferLetTy :: forall m. MonadInfer Typed m
           => (Expr Typed -> Type Typed -> m (Type Typed))
           -> [Binding Resolved]
           -> m ( [Binding Typed]
                , Telescope Typed
                , ImplicitScope Typed
                )
inferLetTy closeOver vs =
  let sccs = depOrder vs

      wasGuessed :: Origin -> Bool
      wasGuessed Guessed = True
      wasGuessed _ = False

      blameSkol :: TypeError -> (Var Resolved, SomeReason) -> TypeError
      blameSkol e (v, r) = propagateBlame r (Note e (string "in the inferred type for" <+> pretty v))

      figureOut :: (Var Resolved, SomeReason) -> Expr Typed -> Type Typed -> Seq.Seq (Constraint Typed) -> m (Type Typed, Expr Typed -> Expr Typed)
      figureOut blame ex ty cs = do
        cur <- genName
        (x, co, vt) <- case solve cur cs of
          Right (x, co) -> do
            ty' <- closeOver ex (apply x ty)
            pure (x, co, ty')
          Left e -> throwError (propagateBlame (snd blame) e)
        skolCheck (TvName (fst blame)) (snd blame) vt
        pure (vt, solveEx vt x co)


      approximate :: Binding Resolved
                  -> m (Origin, (Var Typed, Type Typed))
      approximate (Binding v e _ _) = do
        (ty, st) <- runStateT (approxType e) Supplied
        ty' <- generalise (BecauseOf e) ty
        pure (st, (TvName v, if not (wasGuessed st) then ty' else ty))

      skolCheck :: Var Typed -> SomeReason -> Type Typed -> m ()
      skolCheck var exp ty = do
        env <- view typeVars
        let sks = Set.map (^. skolIdent) (skols ty)
        unless (null (sks `Set.difference` env)) $
          throwError (blameSkol (EscapedSkolems (Set.toList (skols ty)) ty) (unTvName var, exp))

      tcOne :: SCC (Binding Resolved)
            -> m ( [Binding Typed]
                 , Telescope Typed )

      tcOne (AcyclicSCC decl@(Binding var exp p ann)) = do
        (origin, tv) <- approximate decl
        ((exp', ty), cs) <- listen . local (names %~ focus (uncurry one tv)) $
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
        (tp, k) <- figureOut (var, BecauseOf exp) exp' ty cs
        pure ( [Binding (TvName var) (k exp') p (ann, tp)], one var (rename tp) )

      tcOne (CyclicSCC vars) = do
        (origins, tvs) <- unzip <$> traverse approximate vars

        (vs, cs) <- listen . local (names %~ focus (teleFromList tvs)) $
          ifor (zip tvs vars) $ \i ((_, tyvar), Binding var exp p ann) ->
            case origins !! i of
              Supplied -> do
                let exp' (Ascription e _ _) = exp' e
                    exp' e = e
                exp <- check (exp' exp) tyvar
                pure (Binding (TvName var) exp p (ann, tyvar), tyvar)
              Guessed -> do
                (exp', ty) <- infer exp
                _ <- unify exp ty tyvar
                pure (Binding (TvName var) exp' p (ann, ty), ty)

        cur <- genName
        (solution, cs) <- case solve cur cs of
          Right x -> pure x
          Left e -> throwError e
        let solveOne :: (Binding Typed, Type Typed)
                     -> m (Binding Typed, Telescope Typed)
            solveOne (Binding var exp p ann, given) =
              let figure = apply solution
               in do
                  ty <- closeOver exp (figure given)
                  skolCheck var (BecauseOf exp) ty
                  pure ( Binding var (solveEx ty solution cs exp) p (fst ann, ty)
                       , one var (rename ty) )
            squish = fmap (second mconcat . unzip)
         in squish . traverse solveOne $ vs

      tc :: [SCC (Binding Resolved)]
         -> m ( [Binding Typed] , Telescope Typed )
      tc (s:cs) = do
        (vs', binds) <- tcOne s
        fmap ((vs' ++) *** (binds <>)) . local (names %~ focus binds) $ tc cs
      tc [] = pure ([], mempty)

      mkImplicits :: ([Binding Typed], Telescope Typed) -> m ([Binding Typed], Telescope Typed, ImplicitScope Typed)
      mkImplicits (bs, t) = do
        let one :: Binding Typed -> m (ImplicitScope Typed)
            one b@(Binding v _ BindImplicit (_, t)) = do
              checkAmbiguous v t
                `catchError` \e -> throwError (propagateBlame (BecauseOf b) e)
              pure (singleton v t)
            one _ = pure mempty
        (bs, t,) <$> foldMapM one bs
   in mkImplicits =<< tc sccs

solveEx :: Type Typed -> Subst Typed -> Map.Map (Var Typed) (Wrapper Typed) -> Expr Typed -> Expr Typed
solveEx _ ss cs = transformExprTyped go id goType where
  go :: Expr Typed -> Expr Typed
  go (ExprWrapper w e a) = case goWrap w of
    WrapFn w -> runWrapper w e
    x -> ExprWrapper x e a
  go x = x

  goWrap (TypeApp t) = TypeApp (goType t)
  goWrap (TypeAsc t) = TypeAsc (goType t)
  goWrap (ExprApp t) = ExprApp (solveEx undefined ss cs t)
  goWrap (Cast c) = case c of
    ReflCo{} -> IdWrap
    AssumedCo a b | a == b -> IdWrap
    _ -> Cast c
  goWrap (TypeLam l t) = TypeLam l (goType t)
  goWrap (x Syntax.:> y) = goWrap x Syntax.:> goWrap y
  goWrap (WrapVar v) = goWrap $ Map.findWithDefault err v cs where
    err = error $ "Unsolved wrapper variable " ++ show v ++ ". This is a bug"
  goWrap IdWrap = IdWrap
  goWrap (WrapFn f) = WrapFn . flip MkWrapCont (desc f) $ solveEx undefined ss cs . runWrapper f

  goType = apply ss

consFst :: Functor m => a -> m ([a], b) -> m ([a], b)
consFst = fmap . first . (:)

makeImplicitName :: MonadInfer Typed m
                 => Ann Resolved -> Type Typed -> Parameter Typed
                 -> m (Var Typed, Type Typed -> Expr Typed -> Expr Typed, Parameter Typed)
makeImplicitName _ _ pa@(ImplParam (viewOp -> Capture name _)) =
  pure (name, const id, pa)
makeImplicitName _ _ pa@(PatParam (viewOp -> Capture name _)) =
  pure (name, const id, pa)
makeImplicitName an domain (ImplParam p) = do
  name <- TvName <$> genName
  let body co e = Match (VarRef name (an, domain)) [ (p, e) ] (an, co)
      pat = ImplParam (Capture name (an, domain))
  pure (name, body, pat)
makeImplicitName an domain (PatParam p) = do
  name <- TvName <$> genName
  let body co e = Match (VarRef name (an, domain)) [ (p, e) ] (an, co)
      pat = ImplParam (Capture name (an, domain))
  pure (name, body, pat)

viewOp :: Pattern p -> Pattern p
viewOp (PType p _ _) = viewOp p
viewOp v = v

foldMapM :: (Foldable t, Monoid m, Monad f) => (a -> f m) -> t a -> f m
foldMapM k = foldM ((.k) . fmap . mappend) mempty

rename :: Type Typed -> Type Typed
rename = go 0 mempty mempty where
  go :: Int -> Set.Set T.Text -> Subst Typed -> Type Typed -> Type Typed
  go n l s (TyPi (Invisible v k) t) =
    let (v', n', l') = new n l v
     in TyPi (Invisible v' (fmap (apply s) k)) (go n' l' (Map.insert v (TyVar v') s) t)
  go n l s (TyPi (Anon k) t) = TyPi (Anon (go n l s k)) (go n l s t)
  go _ _ s tau = apply s tau

  new v l var@(TvName (TgName vr n))
    | toIdx vr == n || vr `Set.member` l =
      let name = genAlnum v
       in if Set.member name l
             then new (v + 1) l var
             else (TvName (TgName name n), v + 1, Set.insert name l)
    | otherwise = (TvName (TgName vr n), v, Set.insert vr l)

  new _ _ (TvName TgInternal{}) = error "TgInternal in rename"

  toIdx t = go (T.unpack t) (T.length t - 1) - 1 where
    go (c:cs) l = (ord c - 96) * (26 ^ l) + go cs (l - 1)
    go [] _ = 0

generalise :: MonadInfer Typed m => SomeReason -> Type Typed -> m (Type Typed)
generalise r ty =
  let fv = ftv ty in do
    env <- view typeVars
    case Set.toList (fv `Set.difference` env) of
      [] -> pure ty
      vs -> annotateKind r $ foldr (flip TyForall Nothing) ty vs
