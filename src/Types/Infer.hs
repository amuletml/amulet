{-# LANGUAGE FlexibleContexts, TupleSections, ScopedTypeVariables, ViewPatterns #-}
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
import Data.Reason
import Data.Traversable
import Data.Spanned
import Data.Triple
import Data.Graph
import Data.Char

import Control.Monad.State
import Control.Monad.Infer
import Control.Arrow (first)
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
import Types.Infer.Outline
import Types.Wellformed
import Types.Unify

import Text.Pretty.Semantic
import Control.Exception (assert)

-- | Solve for the types of bindings in a problem: Either @TypeDecl@s,
-- @LetStmt@s, or @ForeignVal@s.
inferProgram :: MonadNamey m => Env -> [Toplevel Resolved] -> m (Either [TypeError] ([Toplevel Typed], Env))
inferProgram env ct = fmap fst <$> runInfer env (inferProg ct)

-- | Check an 'Expr'ession against a known 'Type', annotating it with
-- appropriate 'Wrapper's, and performing /some/ level of desugaring.
check :: forall m. MonadInfer Typed m => Expr Resolved -> Type Typed -> m (Expr Typed)
check e ty@TyPi{} | isSkolemisable ty = do -- This is rule Decl∀L from [Complete and Easy]
  (wrap, e) <- secondA (check e) =<< skolemise (ByAscription e ty) ty -- gotta be polymorphic - don't allow instantiation
  pure (ExprWrapper wrap e (annotation e, ty))

check (Hole v a) t = do
  tell (Seq.singleton (ConFail (a, t) (TvName v) t))
  pure (Hole (TvName v) (a, t))

check (Let ns b an) t = do
  (ns, ts, is, vars) <-
    inferLetTy localGenStrat ns
      `catchError` \e -> do
        tell (Seq.singleton (DeferredError e))
        fakeLetTys ns

  let bvs = Set.fromList (map TvName (namesInScope (focus ts mempty)))

  local (typeVars %~ Set.union vars) $
    local (letBound %~ Set.union bvs) $
      local (names %~ focus ts) $
        local (implicits %~ mappend is) $ do
          b <- check b t
          pure (Let ns b (an, t))

check ex@(Fun pat e an) ty = do
  (dom, cod, _) <- quantifier (becauseExp ex) Don'tSkip ty
  let domain = _tyBinderType dom

  (p, tau, vs, cs) <- inferParameter pat
  _ <- unify (becauseExp ex) domain (_tyBinderType tau)
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
  tt <-
    case ps of
      ((p, _):_) -> view _2 <$> inferPattern p
      _ -> view _2 <$> infer t
  t <- check t tt

  ps <- for ps $ \(p, e) -> do
    (p', ms, cs) <- checkPattern p tt
    let tvs = boundTvs p' ms

    bd <- implies (Arm p e) tt cs $
      local (typeVars %~ Set.union tvs) $
        local (names %~ focus ms) (check e ty)
    pure (p', bd)
  pure (Match t ps (a, ty))

check e@(Access rc key a) ty = do
  rho <- freshTV
  rc <- censor (rereason (becauseExp e)) $
    check rc (TyRows rho [(key, ty)])
  pure (Access rc key (a, ty))

-- This is _very_ annoying, but we need it for nested ascriptions
check ex@(Ascription e ty an) goal = do
  ty <- resolveKind (becauseExp ex) ty
  e <- check e ty
  -- here: have ty (given)
  --       want goal (given)
  c <- subsumes (becauseExp ex) ty goal
  pure (ExprWrapper c e (an, goal))

check e ty = do
  (e', t) <- infer e
  -- here: have t (inferred)
  --       want ty (given)
  c <- subsumes (becauseExp e) t ty
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

infer (Let ns b an) = do
  (ns, ts, is, vars) <- inferLetTy localGenStrat ns
    `catchError` \e -> do
       tell (Seq.singleton (DeferredError e))
       fakeLetTys ns

  let bvs = Set.fromList (map TvName (namesInScope (focus ts mempty)))

  local (typeVars %~ Set.union vars) $
    local (letBound %~ Set.union bvs) $
      local (names %~ focus ts) $
        local (implicits %~ mappend is) $ do
          (b, ty) <- infer b
          pure (Let ns b (an, ty), ty)

infer ex@(Ascription e ty an) = do
  ty <- resolveKind (becauseExp ex) ty
  e <- check e ty
  pure (Ascription (correct ty e) ty (an, ty), ty)

infer ex@(App f x a) = do
  (f, ot) <- infer f
  (dom, c, k) <- quantifier (becauseExp ex) DoSkip ot
  case dom of
    Anon d -> do
      x <- check x d
      pure (App (k f) x (a, c), c)
    Implicit{} -> error "invalid implicit quantification in App"
    Invisible{} -> error "invalid invisible quantification in App"

infer ex@(BinOp l o r a) = do
  (o, ty) <- infer o

  (Anon lt, c1, k1) <- quantifier (becauseExp ex) DoSkip ty
  (Anon rt, c2, k2) <- quantifier (becauseExp ex) DoSkip c1

  (l, r) <- (,) <$> check l lt <*> check r rt
  pure (App (k2 (App (k1 o) l (a, c1))) r (a, c2), c2)

infer ex@(Match t ps a) = do
  tt <-
    case ps of
      ((p, _):_) -> view _2 <$> inferPattern p
      _ -> view _2 <$> infer t
  t' <- check t tt
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

infer ex@(RecordExt rec rows a) = do
  (rec, rho) <- infer rec
  (rows, newts) <- unzip <$> inferRows rows
  tv <- freshTV
  let ty = TyRows tv newts

  -- here: have rho (inferred)
  --       want ty (inferred)
  co <- subsumes (becauseExp ex) rho ty
  pure (ExprWrapper co (RecordExt rec rows (a, ty)) (a, ty), ty)

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

infer (Begin xs a) = do
  let start = init xs
      end = last xs
  start <- traverse (fmap fst . infer) start
  (end, t) <- infer end
  pure (Begin (start ++ [end]) (a, t), t)

infer ex = do
  x <- freshTV
  ex' <- check ex x
  pure (ex', x)

inferRows :: MonadInfer Typed m
          => [Field Resolved]
          -> m [(Field Typed, (T.Text, Type Typed))]
inferRows rows = for rows $ \(Field n e s) -> do
  (e, t) <- infer e
  pure (Field n e (s, t), (n, t))

inferProg :: MonadInfer Typed m
          => [Toplevel Resolved] -> m ([Toplevel Typed], Env)
inferProg (stmt@(LetStmt ns):prg) = do
  (ns', ts, is, vs) <- inferLetTy (closeOverStrat (BecauseOf stmt)) ns
                         `catchError` (throwError . propagateBlame (BecauseOf stmt))
  let bvs = Set.fromList (map TvName (namesInScope (focus ts mempty)))

  (ts, es) <- flip foldTeleM ts $ \var ty -> do
    ty <- (Right <$> skolCheck (TvName var) (BecauseOf stmt) ty)
            `catchError` \e -> pure (Left e)
    case ty of
      Left e -> pure (mempty, [e])
      Right t -> pure (one var t, mempty)
  assert (vs == mempty) pure ()
  case es of
    [] -> pure ()
    xs -> throwError (ManyErrors xs)

  local (letBound %~ Set.union bvs) . local (names %~ focus ts) . local (implicits %~ mappend is) $
    consFst (LetStmt ns') $
      inferProg prg

inferProg (st@(ForeignVal v d t ann):prg) = do
  t' <- resolveKind (BecauseOf st) t
  local (names %~ focus (one v t')) . local (letBound %~ Set.insert (TvName v)) $
    consFst (ForeignVal (TvName v) d t' (ann, t')) $
      inferProg prg

inferProg (decl@(TypeDecl n tvs cs):prg) = do
  (kind, retTy, tvs) <- resolveTyDeclKind (BecauseOf decl) n tvs cs
                          `catchError` (throwError . propagateBlame (BecauseOf decl))

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

inferLetTy :: forall m. MonadInfer Typed m
           => (Set.Set (Var Typed) -> Expr Typed -> Type Typed -> m (Type Typed))
           -> [Binding Resolved]
           -> m ( [Binding Typed]
                , Telescope Typed
                , ImplicitScope Typed
                , Set.Set (Var Typed)
                )
inferLetTy closeOver vs =
  let sccs = depOrder vs

      figureOut :: (Var Resolved, SomeReason)
                -> Expr Typed -> Type Typed
                -> Seq.Seq (Constraint Typed)
                -> m (Type Typed, Expr Typed -> Expr Typed)
      figureOut blame ex ty cs = do
        cur <- genName
        (x, co, vt) <- case solve cur cs of
          Right (x, co) -> do
            ty' <- closeOver (Set.singleton (TvName (fst blame))) ex (apply x ty)
            pure (x, co, ty')
          Left e -> throwError (propagateBlame (snd blame) e)
        ty <- skolCheck (TvName (fst blame)) (snd blame) vt
        pure (ty, solveEx ty x co)


      tcOne :: SCC (Binding Resolved)
            -> m ( [Binding Typed]
                 , Telescope Typed
                 , Set.Set (Var Typed))

      tcOne (AcyclicSCC decl@(Binding var exp p ann)) = do
        (origin, tv@(_, ty)) <- approximate decl
        ((exp', ty), cs) <- listen $
          case origin of
            Supplied -> do
              let exp' (Ascription e _ _) = exp' e
                  exp' e = e
              exp <- check (exp' exp) ty
              pure (exp, ty)
            Guessed -> do
              (exp', ty) <- infer exp
              _ <- unify (becauseExp exp) ty (snd tv)
              pure (exp', ty)
        (tp, k) <- figureOut (var, becauseExp exp) exp' ty cs
        pure ( [Binding (TvName var) (k exp') p (ann, tp)], one var (rename tp), mempty )

      tcOne (AcyclicSCC TypedMatching{}) = error "TypedMatching being TC'd"
      tcOne (AcyclicSCC b@(Matching p e ann)) = do
        ((e, p, ty, tel), cs) <- listen $ do
          (e, ety) <- infer e
          (p, pty, tel, cs) <- inferPattern p
          leakEqualities b cs

          -- here: have expression type (inferred)
          --       want pattern type (inferred)
          wrap <- subsumes (BecauseOf b) ety pty
          pure (ExprWrapper wrap e (annotation e, pty), p, pty, tel)

        cur <- genName
        (solution, wraps) <- case solve cur cs of
          Left e -> throwError e 
          Right x -> pure x
        let solved = closeOver mempty ex . apply solution
            ex = solveEx ty solution wraps e

        tel' <- traverseTele (const solved) tel
        ty <- solved ty

        let pat = transformPatternTyped id (apply solution) p
        pure ( [TypedMatching pat ex (ann, ty) (teleToList tel')], tel', boundTvs pat tel' )

      tcOne (AcyclicSCC ParsedBinding{}) = error "ParsedBinding in TC (inferLetTy)"

      tcOne (CyclicSCC vars) = do
        guardOnlyBindings vars
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
                _ <- unify (becauseExp exp) ty tyvar
                pure (Binding (TvName var) exp' p (ann, ty), ty)

        cur <- genName
        (solution, cs) <- case solve cur cs of
          Right x -> pure x
          Left e -> throwError e
        let solveOne :: (Binding Typed, Type Typed)
                     -> m (Binding Typed, Telescope Typed, Set.Set (Var Typed))
            solveOne (Binding var exp p ann, given) =
              let figure :: Type Typed -> Type Typed
                  figure = apply solution
               in do
                  ty <- closeOver mempty exp (figure given)
                  ty <- skolCheck var (becauseExp exp) ty
                  pure ( Binding var (solveEx ty solution cs exp) p (fst ann, ty)
                       , one var (rename ty) 
                       , mempty )

            solveOne _ = error "solveOne non-Binding forbidden"

            squish :: m [(Binding Typed, Telescope Typed, Set.Set (Var Typed))]
                   -> m ([Binding Typed], Telescope Typed, Set.Set (Var Typed))
            squish = fmap ((\(b, t, v) -> (b, mconcat t, mconcat v)) . unzip3)
        squish . traverse solveOne $ vs

      tc :: [SCC (Binding Resolved)]
         -> m ( [Binding Typed] , Telescope Typed, Set.Set (Var Typed) )
      tc (s:cs) = do
        (vs', binds, vars) <- tcOne s
        fmap (\(bs, tel, vs) -> (vs' ++ bs, tel <> binds, vars <> vs)) . local (names %~ focus binds) . local (typeVars %~ Set.union vars) $ tc cs
      tc [] = pure ([], mempty, mempty)

      mkImplicits :: ([Binding Typed], Telescope Typed, Set.Set (Var Typed)) -> m ([Binding Typed], Telescope Typed, ImplicitScope Typed, Set.Set (Var Typed))
      mkImplicits (bs, t, vars) = do
        let one :: Binding Typed -> m (ImplicitScope Typed)
            one b@(Binding v _ BindImplicit (_, t)) = do
              checkAmbiguous v t
                `catchError` \e -> throwError (propagateBlame (BecauseOf b) e)
              pure (singleton v t)
            one _ = pure mempty
        (bs, t,, vars) <$> foldMapM one bs
   in mkImplicits =<< tc sccs

fakeLetTys :: MonadInfer Typed m => [Binding Resolved] -> m ([Binding Typed], Telescope Typed, ImplicitScope Typed, Set.Set (Var Typed))
fakeLetTys bs = do
  let go (b:bs) =
        case b of
          Binding _ e _ _ -> do
            (var, ty) <- snd <$> approximate b 
            ty <- localGenStrat mempty e ty
            (<>) (one var ty) <$> go bs
          Matching p _ _ -> do
            let bound' :: Pattern p -> [Var p]
                bound' = bound
            tel <- flip (flip foldM mempty) (bound' p) $ \rest var -> do
              ty <- freshTV
              pure (one var ty <> rest)
            (<>) tel <$> go bs
          _ -> error "impossible"
      go [] = pure mempty
  tele <- go bs
  pure (mempty, tele, mempty, mempty)

skolCheck :: MonadInfer Typed m => Var Typed -> SomeReason -> Type Typed -> m (Type Typed)
skolCheck var exp ty = do
  let blameSkol :: TypeError -> (Var Resolved, SomeReason) -> TypeError
      blameSkol e (v, r) = propagateBlame r (Note e (string "in the inferred type for" <+> pretty v))
      sks = Set.map (^. skolIdent) (skols ty)
  env <- view typeVars
  unless (null (sks `Set.difference` env)) $
    throwError (blameSkol (EscapedSkolems (Set.toList (skols ty)) ty) (unTvName var, exp))
  pure (deSkol ty)

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

  goType :: Type Typed -> Type Typed
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

localGenStrat :: forall p m. (Pretty (Var p), Respannable p, Ord (Var p), Degrade p, MonadInfer Typed m)
              => Set.Set (Var Typed) -> Expr p -> Type Typed -> m (Type Typed)
localGenStrat bg ex ty = do
  bound <- view letBound
  cons <- view constructors
  types <- view names
  let generalisable v =
        (Set.member v bound || Set.member v cons || Set.member v builtinNames)
       && maybe True Set.null (types ^. at (unTvName v) . to (fmap ftv))
      freeIn' :: Expr p -> Set.Set (Var Typed)
      freeIn' = Set.mapMonotonic (upgrade . degrade) . freeIn

  if Set.foldr ((&&) . generalisable) True (freeIn' ex Set.\\ bg) && value ex
     then generalise ftv (becauseExp ex) ty
     else annotateKind (becauseExp ex) ty

closeOverStrat :: MonadInfer Typed m
               => SomeReason
               -> Set.Set (Var Typed) -> Expr Typed -> Type Typed -> m (Type Typed)
closeOverStrat r _ _ = closeOver r

value :: Expr a -> Bool
value Fun{} = True
value Literal{} = True
value Function{} = True
value (Let _ e _) = value e
value (Parens e _) = value e
value Tuple{} = True
value (Begin es _) = value (last es)
value Syntax.Lazy{} = True
value TupleSection{} = True
value Record{} = True
value RecordExt{} = True
value VarRef{} = False
value If{} = False
value App{} = False
value Match{} = False
value BinOp{} = False
value Hole{} = False
value (Ascription e _ _) = value e
value Access{} = False
value LeftSection{} = True
value RightSection{} = True
value BothSection{} = True
value AccessSection{} = True
value (OpenIn _ e _) = value e
value (ExprWrapper _ e _) = value e

deSkol :: Type Typed -> Type Typed
deSkol = go mempty where
  go acc (TyPi x k) =
    case x of
      Invisible v kind -> TyPi (Invisible v kind) (go (Set.insert v acc) k)
      Implicit a -> TyPi (Implicit (go acc a)) (go acc k)
      Anon a -> TyPi (Anon (go acc a)) (go acc k)
  go acc ty@(TySkol (Skolem _ var _ _))
    | var `Set.member` acc = TyVar var
    | otherwise = ty
  go _ x@TyCon{} = x
  go _ x@TyVar{} = x
  go _ x@TyPromotedCon{} = x
  go acc (TyApp f x) = TyApp (go acc f) (go acc x)
  go acc (TyRows t rs) = TyRows (go acc t) (map (_2 %~ go acc) rs)
  go acc (TyExactRows rs) = TyExactRows (map (_2 %~ go acc) rs)
  go acc (TyTuple a b) = TyTuple (go acc a) (go acc b)
  go acc (TyWildcard x) = TyWildcard (go acc <$> x)
  go acc (TyWithConstraints cs x) = TyWithConstraints (map (\(a, b) -> (go acc a, go acc b)) cs) (go acc x)
  go _ TyType = TyType

guardOnlyBindings :: MonadError TypeError m => [Binding Resolved] -> m ()
guardOnlyBindings bs = go bs where
  go (Binding{}:xs) = go xs
  go (m@Matching{}:_) =
    throwError (PatternRecursive m bs)

  go (ParsedBinding{}:_) = error "ParsedBinding in guardOnlyBindings"
  go (TypedMatching{}:_) = error "TypedMatching in guardOnlyBindings"
  go [] = pure ()
