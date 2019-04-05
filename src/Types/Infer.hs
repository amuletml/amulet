{-# LANGUAGE FlexibleContexts, TupleSections, ScopedTypeVariables,
   ViewPatterns, LambdaCase, TypeFamilies, CPP #-}
module Types.Infer
  ( inferProgram
  , closeOver

  , infer, check, solveEx
  ) where

import Prelude

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Set as Set
import Data.Traversable
import Data.List (find)
import Data.Spanned
import Data.Reason
import Data.Triple
import Data.Maybe
import Data.These
import Data.Graph
import Data.Char
import Data.Span

import Control.Monad.State
import Control.Monad.Infer
import Control.Arrow (first)
import Control.Lens

import Syntax.Resolve.Toplevel
import Syntax.Implicits
import Syntax.Transform
import Syntax.Builtin
import Syntax.Subst
import Syntax.Types
import Syntax.Let
import Syntax.Var
import Syntax

import Types.Infer.Constructor
import Types.Infer.Pattern
import Types.Infer.Builtin
import Types.Infer.Outline
import Types.Infer.Class
import Types.Wellformed
import Types.Kinds
import Types.Unify

import Text.Pretty.Semantic
import Control.Exception (assert)

#ifdef TRACE_TC
import Debug.Trace
#endif

-- | Solve for the types of bindings in a problem: Either @TypeDecl@s,
-- @LetStmt@s, or @ForeignVal@s.
inferProgram :: MonadNamey m => Env -> [Toplevel Desugared] -> m (These [TypeError] ([Toplevel Typed], Env))
inferProgram env ct = fmap fst <$> runInfer env (inferProg ct)

-- | Check an 'Expr'ession against a known 'Type', annotating it with
-- appropriate 'Wrapper's, and performing /some/ level of desugaring.
check :: forall m. MonadInfer Typed m => Expr Desugared -> Type Typed -> m (Expr Typed)
check e oty@TyPi{} | isSkolemisable oty = do
  (wrap, ty, scope) <- skolemise (ByAscription (annotation e) oty) oty
  local (classes %~ mappend scope) $ do
    e <- check e ty
    pure (ExprWrapper wrap e (annotation e, oty))

check (Hole v a) t = do
  tell (Seq.singleton (ConFail (a, t) v t))
  pure (Hole v (a, t))

check (Let ns b an) t = do
  (ns, ts, vars) <-
    inferLetTy localGenStrat Propagate ns
      `catchChronicle` \e -> do
        tell (DeferredError <$> e)
        fakeLetTys ns

  let bvs = Set.fromList (namesInScope (focus ts mempty))

  local (typeVars %~ Set.union vars) $
    local (letBound %~ Set.union bvs) $
      local (names %~ focus ts) $ do
        b <- check b t
        pure (Let ns b (an, t))

check ex@(Fun pat e an) ty = do
  (dom, cod, _) <- quantifier (becauseExp ex) ty
  let domain = _tyBinderType dom

  (p, tau, vs, cs) <- inferParameter pat
  _ <- unify (becauseExp ex) domain (_tyBinderType tau)
  let tvs = boundTvs (p ^. paramPat) vs

  implies (Arm (pat ^. paramPat) Nothing e) domain cs $
    case dom of
      Anon{} -> do
        e <- local (typeVars %~ Set.union tvs) . local (names %~ focus vs) $
          check e cod
        pure (Fun p e (an, ty))
      _ -> error "invalid quantifier in check Fun"

check (If c t e an) ty = If <$> check c tyBool <*> check t ty <*> check e ty <*> pure (an, ty)

check (Match t ps a) ty = do
  tt <-
    case ps of
      (Arm p _ _:_) -> view _2 <$> inferPattern p
      _ -> view _2 <$> infer t
  t <- check t tt

  ps <- for ps $ \(Arm p g e) -> do
    (p', ms, cs) <- checkPattern p tt
    let tvs = boundTvs p' ms

    implies (Arm p g e) tt cs
      . local (typeVars %~ Set.union tvs)
      . local (names %~ focus ms) $ do
        g' <- traverse (`check` tyBool) g
        e' <- check e ty
        pure (Arm p' g' e')
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

check (Parens e _) ty = check e ty

check AccessSection{} tau =
  error $ "check AccessSection : " ++ displayS (pretty tau) ++ " in TC (desugar didn't run?)"
check RightSection{} tau =
  error $ "check RightSection : " ++ displayS (pretty tau) ++ " in TC (desugar didn't run?)"
check LeftSection{} tau =
  error $ "check LeftSection : " ++ displayS (pretty tau) ++ " in TC (desugar didn't run?)"
check BothSection{} tau =
  error $ "check BothSection : " ++ displayS (pretty tau) ++ " in TC (desugar didn't run?)"
check TupleSection{} tau =
  error $ "check TupleSection : " ++ displayS (pretty tau) ++ " in TC (desugar didn't run?)"
check Syntax.Lazy{} tau =
  error $ "check Syntax.Lazy : " ++ displayS (pretty tau) ++ " in TC (desugar didn't run?)"
check OpenIn{} tau =
  error $ "check OpenIn : " ++ displayS (pretty tau) ++ " in TC (desugar didn't run?)"

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
infer :: MonadInfer Typed m => Expr Desugared -> m (Expr Typed, Type Typed)
infer (VarRef k a) = do
  (cont, old, (new, cont')) <- third3A (discharge (VarRef k a :: Expr Desugared)) =<< lookupTy' Strong k
  case cont of
    Nothing -> pure (VarRef k (a, old), old)
    Just cont -> pure (cont' (cont (VarRef k (a, old))), new)

infer (Fun (view paramPat -> p) e an) = do
  (p, dom, ms, cs) <- inferPattern p
  let tvs = boundTvs p ms

  _ <- leakEqualities p cs

  (e, cod) <- local (typeVars %~ Set.union tvs) $
    local (names %~ focus ms) (infer e)

  pure (Fun (PatParam p) e (an, TyArr dom cod), TyArr dom cod)

infer (Literal l an) = pure (Literal l (an, ty), ty) where
  ty = litTy l

infer (ListExp es an) = do
  t <- freshTV
  es <- traverse (`check` t) es
  pure (buildList an t es, TyApp tyList t)

infer (Let ns b an) = do
  (ns, ts, vars) <- inferLetTy localGenStrat Propagate ns
    `catchChronicle` \e -> do
       tell (DeferredError <$> e)
       fakeLetTys ns

  let bvs = Set.fromList (namesInScope (focus ts mempty))

  local (typeVars %~ Set.union vars) $
    local (letBound %~ Set.union bvs) $
      local (names %~ focus ts) $ do
        (b, ty) <- infer' b
        pure (Let ns b (an, ty), ty)

infer ex@(Ascription e ty an) = do
  ty <- resolveKind (becauseExp ex) ty
  e <- check e ty
  pure (Ascription (correct ty e) ty (an, ty), ty)

infer ex@(BinOp l o r a) = do
  (o, ty) <- infer o

  ~(Anon lt, c1, k1) <- quantifier (becauseExp ex) ty
  ~(Anon rt, c2, k2) <- quantifier (becauseExp ex) c1

  (l, r) <- (,) <$> check l lt <*> check r rt
  pure (App (k2 (App (k1 o) l (a, c1))) r (a, c2), c2)

infer ex@App{} = do
  (ex, ty) <- inferApp ex
  (k, _, ty) <- instantiate Strong Expression ty
  pure (fromMaybe id k ex, ty)

infer ex@Vta{} = do
  (ex, ty) <- inferApp ex
  (k, _, ty) <- instantiate Strong Expression ty
  pure (fromMaybe id k ex, ty)

infer ex@(Match t ps a) = do
  tt <-
    case ps of
      (Arm p _ _:_) -> view _2 <$> inferPattern p
      _ -> view _2 <$> infer t

  t' <- check t tt
  ty <- freshTV

  ps' <- for ps $ \(Arm p g e) -> do
    (p', ms, cs) <- checkPattern p tt
    let tvs = boundTvs p' ms
    leakEqualities ex cs
    local (typeVars %~ Set.union tvs) . local (names %~ focus ms) $ do
      e' <- check e ty
      g' <- traverse (`check` tyBool) g
      pure (Arm p' g' e')
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
  (end, t) <- infer' end
  pure (Begin (start ++ [end]) (a, t), t)

infer ex = do
  x <- freshTV
  ex' <- check ex x
  pure (ex', x)

-- | Infer a 'Type' for an 'Expr'ession without instantiating variables
infer' :: MonadInfer Typed m => Expr Desugared -> m (Expr Typed, Type Typed)
infer' (VarRef k a) = do
  (cont, old, ty) <- lookupTy' Weak k
  pure (fromMaybe id cont (VarRef k (a, old)), ty)
infer' ex@App{} = inferApp ex
infer' ex@Vta{} = inferApp ex
infer' x = infer x

inferApp :: MonadInfer Typed m => Expr Desugared -> m (Expr Typed, Type Typed)
inferApp ex@(App f x a) = do
  (f, ot) <- infer' f
  (dom, c, k) <- quantifier (becauseExp ex) ot
  case dom of
    Anon d -> do
      x <- check x d
      pure (App (k f) x (a, c), c)
    Invisible _ _ Req -> do
      (_, t) <- infer x
      b <- freshTV
      confesses (NotEqual ot (TyArr t b))
    Invisible{} -> error "invalid invisible quantification in App"
    Implicit{} -> error "invalid invisible quantification in App"

inferApp ex@(Vta f x a) = do
  (f, ot) <- infer' f
  (dom, c) <- retcons (addBlame (becauseExp ex)) $
    firstForall x ot
  case dom of
    Invisible v kind r | r /= Infer{} -> do
      x <- case kind of
        Just k -> checkAgainstKind (becauseExp ex) x k
        Nothing -> resolveKind (becauseExp ex) x
      let ty = apply (Map.singleton v x) c
      pure (ExprWrapper (TypeApp x) f (a, ty), ty)
    Invisible{} -> error "inferred forall should always be eliminated"
    Implicit{} -> error "invalid implicit quantification in Vta"
    Anon{} -> error "invalid arrow type in Vta"

inferApp _ = error "not an application"

inferRows :: MonadInfer Typed m
          => [Field Desugared]
          -> m [(Field Typed, (T.Text, Type Typed))]
inferRows rows = for rows $ \(Field n e s) -> do
  (e, t) <- infer e
  pure (Field n e (s, t), (n, t))

inferProg :: MonadInfer Typed m
          => [Toplevel Desugared] -> m ([Toplevel Typed], Env)
inferProg (stmt@(LetStmt am ns):prg) = censor (const mempty) $ do
  (ns', ts, vs) <- retcons (addBlame (BecauseOf stmt)) (inferLetTy (closeOverStrat (BecauseOf stmt)) Fail ns)
  let bvs = Set.fromList (namesInScope (focus ts mempty))

  (ts, es) <- flip foldTeleM ts $ \var ty -> do
    ty <- memento $ skolCheck var (BecauseOf stmt) ty
    case ty of
      Left e -> pure (mempty, [e])
      Right t -> do
        assert (vs == mempty) pure ()
        pure (one var t, mempty)
  case es of
    [] -> pure ()
    xs -> confess (mconcat xs)

  local (letBound %~ Set.union bvs) . local (names %~ focus ts) $
    consFst (LetStmt am ns') $
      inferProg prg

inferProg (st@(ForeignVal am v d t ann):prg) = do
  t' <- resolveKind (BecauseOf st) t
  local (names %~ focus (one v t')) . local (letBound %~ Set.insert v) $
    consFst (ForeignVal am v d t' (ann, t')) $
      inferProg prg

inferProg (decl@(TypeDecl am n tvs cs):prg) = do
  (kind, retTy, tvs) <- retcons (addBlame (BecauseOf decl)) $
                          resolveTyDeclKind (BecauseOf decl) n tvs cs
  let scope (TyAnnArg v k:vs) = one v k <> scope vs
      scope (_:cs) = scope cs
      scope [] = mempty

  let vars =
        flip foldMap tvs $ \case
          TyVarArg v -> Set.singleton v
          TyAnnArg v _ -> Set.singleton v

  local (names %~ focus (one n (fst (rename kind)))) $ do
    (ts, cs') <- unzip <$> local (names %~ focus (scope tvs))
      (for cs (\con -> retcons (addBlame (BecauseOf con)) (inferCon vars retTy con)))
    let ts' = Set.fromList (map fst ts)

    local ( (names %~ focus (teleFromList ts))
          . (types %~ Map.insert n ts')
          . (constructors %~ Set.union ts') ) $
        consFst (TypeDecl am n tvs cs') $
          inferProg prg

inferProg (Open mod pre:prg) = do
  modImplicits <- view (modules . at mod . non undefined)
  local (classes %~ (<>modImplicits)) $
    consFst (Open mod pre) $ inferProg prg

inferProg (c@(Class v _ _ _ _ _):prg) = do
  (stmts, decls, clss, implicits) <- condemn $ inferClass c
  first (stmts ++) <$> do
    local (names %~ focus decls) $
      local (classDecs . at v ?~ clss) $
      local (classes %~ mappend implicits) $
        inferProg prg

inferProg (inst@Instance{}:prg) = do
  (stmt, instName, instTy) <- condemn $ inferInstance inst
  let addFst (LetStmt _ []) = id
      addFst stmt@LetStmt{} = consFst stmt
      addFst _ = undefined
  addFst stmt . local (classes %~ insert (annotation inst) InstSort instName instTy) $ inferProg prg

inferProg (Module am name body:prg) = do
  (body', env) <- inferProg body

  let (vars, tys) = extractToplevels body
      vars' = map (\x -> (x, env ^. names . at x . non (error ("value: " ++ show x)))) (vars ++ tys)

  -- Extend the current scope and module scope
  local ( (names %~ focus (teleFromList vars'))
        . (types %~ (<> (env ^. types)))
        . (classDecs %~ (<> (env ^. classDecs)))
        . (modules %~ (Map.insert name (env ^. classes) . (<> (env ^. modules))))) $
    consFst (Module am name body') $
    inferProg prg

inferProg [] = asks ([],)

inferLetTy :: forall m. MonadInfer Typed m
           => (Set.Set (Var Typed) -> Expr Typed -> Type Typed -> m (Type Typed))
           -> PatternStrat
           -> [Binding Desugared]
           -> m ( [Binding Typed]
                , Telescope Typed
                , Set.Set (Var Typed)
                )
inferLetTy closeOver strategy vs =
  let sccs = depOrder vs

      figureOut :: Bool
                -> (Var Desugared, SomeReason)
                -> Expr Typed -> Type Typed
                -> Seq.Seq (Constraint Typed)
                -> m (Type Typed, Expr Typed -> Expr Typed, Subst Typed)
      figureOut canAdd blame ex ty cs = do
        (x, co, deferred) <- condemn $ retcons (addBlame (snd blame)) (solve cs)
        deferred <- pure (fmap (apply x) deferred)
        (compose x -> x, wraps', cons) <- condemn $ solve (Seq.fromList deferred)

        name <- genName
        let reify an ty var =
              case wraps' Map.! var of
                ExprApp v -> v
                _ -> ExprWrapper (wraps' Map.! var)
                       (Fun (EvParam (Capture name (an, ty))) (VarRef name (an, ty)) (an, TyArr ty ty))
                       (an, ty)

        (context, wrapper, solve) <-
          case strategy of
            Fail -> do
              (context, wrapper, needed) <- reduceClassContext mempty (annotation ex) cons

              when (not (null needed) && not canAdd) $
                let fakeCon = ConImplicit (head needed ^. _3) undefined (fst blame) (head needed ^. _2)
                 in confesses . addBlame (snd blame) $
                   UnsatClassCon (snd blame) fakeCon (GivenContextNotEnough (getTypeContext ty))

              when (not (isFn ex) && not (null cons)) $
                confesses (addBlame (snd blame) (UnsatClassCon (snd blame) (head cons) NotAFun))

              pure (context, wrapper, \tp sub -> solveEx tp (x <> sub) (wraps' <> co))

            Propagate -> do
              tell (Seq.fromList cons)
              let notSolvedHere =
                    flip foldMap cons $ \case
                      ConImplicit _ _ v _ -> Set.singleton v
                      _ -> mempty
                  wraps'' = Map.withoutKeys (co <> wraps') notSolvedHere
              pure (id, flip const, \tp sub -> solveEx tp (x <> sub) wraps'')

        vt <- closeOver (Set.singleton (fst blame))
                ex (apply x (context ty))
        ty <- uncurry skolCheck blame vt
        let (tp, sub) = rename ty

        pure (tp, wrapper Full . solve tp sub . substituteDeferredDicts reify deferred, sub)


      tcOne :: SCC (Binding Desugared)
            -> m ( [Binding Typed]
                 , Telescope Typed
                 , Set.Set (Var Typed))

      tcOne (AcyclicSCC decl@(Binding var exp _ ann)) = do
        (origin, tv@(_, ty)) <- condemn $ approximate decl
        ((exp', ty, shouldAddContext), cs) <- listen $
          case origin of
            Guessed -> do
              (exp', ty) <- infer exp
              _ <- unify (becauseExp exp) ty (snd tv)
              pure (exp', ty, True)
            ex -> do
              checkAmbiguous var (becauseExp exp) ty
              let exp' (Ascription e _ _) = exp' e
                  exp' e = e
              exp <- check (exp' exp) ty
              pure (exp, ty, ex == Deduced)

        (tp, k, _) <- figureOut shouldAddContext (var, becauseExp exp) exp' ty cs
        -- let extra =
        --       case origin of
        --         Deduced -> \e -> ExprWrapper (TypeAsc tp) e (ann, tp)
        --         _ -> id
        pure ( [Binding var (k exp') True (ann, tp)], one var tp, mempty )

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

        (solution, wraps, deferred) <- solve cs
        let solved = closeOver mempty ex . apply solution
            ex = solveEx ty solution wraps e

        deferred <- pure (fmap (apply solution) deferred)
        (compose solution -> solution, wraps', cons) <- solve (Seq.fromList deferred)

        case strategy of
          Fail ->
            when (cons /= []) $
              confesses (ArisingFrom (UnsatClassCon (BecauseOf b) (head cons) PatBinding) (BecauseOf b))
          Propagate -> tell (Seq.fromList deferred)

        name <- genName
        let reify an ty var =
              case wraps' Map.! var of
                ExprApp v -> v
                _ -> ExprWrapper (wraps' Map.! var)
                       (Fun (EvParam (Capture name (an, ty))) (VarRef name (an, ty)) (an, TyArr ty ty))
                       (an, ty)
        tel' <- traverseTele (const solved) tel
        ty <- solved ty

        let pat = transformPatternTyped id (apply solution) p
            ex' = solveEx ty solution wraps' $ substituteDeferredDicts reify deferred ex

        pure ( [TypedMatching pat ex' (ann, ty) (teleToList tel')], tel', boundTvs pat tel' )

      tcOne (CyclicSCC vars) = do
        () <- guardOnlyBindings vars
        (origins, tvs) <- unzip <$> traverse approximate vars

        (bindings, cs) <- listen . local (names %~ focus (teleFromList tvs)) $
          ifor (zip tvs vars) $ \i ((_, tyvar), Binding var exp _ ann) ->
            case origins !! i of
              Guessed -> do
                (exp', ty) <- infer exp
                _ <- unify (becauseExp exp) ty tyvar
                pure (Binding var exp' True (ann, ty), ty)
              _ -> do
                checkAmbiguous var (becauseExp exp) tyvar
                let exp' (Ascription e _ _) = exp' e
                    exp' e = e
                exp <- check (exp' exp) tyvar
                pure (Binding var exp True (ann, tyvar), tyvar)

        (solution, wrap, cons) <- solve cs
        name <- genName

        let deferred = fmap (apply solution) cons
        (compose solution -> solution, wrap', cons) <- solve (Seq.fromList deferred)
        let reify an ty var =
              case wrap' Map.! var of
                ExprApp v -> v
                _ -> ExprWrapper (wrap Map.! var)
                       (Fun (EvParam (Capture name (an, ty))) (VarRef name (an, ty)) (an, TyArr ty ty))
                       (an, ty)

        if null cons
           then do
             let solveOne :: (Binding Typed, Type Typed) -> m ( Binding Typed )
                 solveOne (Binding var exp _ an, ty) = do
                   ty <- closeOver mempty exp (apply solution ty)
                   (new, sub) <- pure $ rename ty
                   pure ( Binding var
                           (shoveLets (solveEx new (sub `compose` solution) (wrap <> wrap') exp))
                           True
                           (fst an, new)
                        )
                 solveOne _ = undefined

                 shoveLets (ExprWrapper w e a) = ExprWrapper w (shoveLets e) a
                 shoveLets (Fun x@EvParam{} e a) = Fun x (shoveLets e) a
                 shoveLets e = substituteDeferredDicts reify deferred e
             bs <- traverse solveOne bindings
             let makeTele (Binding var _ _ (_, ty):bs) = one var ty <> makeTele bs
                 makeTele _ = mempty
                 makeTele :: [Binding Typed] -> Telescope Typed
             pure (bs, makeTele bs, mempty)
           else do
             when (any (/= Guessed) origins || all (== Supplied) origins) $ do
               let Just reason = fmap fst $
                     find ((/= Guessed) . snd) (zip vars origins)
                   blame = BecauseOf reason
               confesses $ ArisingFrom (UnsatClassCon blame (head cons) RecursiveDeduced) blame

             recVar <- genName
             innerNames <- fmap Map.fromList . for tvs $ \(v, _) ->
               (v,) <$> genNameFrom (T.cons '$' (nameName v))
             let renameInside (VarRef v a) | Just x <- Map.lookup v innerNames = VarRef x a
                 renameInside x = x

             let solveOne :: (Binding Typed, Type Typed)
                          -> m ( (T.Text, Var Typed, Ann Desugared, Type Typed), Binding Typed, Field Typed )
                 solveOne (Binding var exp _ an, ty) = do
                   let nm = nameName var
                   ty <- pure (apply solution ty)

                   pure ( (nm, var, fst an, ty)
                        , Binding (innerNames Map.! var)
                           (Ascription
                             (transformExprTyped renameInside id id (solveEx ty solution wrap exp))
                             ty (fst an, ty))
                           True
                           (fst an, ty)
                        , Field nm (VarRef (innerNames Map.! var) (fst an, ty)) (fst an, ty)
                        )
                 solveOne _ = undefined

             (info, inners, fields) <- unzip3 <$> traverse solveOne bindings
             let (blamed:_) = inners
                 an = annotation blamed
                 recTy = TyExactRows rows
                 rows = map (\(t, _, _, ty) -> (t, ty)) info

             (context, wrapper, needed) <- reduceClassContext mempty (annotation blamed) cons

             closed <- skolCheck recVar (BecauseOf blamed) <=<
               closeOver (Set.fromList (map fst tvs)) (Record fields (an, recTy)) $
                 context recTy
             (closed, _) <- pure $ rename closed
             tyLams <- mkTypeLambdas (ByConstraint recTy) closed

             let record =
                   Binding recVar
                     (Ascription
                       (ExprWrapper tyLams
                         (wrapper Full
                           (substituteDeferredDicts reify deferred
                             (Let inners (Record fields (an, recTy)) (an, recTy))))
                         (an, closed))
                       closed (an, closed))
                     True
                     (an, closed)
                 makeOne (nm, var, an, ty) = do
                   ty' <- skolCheck recVar (BecauseOf blamed)
                     <=< closeOver (Set.fromList (map fst tvs)) (Record fields (an, recTy)) $
                      context ty
                   (ty', _) <- pure $ rename ty'
                   let lineUp c (TyForall v _ rest) ex =
                         lineUp c rest $ ExprWrapper (TypeApp (TyVar v)) ex (annotation ex, rest)
                       lineUp ((v, t, _):cs) (TyPi (Implicit _) rest) ex =
                         lineUp cs rest $ App ex (VarRef v (annotation ex, t)) (annotation ex, rest)
                       lineUp _ _ e = e
                       lineUp :: [(Var Typed, Type Typed, SomeReason)] -> Type Typed -> Expr Typed -> Expr Typed
                   pure $ Binding var
                     (ExprWrapper tyLams
                       (wrapper Thin
                         (Access
                             (ExprWrapper (TypeAsc recTy)
                               (lineUp needed closed (VarRef recVar (an, closed)))
                               (an, recTy))
                             nm (an, ty)))
                       (an, ty'))
                     True
                     (an, ty')

             getters <- traverse makeOne info
             let makeTele (Binding var _ _ (_, ty):bs) = one var ty <> makeTele bs
                 makeTele _ = mempty
                 makeTele :: [Binding Typed] -> Telescope Typed
             pure (record:getters, makeTele getters, mempty)

      tc :: [SCC (Binding Desugared)]
         -> m ( [Binding Typed] , Telescope Typed, Set.Set (Var Typed) )
      tc (s:cs) = do
        (vs', binds, vars) <- tcOne s
        fmap (\(bs, tel, vs) -> (vs' ++ bs, tel <> binds, vars <> vs))
          . local (names %~ focus binds) . local (typeVars %~ Set.union vars) $ tc cs
      tc [] = pure ([], mempty, mempty)
   in tc sccs

buildList :: Ann Resolved -> Type Typed -> [Expr Typed] -> Expr Typed
buildList an tau [] =
  ExprWrapper (TypeApp tau)
    (VarRef nILName (an, nILTy))
    (an, ty)
  where ty = TyApp tyList tau

buildList an tau (x:xs) =
  App (ExprWrapper (TypeApp tau)
        (VarRef cONSName (an, cONSTy)) (an, cONSTy' tau))
    (Tuple [x, buildList an tau xs]
      (an, TyTuple tau ty))
    (an, ty)
  where ty = TyApp tyList tau

fakeLetTys :: MonadInfer Typed m
           => [Binding Desugared]
           -> m ([Binding Typed], Telescope Typed, Set.Set (Var Typed))
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
            tel <- flip (`foldM` mempty) (bound' p) $ \rest var -> do
              ty <- freshTV
              pure (one var ty <> rest)
            (<>) tel <$> go bs
          p -> error ("fakeLetTys impossible: " ++ show (pretty p))
      go [] = pure mempty
  tele <- go bs
  pure (mempty, tele, mempty)


data PatternStrat = Fail | Propagate

skolCheck :: MonadInfer Typed m => Var Typed -> SomeReason -> Type Typed -> m (Type Typed)
skolCheck var exp ty = do
  let blameSkol :: TypeError -> (Var Desugared, SomeReason) -> TypeError
      blameSkol e (v, r) =
        addBlame r . Note e $
          string "in the inferred type for" <+> pretty v <> comma <#> indent 4 (displayType ty)
      sks = Set.map (^. skolIdent) (skols ty)
  env <- view typeVars
  unless (null (sks `Set.difference` env)) $
    confesses (blameSkol (EscapedSkolems (Set.toList (skols ty)) ty) (var, exp))

  let t = deSkol ty
  checkAmbiguous var exp t
  pure t

checkAmbiguous :: MonadInfer Typed m => Var Typed -> SomeReason -> Type Typed -> m ()
checkAmbiguous var exp tau = go mempty tau where
  go s (TyPi Invisible{} t) = go s t
  go s (TyPi (Implicit v) t) = go (s <> ftv v) t
  go s t = if not (Set.null (s Set.\\ fv))
              then confesses (addBlame exp (AmbiguousType var tau (s Set.\\ fv)))
              else pure ()
    where fv = ftv t

solveEx :: Type Typed -> Subst Typed -> Map.Map (Var Typed) (Wrapper Typed) -> Expr Typed -> Expr Typed
solveEx _ ss cs = transformExprTyped go id goType where
  go :: Expr Typed -> Expr Typed
  go (ExprWrapper w e a) = case goWrap w of
    WrapFn w@(MkWrapCont _ desc) -> ExprWrapper (WrapFn (MkWrapCont id desc)) (runWrapper w e) a
    x -> ExprWrapper x e a
  go x = x

  goWrap (TypeApp t) = TypeApp (goType t)
  goWrap (TypeAsc t) = TypeAsc (goType t)
  goWrap (Cast c) = case c of
    ReflCo{} -> IdWrap
    AssumedCo a b | a == b -> IdWrap
    _ -> Cast c
  goWrap (TypeLam l t) = TypeLam l (goType t)
  goWrap (ExprApp f) = ExprApp (go f)
  goWrap (x Syntax.:> y) = goWrap x Syntax.:> goWrap y
  goWrap (WrapVar v) =
    case Map.lookup v cs of
      Just x -> goWrap x
      Nothing -> WrapVar v
  goWrap IdWrap = IdWrap
  goWrap (WrapFn f) = WrapFn . flip MkWrapCont (desc f) $ solveEx undefined ss cs . runWrapper f

  goType :: Type Typed -> Type Typed
  goType = apply ss

consFst :: Functor m => a -> m ([a], b) -> m ([a], b)
consFst = fmap . first . (:)

rename :: Type Typed -> (Type Typed, Subst Typed)
rename = go 0 mempty mempty where
  go :: Int -> Set.Set T.Text -> Subst Typed -> Type Typed -> (Type Typed, Subst Typed)
  go n l s (TyPi (Invisible v k req) t) =
    let (v', n', l') = new n l v
        s' = Map.insert v (TyVar v') s
        (ty, s'') = go n' l' s' t
     in (TyPi (Invisible v' (fmap (apply s) k) req) ty, s'')
  go n l s (TyPi (Anon k) t) =
    let (ty, sub) = go n l s t
     in (TyPi (Anon (fst (go n l s k))) ty, sub)
  go _ _ s tau = (apply s tau, s)

  new v l var@(TgName vr n)
    | toIdx vr == n || vr `Set.member` l =
      let name = genAlnum v
       in if Set.member name l
             then new (v + 1) l var
             else (TgName name n, v + 1, Set.insert name l)
    | otherwise = (TgName vr n, v, Set.insert vr l)

  new _ _ TgInternal{} = error "TgInternal in rename"

  toIdx t = go (T.unpack t) (T.length t - 1) - 1 where
    go (c:cs) l = (ord c - 96) * (26 ^ l) + go cs (l - 1)
    go [] _ = 0

localGenStrat :: forall p m. (Var p ~ Var Desugared, Respannable (Ann p), MonadInfer Typed m)
              => Set.Set (Var Typed) -> Expr p -> Type Typed -> m (Type Typed)
localGenStrat bg ex ty = do
  bound <- view letBound
  cons <- view constructors
  types <- view names
  let generalisable v =
        (Set.member v bound || Set.member v cons || Set.member v builtinNames)
       && maybe True Set.null (types ^. at v . to (fmap ftv))

  if Set.foldr ((&&) . generalisable) True (freeIn ex Set.\\ bg) && value ex
     then generalise ftv (becauseExp ex) ty
     else annotateKind (becauseExp ex) ty

type Reify p = (Ann Desugared -> Type p -> Var p -> Expr p)

substituteDeferredDicts :: Reify Typed -> [Constraint Typed] -> Expr Typed -> Expr Typed
substituteDeferredDicts reify cons = transformExprTyped go id id where
  go (VarRef v _) | Just x <- Map.lookup v replaced = x
  go x = x


  choose (ConImplicit _ _ var ty) =
    let ex = operational (reify internal ty var)
     in Map.singleton var ex
  choose _ = error "Not a deferred ConImplicit in choose substituteDeferredDicts"

  replaced = foldMap choose cons

operational :: Expr Typed -> Expr Typed
operational (ExprWrapper IdWrap e _) = e
operational (ExprWrapper (WrapFn k) e _) = operational $ runWrapper k e
operational e = e

closeOverStrat :: MonadInfer Typed m
               => SomeReason
               -> Set.Set (Var Typed) -> Expr Typed -> Type Typed -> m (Type Typed)
closeOverStrat r _ _ = closeOver r

firstForall :: MonadInfer Typed m => Type Desugared -> Type Typed -> m (TyBinder Typed, Type Typed)
firstForall _ (TyPi x@Invisible{} k) = pure (x, k)
firstForall a e = confesses (CanNotVta e a)


deSkol :: Type Typed -> Type Typed
deSkol = go mempty where
  go acc (TyPi x k) =
    case x of
      Invisible v kind req -> TyPi (Invisible v kind req) (go (Set.insert v acc) k)
      Anon a -> TyPi (Anon (go acc a)) (go acc k)
      Implicit a -> TyPi (Implicit (go acc a)) (go acc k)
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
  go acc (TyOperator l o r) = TyOperator (go acc l) o (go acc r)
  go acc (TyParens p) = TyParens $ go acc p
  go _ TyType = TyType

guardOnlyBindings :: MonadChronicles TypeError m
                  => [Binding Desugared] -> m ()
guardOnlyBindings bs = go bs where
  go (Binding{}:xs) = go xs
  go (m@Matching{}:_) =
    confesses (addBlame (BecauseOf m) (PatternRecursive m bs))

  go (TypedMatching{}:_) = error "TypedMatching in guardOnlyBindings"
  go [] = pure ()

nameName :: Var Desugared -> T.Text
nameName (TgInternal x) = x
nameName (TgName x _) = x

mkTypeLambdas :: MonadNamey m => SkolemMotive Typed -> Type Typed -> m (Wrapper Typed)
mkTypeLambdas motive ty@(TyPi (Invisible tv k _) t) = do
  sk <- freshSkol motive ty tv
  wrap <- mkTypeLambdas motive (apply (Map.singleton tv sk) t)
  kind <- case k of
    Nothing -> freshTV
    Just x -> pure x
  let getSkol (TySkol s) = s
      getSkol _ = error "not a skolem from freshSkol"
  pure (TypeLam (getSkol sk) kind Syntax.:> wrap)
mkTypeLambdas _ _ = pure IdWrap

getTypeContext :: Type Typed -> Type Typed
getTypeContext ty =
  case getCtxParts ty of
    [] -> tyUnit
    (x:xs) -> foldr TyTuple x xs
  where
    getCtxParts (TyPi (Implicit v) t) = v:getCtxParts t
    getCtxParts (TyPi _ t) = getCtxParts t
    getCtxParts _ = []
