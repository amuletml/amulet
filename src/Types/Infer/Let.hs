{-# LANGUAGE FlexibleContexts, TupleSections, ScopedTypeVariables,
   LambdaCase, TypeFamilies, CPP #-}
module Types.Infer.Let (inferLetTy, fakeLetTys, rename, skolCheck, PatternStrat(..), localGenStrat, solveFixpoint) where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Set as Set
import Data.Traversable
import Data.List (find)
import Data.Spanned
import Data.Reason
import Data.Triple
import Data.Graph
import Data.Char

import Control.Monad.State
import Control.Monad.Infer
import Control.Lens

import Syntax.Transform
import Syntax.Builtin
import Syntax.Value
import Syntax.Subst
import Syntax.Types
import Syntax.Let
import Syntax.Var
import Syntax

import Types.Infer.Pattern
import Types.Infer.Builtin
import Types.Infer.Outline
import Types.Infer.Errors
import Types.Wellformed
import Types.Kinds
import Types.Unify

import Text.Pretty.Semantic

import {-# SOURCE #-} Types.Infer.Class
import {-# SOURCE #-} Types.Infer

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
        c <- getSolveInfo
        (x, wraps, cons) <- condemn $ retcons (addBlame (snd blame)) (solveFixpoint (snd blame) cs c)

        ts <- view tySyms

        (context, wrapper, solve, reduce_sub) <-
          case strategy of
            Fail -> do
              (context, wrapper, needed, sub') <- reduceClassContext mempty (spanOf ex) cons

              let needsLet = wraps `Map.restrictKeys` freeIn ex
                  addOne (v, ExprApp e) ex
                    | VarRef v' _ <- e, v == v' = ex
                    | otherwise = Let Recursive [ Binding v (spanOf ex) e False (spanOf ex, getType e) ] ex (spanOf ex, getType ex)
                  addOne _ ex = ex
                  addFreeDicts ex = foldr addOne ex (Map.toList needsLet)

              when (not (null needed) && not canAdd) $
                let fakeCon = ConImplicit (head needed ^. _3) undefined (fst blame) (head needed ^. _2)
                 in confesses =<< unsatClassCon (Const (snd blame)) fakeCon (GivenContextNotEnough (getTypeContext ty))

              when (not (isFn ex) && not (null cons)) $
                confesses =<< unsatClassCon (Const (snd blame)) (head cons) NotAFun

              pure ( context
                   , wrapper
                   , \_ sub -> solveEx ts (x `compose` sub `compose` sub') wraps . addFreeDicts
                   , sub' )

            Propagate -> do
              tell (Seq.fromList cons)
              let notSolvedHere =
                    flip foldMap cons $ \case
                      ConImplicit _ _ v _ -> Set.singleton v
                      _ -> mempty
                  wraps'' = Map.withoutKeys wraps notSolvedHere
              pure (id, \_ x -> x, \_ sub -> solveEx ts (x <> sub) wraps'', mempty)

        vt <- closeOver (Set.singleton (fst blame))
                ex (apply (x `compose` reduce_sub) (context ty))
        ty <- uncurry skolCheck blame vt
        let (tp, sub) = rename ty

        pure (tp, wrapper Full . solve tp sub, sub)


      tcOne :: SCC (Binding Desugared)
            -> m ( [Binding Typed]
                 , Telescope Typed
                 , Set.Set (Var Typed))

      tcOne (AcyclicSCC decl@(Binding var vp exp _ ann)) = do
        (origin, tv@(_, ty)) <- condemn $ approximate decl
        ((exp', ty, shouldAddContext), cs) <- listen $
          case origin of
            Guessed -> do
              (exp', ty) <- infer exp
              _ <- unify (becauseExp exp) ty (snd tv)
              pure (exp', ty, True)
            ex -> do
              let exp' (Ascription e _ _) = exp' e
                  exp' e = e
              exp <- check (exp' exp) ty
              pure (exp, ty, ex == Deduced)

        (tp, k, _) <- figureOut shouldAddContext (var, becauseExp exp) exp' ty cs
        -- let extra =
        --       case origin of
        --         Deduced -> \e -> ExprWrapper (TypeAsc tp) e (ann, tp)
        --         _ -> id
        pure ( [Binding var vp (k exp') True (ann, tp)], one var tp, mempty )

      tcOne (AcyclicSCC TypedMatching{}) = error "TypedMatching being TC'd"
      tcOne (AcyclicSCC b@(Matching p e ann)) = do
        ((e, p, ty, tel), cs) <- listen $ do
          (e, ety) <- infer e
          (p, pty, tel, cs, _) <- inferPattern p
          leakEqualities b cs

          -- here: have expression type (inferred)
          --       want pattern type (inferred)
          wrap <- subsumes (BecauseOf b) ety pty
          pure (ExprWrapper wrap e (spanOf e, pty), p, pty, tel)

        (solution, wraps, cons) <- solveFixpoint (BecauseOf b) cs =<< getSolveInfo
        tys <- view tySyms
        let solved = closeOver mempty ex . apply solution
            ex = solveEx tys solution wraps e

        case strategy of
          Fail ->
            when (cons /= []) $
              confesses =<< unsatClassCon b (head cons) PatBinding
          Propagate -> tell (Seq.fromList cons)

        tel' <- traverseTele (const solved) tel
        ty <- solved ty
        tys <- view tySyms

        let pat = transformPatternTyped id (apply solution) p
            ex' = solveEx tys solution wraps ex

        pure ( [TypedMatching pat ex' (ann, ty) (teleToList tel')], tel', boundTvs pat tel' )

      tcOne (CyclicSCC vars) = do
        () <- guardOnlyBindings vars
        (origins, tvs) <- unzip <$> traverse approximate vars

        (bindings, cs) <- listen . local (names %~ focus (teleFromList tvs)) $
          ifor (zip tvs vars) $ \i ((_, tyvar), Binding var vp exp _ ann) ->
            case origins !! i of
              Guessed -> do
                (exp', ty) <- infer exp
                _ <- unify (becauseExp exp) ty tyvar
                pure (Binding var vp exp' True (ann, ty), ty)
              _ -> do
                let exp' (Ascription e _ _) = exp' e
                    exp' e = e
                exp <- check (exp' exp) tyvar
                pure (Binding var vp exp True (ann, tyvar), tyvar)

        (solution, wrap, cons) <- condemn $ solveFixpoint (It'sThis (BecauseInternal "fixed point solving")) cs =<< getSolveInfo

        tys <- view tySyms
        if null cons
           then do
             let solveOne :: (Binding Typed, Type Typed) -> m ( Binding Typed )
                 solveOne (Binding var vp exp _ an, ty) = do
                   ty <- closeOver mempty exp (apply solution ty)
                   (new, sub) <- pure $ rename ty
                   pure ( Binding var vp
                           (solveEx tys (sub `compose` solution) wrap exp)
                           True
                           (fst an, new)
                        )
                 solveOne _ = undefined

             bs <- traverse solveOne bindings
             let makeTele (Binding var _ _ _ (_, ty):bs) = one var ty <> makeTele bs
                 makeTele _ = mempty
                 makeTele :: [Binding Typed] -> Telescope Typed
             pure (bs, makeTele bs, mempty)
           else do
             when (any (/= Guessed) origins || all (== Supplied) origins) $ do
               let Just reason = fst <$>
                     find ((/= Guessed) . snd) (zip vars origins)
               confesses =<< unsatClassCon reason (head cons) RecursiveDeduced

             recVar <- genName
             innerNames <- fmap Map.fromList . for tvs $ \(v, _) ->
               (v,) <$> genNameFrom (T.cons '$' (nameName v))
             let renameInside (VarRef v a) | Just x <- Map.lookup v innerNames = VarRef x a
                 renameInside x = x

             let solveOne :: Subst Typed
                          -> (Binding Typed, Type Typed)
                          -> m ( (T.Text, Var Typed, Ann Desugared, Type Typed), Binding Typed, Field Typed )
                 solveOne sub (Binding var vp exp _ an, ty) = do
                   let nm = nameName var
                   ty <- pure (apply (solution <> sub) ty)

                   pure ( (nm, var, fst an, ty)
                        , Binding (innerNames Map.! var) vp
                           (Ascription
                             (transformExprTyped renameInside id id (solveEx tys (solution <> sub) wrap exp))
                             ty (fst an, ty))
                           True
                           (fst an, ty)
                        , Field nm (VarRef (innerNames Map.! var) (fst an, ty)) (fst an, ty)
                        )
                 solveOne _ _ = undefined

             let (blamed:_) = vars
                 an = spanOf blamed

             (context, wrapper, needed, sub') <- reduceClassContext mempty (spanOf blamed) cons

             (info, inners, fields) <- unzip3 <$> traverse (solveOne sub') bindings
             let rows = map (\(t, _, _, ty) -> (t, ty)) info
                 recTy = TyExactRows rows

             closed <- skolCheck recVar (BecauseOf blamed) <=<
               closeOver (Set.fromList (map fst tvs)) (Record fields (an, recTy)) $
                 context recTy
             (closed, _) <- pure $ rename closed
             tyLams <- mkTypeLambdas (ByConstraint recTy) closed

             let record =
                   Binding recVar an
                     (Ascription
                       (ExprWrapper tyLams
                         (wrapper Full
                           (Let Recursive inners (Record fields (an, recTy)) (an, recTy)))
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
                         lineUp c rest $ ExprWrapper (TypeApp (TyVar v ())) ex (spanOf ex, rest)
                       lineUp ((v, t, _):cs) (TyPi (Implicit _) rest) ex =
                         lineUp cs rest $ App ex (VarRef v (spanOf ex, t)) (spanOf ex, rest)
                       lineUp _ _ e = e
                       lineUp :: [(Var Typed, Type Typed, SomeReason)] -> Type Typed -> Expr Typed -> Expr Typed
                   pure $ Binding var an
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
             let makeTele (Binding var _ _ _ (_, ty):bs) = one var ty <> makeTele bs
                 makeTele _ = mempty
                 makeTele :: [Binding Typed] -> Telescope Typed
             pure (record:getters, makeTele getters, mempty)

      tc :: [SCC (Binding Desugared)]
         -> m ( [Binding Typed] , Telescope Typed, Set.Set (Var Typed) )
      tc (s:cs) = do
        (vs', binds, vars) <- tcOne s
        vs' <- traverse expandTyBindings vs'
        fmap (\(bs, tel, vs) -> (vs' ++ bs, tel <> binds, vars <> vs))
          . local (names %~ focus binds) . local (typeVars %~ Set.union vars) $ tc cs
      tc [] = pure ([], mempty, mempty)
   in tc sccs

fakeLetTys :: MonadInfer Typed m
           => [Binding Desugared]
           -> m ([Binding Typed], Telescope Typed, Set.Set (Var Typed))
fakeLetTys bs = do
  let go (b:bs) =
        case b of
          Binding _ _ e _ _ -> do
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

skolCheck :: ( MonadChronicles TypeError m
             , MonadReader Env m
             )
          => Var Typed -> SomeReason -> Type Typed -> m (Type Typed)
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
  condemn $ checkAmbiguous var exp t
  pure t

checkAmbiguous :: forall m. ( MonadChronicles TypeError m
                            , MonadReader Env m
                            )
               => Var Typed -> SomeReason -> Type Typed -> m ()
checkAmbiguous var exp tau = go mempty mempty mempty tau where
  go :: Set.Set (Var Typed) -> OccMap Typed -> Set.Set (Var Typed) -> Type Typed -> m ()
  go ok s tfs (TyPi (Invisible v _ Req) t) = go (Set.insert v ok) s tfs t
  go ok s tfs(TyPi Invisible{} t) = go ok s tfs t
  go ok s tfs (TyPi (Implicit v) t)
    | (TyCon clss ():args) <- appsView v = do
        ci <- view (classDecs . at clss)
        case ci of
          Just ci ->
            let fds = ci ^. ciFundep
                det (_, x, _) = x
                fundep_ok = foldMap (ftv . (args !!)) (foldMap det fds)
             in go (ok <> fundep_ok) (s <> tyVarOcc v) tfs t
          Nothing -> go ok (s <> tyVarOcc v) tfs t
    | TyTuple a b <- v = go ok s tfs (TyPi (Implicit a) (TyPi (Implicit b) t))
    | otherwise = go ok (s <> tyVarOcc v) tfs t

  go ok s tfs (TyPi (Anon dom) cod) = do
    scope <- view tySyms

    let (no_can_do, tfs') =
          foldMapOf cosmos fv_under_tf dom

        fv_under_tf (TyApps (TyCon v _) apps)
          | v `Map.member` scope = (ftv apps, Set.singleton v)
          | otherwise = mempty
        fv_under_tf _ = mempty

        ok_fvs = fv `Set.difference` no_can_do
    go (ok <> ok_fvs) s (tfs <> tfs') cod
    where fv = ftv dom

  go ok s tfs t = do
    scope <- view tySyms

    let (no_can_do, tfs') =
          foldMapOf cosmos fv_under_tf t

        fv_under_tf (TyApps (TyCon v _) apps)
          | v `Map.member` scope = (ftv apps, Set.singleton v)
          | otherwise = mempty
        fv_under_tf _ = mempty

        ok_fvs = fv `subtractOccs` no_can_do

    let fv_set = occToFv ((s `diffOccs` ok_fvs) `removeOccs` ok)

    if not (Set.null fv_set)
       then confesses (addBlame exp (note_tfs scope (Set.toList (tfs <> tfs'))
                          (AmbiguousType var tau fv_set)))
       else pure ()
    where fv = tyVarOcc t

  note_tfs :: Map.Map (Var Typed) TySymInfo -> [Var Typed] -> TypeError -> TypeError
  note_tfs scope (v:vs) e =
    Note (note_tfs scope vs e) $
      case scope ^. at v of
        Just TyFamInfo { _tsConstraint = Just _ } ->
          displayType (TyCon v () :: Type Typed)
            <+> string "is an" <+> keyword "associated type" <> string ", and so may not be injective"
        Just TyFamInfo{} ->
          displayType (TyCon v () :: Type Typed)
            <+> string "is a" <+> keyword "type function" <> string ", and so may not be injective"
        _ -> undefined
  note_tfs _ [] e = e


rename :: Type Typed -> (Type Typed, Subst Typed)
rename = go 0 mempty mempty where
  go :: Int -> Set.Set T.Text -> Subst Typed -> Type Typed -> (Type Typed, Subst Typed)
  go n l s (TyPi (Invisible v k req) t) =
    let (v', n', l') = new n l v
        s' = Map.insert v (TyVar v' ()) s
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


deSkol :: Type Typed -> Type Typed
deSkol = go mempty where
  go acc (TyPi x k) =
    case x of
      Invisible v kind req -> TyPi (Invisible v kind req) (go (Set.insert v acc) k)
      Anon a -> TyPi (Anon (go acc a)) (go acc k)
      Implicit a -> TyPi (Implicit (go acc a)) (go acc k)
  go acc ty@(TySkol (Skolem _ var _ _))
    | var `Set.member` acc = TyVar var ()
    | otherwise = ty
  go _ x@TyCon{} = x
  go _ x@TyLit{} = x
  go _ x@TyVar{} = x
  go _ x@TyPromotedCon{} = x
  go acc (TyApp f x) = TyApp (go acc f) (go acc x)
  go acc (TyRows t rs) = TyRows (go acc t) (map (_2 %~ go acc) rs)
  go acc (TyExactRows rs) = TyExactRows (map (_2 %~ go acc) rs)
  go acc (TyTuple a b) = TyTuple (go acc a) (go acc b)
  go acc (TyWildcard x) = TyWildcard (go acc <$> x)
  go acc (TyWithConstraints cs x) = TyWithConstraints (map (bimap (go acc) (go acc)) cs) (go acc x)
  go acc (TyOperator l o r) = TyOperator (go acc l) o (go acc r)
  go acc (TyParens p) = TyParens $ go acc p
  go acc (TyTupleL a b) = TyTupleL (go acc a) (go acc b)
  go _ TyType = TyType

expandTyBindings :: MonadReader Env m => Binding Typed -> m (Binding Typed)
expandTyBindings = bindAnn %%~ secondA expandType

mkTypeLambdas :: MonadNamey m => SkolemMotive Typed -> Type Typed -> m (Wrapper Typed)
mkTypeLambdas motive ty@(TyPi (Invisible tv k _) t) = do
  sk <- freshSkol motive ty tv
  wrap <- mkTypeLambdas motive (apply (Map.singleton tv sk) t)
  kind <- maybe freshTV pure k
  let getSkol (TySkol s) = s
      getSkol _ = error "not a skolem from freshSkol"
  pure (TypeLam (getSkol sk) kind Syntax.:> wrap)
mkTypeLambdas _ _ = pure IdWrap

nameName :: Var Desugared -> T.Text
nameName (TgInternal x) = x
nameName (TgName x _) = x

guardOnlyBindings :: MonadChronicles TypeError m
                  => [Binding Desugared] -> m ()
guardOnlyBindings bs = go bs where
  go (Binding{}:xs) = go xs
  go (m@Matching{}:_) =
    confesses (addBlame (BecauseOf m) (PatternRecursive m bs))

  go (TypedMatching{}:_) = error "TypedMatching in guardOnlyBindings"
  go [] = pure ()

getTypeContext :: Type Typed -> Type Typed
getTypeContext ty =
  case getCtxParts ty of
    [] -> tyUnit
    (x:xs) -> foldr TyTuple x xs
  where
    getCtxParts (TyPi (Implicit v) t) = v:getCtxParts t
    getCtxParts (TyPi _ t) = getCtxParts t
    getCtxParts _ = []
