{-# LANGUAGE ConstraintKinds, FlexibleContexts, LambdaCase, TypeFamilies, ViewPatterns #-}
module Types.Kinds
  ( resolveKind
  , resolveTyDeclKind, resolveClassKind, resolveTySymDeclKind, resolveTyFunDeclKind
  , annotateKind
  , closeOver, closeOver'
  , checkAgainstKind, getKind, liftType
  , generalise
  , checkKind
  , solveFixpoint
  )
  where

import Control.Monad.State.Strict
import Control.Monad.Chronicles
import Control.Monad.Infer
import Control.Applicative
import Control.Arrow hiding ((<+>))
import Control.Lens

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Traversable
import Data.Foldable
import Data.Spanned
import Data.Reason
import Data.Triple
import Data.Maybe

import Types.Wellformed (wellformed)
import Types.Infer.Builtin
import Types.Infer.Errors
import Types.Unify (solve, getSolveInfo, unifyPure_v, freshSkol)

import Syntax.Implicits
import Syntax.Transform
import Syntax.Builtin
import Syntax.Subst
import Syntax.Types
import Syntax.Raise
import Syntax.Var
import Syntax

import Types.Unify.Trace
import Text.Pretty.Semantic

import {-# SOURCE #-} Types.Infer.Let (skolCheck)

type KindT m = StateT SomeReason (WriterT (Seq.Seq (Constraint Typed)) m)

type MonadKind m =
  ( MonadChronicles TypeError m
  , MonadReader Env m
  , MonadNamey m
  , MonadWriter (Seq.Seq (Constraint Typed)) m
  )

type Kind = Type

solveFixpoint :: (MonadNamey m, MonadChronicles TypeError m)
              => SomeReason
              -> Seq.Seq (Constraint Typed)
              -> Map.Map (Var Resolved) (Either ClassInfo TySymInfo)
              -> m (Subst Typed, Map.Map (Var Resolved) (Wrapper Typed), [Constraint Typed])
solveFixpoint blame = (fmap (_3 %~ reblame_con blame) . ) . go True (mempty, mempty) where
  go True (sub, wraps) cs c = do
    (compose sub -> sub, wraps', cons) <- solve cs c
    let new_cons = apply sub cons

    go (length cons < Seq.length cs && any isEquality new_cons) (sub, wraps' <> wraps) (Seq.fromList new_cons) c

  go False (sub, wraps) cs c = do
    (compose sub -> sub, wraps', cons) <- solve cs c
    (compose sub -> sub, wraps'', cons) <- solve (Seq.fromList (apply sub cons)) c
    pure (sub, wraps'' <> wraps' <> wraps, apply sub cons)

  isEquality (ConImplicit _ _ _ (TyApps t as)) | t == tyEq && solvable as = True
  isEquality _ = False

  solvable [TyApps{}, TyVar{}] = True
  solvable [TyVar{}, TyApps{}] = True
  solvable _ = False

  reblame_con r = map go where
    go (ConUnify _ a b c d) = ConUnify r a b c d
    go (ConSubsume _ a b c d) = ConSubsume r a b c d
    go (ConImplies _ a b c) = ConImplies r a b c
    go (ConImplicit (It'sThis BecauseInternal{}) a b c) = ConImplicit r a b c
    go (ConImplicit r a b c) = ConImplicit r a b c
    go x@ConFail{} = x
    go x@DeferredError{} = x


resolveKind :: MonadKind m => SomeReason -> Type Desugared -> m (Type Typed)
resolveKind reason otp = do
  ((ty, _), cs) <- let cont t = do
                        (t, _) <- secondA isType =<< inferKind t
                        expandType t
                     in runWriterT (runStateT (cont otp) reason)

  (sub, _, cons) <- solveFixpoint reason cs =<< getSolveInfo

  unless (null cons) $ do
    tell (Seq.fromList cons)

  let t = apply sub ty
  wellformed t
  pure t

getKind :: MonadKind m => SomeReason -> Type Desugared -> m (Kind Typed)
getKind r t = solveK pure r (snd <$> inferKind t)

liftType :: MonadKind m => SomeReason -> Type Desugared -> m (Type Typed)
liftType r t = solveK pure r (fst <$> inferKind t)

checkAgainstKind :: MonadInfer Typed m => SomeReason -> Type Desugared -> Type Typed -> m (Type Typed)
checkAgainstKind r t k = solveK pure r $
  checkKind t k

annotateKind :: MonadKind m => SomeReason -> Type Typed -> m (Type Typed)
annotateKind r ty = do
  ((ty, _), cs) <- runWriterT (runStateT (fmap fst (inferKind (raiseT id ty))) r)
  (sub, _, cons) <- solveFixpoint r cs =<< getSolveInfo
  unless (null cons) $ do
    tell (Seq.fromList cons)
  pure (apply sub ty)

initialKind :: MonadKind m => Dep -> Type Typed -> [TyConArg Desugared] -> KindT m (Type Typed, Telescope Typed)
initialKind dep k (TyVarArg v:as) = do
  (k, t) <- initialKind dep k as
  ty <- freshTV
  pure (mkFunT dep v ty k, one v ty <> t)
initialKind dep ret (TyAnnArg v k:as) = do
  k <- checkKind k TyType
  (s, t) <- initialKind dep ret as
  pure (mkFunT dep v k s, t <> one v k)
initialKind _ ret [] = pure (ret, mempty)

mkFunT :: Dep -> Var Resolved -> Type Typed -> Type Typed -> Type Typed
mkFunT NoDep _ a b = TyArr a b
mkFunT LotsaDep v a b = TyPi (Invisible v (Just a) Req) b

resolveTyFunDeclKind :: MonadKind m
                     => SomeReason
                     -> Var Typed
                     -> [TyConArg Desugared]
                     -> Maybe (Type Desugared)
                     -> [TyFunClause Desugared]
                     -> m (Type Typed, [TyFunClause Typed], [TyConArg Typed])
resolveTyFunDeclKind reason name arguments kindsig equations = do
  (k, eqs, arguments) <- solveK $ do
    return_kind <- case kindsig of
      Nothing -> freshTV
      Just t -> expandType =<< checkKind t TyType

    (args, argts) <- fmap unzip . for arguments $ \case
      TyAnnArg v t -> do
        t <- expandType =<< checkKind t TyType
        pure (TyAnnArg v t, t)
      TyVarArg v -> do
        t <- freshTV
        pure (TyAnnArg v t, t)

    let initk (TyAnnArg v k:xs) = TyPi (Invisible v (Just k) Req) <$> initk xs
        initk (_:_) = undefined
        initk [] = pure return_kind
        fromArgs (~(TyAnnArg v t):xs) = one v t <> fromArgs xs
        fromArgs [] = mempty
    kind <- initk args

    eqs <- local (names %~ focus (one name kind <> fromArgs args)) $
      for equations $ \clause@(TyFunClause ty@(TyApps (TyCon con) xs) rhs an) -> do
        tvs <- for (Set.toList (ftv ty)) $ \v -> (,) v <$> freshTV
        local (names %~ focus (teleFromList tvs)) $ do
          put (BecauseOf clause)

          ty <- TyApps (TyCon con) <$> traverse (uncurry checkKind) (zip xs argts)

          skols <- for (Set.toList (ftv ty)) $ \v ->
            (,) v <$> freshSkol (ByTyFunLhs ty an) ty v

          let sk (_, t) ~(_, TySkol s) = (s ^. skolIdent, t)

          traceM KcC (shown (zipWith sk tvs skols))
          traceM KcC (shown skols)
          traceM KcC (displayType ty)

          rhs <-
            local (names %~ focus (teleFromList (zipWith sk tvs skols))) $
              checkKind (apply (raiseT id <$> Map.fromList skols) rhs) return_kind

          let our_skols =
                Set.fromList $ map (\(_, TySkol s) -> s ^. skolIdent) $ skols

          put reason
          pure (TyFunClause ty (unskolemise' our_skols rhs) (an, kind))

    pure (kind, eqs, args)

  let k' = undependentify k
  k' <- skolCheck name reason k'
  pure (k', eqs, arguments)
 where
   solveK k = do
     (((kind, equations, args), _), cs) <- runWriterT (runStateT k reason)
     (sub, _, cons) <- solveFixpoint reason cs =<< getSolveInfo
     unless (null cons) $ do
       tell (Seq.fromList cons)
     pure ( apply sub kind
          , map (\(TyFunClause lhs rhs (ann, kind)) -> TyFunClause (apply sub lhs) (apply sub rhs) (ann, apply sub kind)) equations
          , map (apply_arg sub) args
          )
   apply_arg sub (TyAnnArg v t) = TyAnnArg v (apply sub t)
   apply_arg _ _ = undefined

   undependentify (TyPi (Invisible v (Just k) Req) rest)
     | v `Set.notMember` ftv rest = TyArr k (undependentify rest)
     | otherwise = TyPi (Invisible v (Just k) Req) (undependentify rest)
   undependentify t = t

   unskolemise m (TySkol v)
     | (v ^. skolIdent) `Set.member` m = TyVar (v ^. skolVar)
     | otherwise = TySkol v
   unskolemise _ t = t
   unskolemise' m = transformType (unskolemise m)

resolveClassKind :: MonadKind m
                 => Toplevel Desugared
                 -> m (Type Typed, [TyConArg Typed])
resolveClassKind stmt@(Class classcon _ ctx args _ methods _) = do
  let reason = BecauseOf stmt
  k <- solveForKind reason $ do
    (kind, tele) <- initialKind LotsaDep tyConstraint args
    let scope = one classcon kind <> tele
        replaceK (TyPi b t) k = TyPi b (replaceK t k)
        replaceK _ k = k

        forTys (m:ms) k = do
          t <- k m
          local (names %~ focus t) $ do
            t' <- forTys ms k
            pure (t <> t')
        forTys [] _ = pure mempty

    local (names %~ focus scope) $ do
      tys <- forTys methods $ \case
        AssocType v _ ty _ -> do
          ty <- checkKind ty TyType
          pure (one v (replaceK kind ty))
        _ -> pure mempty
      local (names %~ focus tys) $ do
        traverse_ (`checkKind` tyConstraint) ctx
        for_ methods $ \case
          m@(MethodSig _ ty _) -> do
            put (BecauseOf m)
            _ <- retcons (addBlame (BecauseOf m)) $
              checkKind ty TyType
            put reason
          _ -> pure ()
    expandType kind
  let remake (TyVarArg v:as) (TyArr k r) = TyAnnArg v k:remake as r
      remake (TyAnnArg v _:as) (TyArr k r) = TyAnnArg v k:remake as r
      remake (TyVarArg v:as) (TyPi (Invisible _ (Just k) Req) r) = TyAnnArg v k:remake as r
      remake (TyAnnArg v _:as) (TyPi (Invisible _ (Just k) Req) r) = TyAnnArg v k:remake as r
      remake cs (TyPi Invisible{} x) = remake cs x
      remake _ _ = []
  pure (k, remake args k)
resolveClassKind _ = error "not a class"

resolveTySymDeclKind :: MonadKind m
                  => SomeReason
                  -> Var Desugared -> [TyConArg Desugared]
                  -> Type Desugared
                  -> m (Type Typed, Type Typed, [TyConArg Typed])
resolveTySymDeclKind reason _ args expansion = do
  (expansion, k) <- solveForKind2 reason $ do
    (kind, tele) <- initialKind NoDep TyType args
    let replace x (TyArr t r) = TyArr t (replace x r)
        replace x _ = x

    local (names %~ focus tele) $ do
      (expansion, ret) <- inferKind expansion
      pure (expansion, replace ret kind)

  let remake (TyVarArg v:as) (TyArr k r) = TyAnnArg v k:remake as r
      remake (TyAnnArg v _:as) (TyArr k r) = TyAnnArg v k:remake as r
      remake as (TyForall _ _ k) = remake as k
      remake _ _ = []

  pure (k, expansion, remake args k)

resolveTyDeclKind :: MonadKind m
                  => SomeReason
                  -> Var Desugared -> [TyConArg Desugared]
                  -> [Constructor Desugared]
                  -> m (Type Typed, Type Typed, [TyConArg Typed])
resolveTyDeclKind reason tycon args cons = do
  let argTvName (TyVarArg v)   = Just v
      argTvName (TyAnnArg v _) = Just v
      vs = mapMaybe argTvName args
  k <- solveForKind reason $ do
    (kind, tele) <- initialKind NoDep TyType args
    let scope = one tycon kind <> tele

    local (names %~ focus scope) $ do
      for_ cons $ \case
        UnitCon{} -> pure ()
        c@(ArgCon _ _ t _) -> () <$ retcons (addBlame (BecauseOf c)) (checkKind t TyType)
        c@(GadtCon _ _ t _) -> condemn $
          retcons (addBlame (BecauseOf c)) (inferGadtConKind c t tycon (mapMaybe argTvName args))
      pure kind
  let remake (TyVarArg v:as) (TyArr k r) = TyAnnArg v k:remake as r
      remake (TyAnnArg v _:as) (TyArr k r) = TyAnnArg v k:remake as r
      remake as (TyForall _ _ k) = remake as k
      remake _ _ = []
  pure (k, foldl TyApp (TyCon tycon) (map TyVar vs), remake args k)

solveForKind :: MonadKind m => SomeReason -> KindT m (Type Typed) -> m (Type Typed)
solveForKind reason = solveK (closeOver reason) reason

solveForKind2 :: MonadKind m => SomeReason -> KindT m (Type Typed, Kind Typed) -> m (Type Typed, Kind Typed)
solveForKind2 reason = solveK cont reason where
  solveK cont reason k = do
    (((typ_e, kind), _), cs) <- runWriterT (runStateT k reason)
    (sub, _, cons) <- solveFixpoint reason cs =<< getSolveInfo
    unless (null cons) $ do
      tell (Seq.fromList cons)
    cont (apply sub typ_e, apply sub kind)

  cont (t, k) = (,) t <$> closeOver reason k

solveK :: MonadKind m => (Type Typed -> m (Type Typed)) -> SomeReason -> KindT m (Type Typed) -> m (Type Typed)
solveK cont reason k = do
  ((kind, _), cs) <- runWriterT (runStateT k reason)
  (sub, _, cons) <- solveFixpoint reason cs =<< getSolveInfo
  unless (null cons) $ do
    tell (Seq.fromList cons)
  cont =<< expandType (apply sub kind)

inferKind :: MonadKind m => Type Desugared -> KindT m (Type Typed, Kind Typed)
inferKind p | trace KcI (pretty p) False = undefined
inferKind (TyCon v) = do
  info <- view (tySyms . at v)
  case info of
    Just info -> do
      reason <- get
      unless (null (info ^. tsArgs)) $
        confesses (UnsaturatedTS reason info 0)
    Nothing -> pure ()

  x <- view (names . at v)
  case x of
    Nothing -> do
      x <- get
      confesses (ArisingFrom (NotInScope v) x)
    Just k -> do
      (_, _, k) <- instantiate Strong Expression k
      pure (TyCon v, k)

inferKind (TyLit l) = pure (TyLit l, litTy l)

inferKind (TyPromotedCon v) = do
  x <- view (names . at v)
  case x of
    Nothing -> confesses (NotInScope v)
    Just k -> do
      (_, _, k) <- instantiate Strong Expression k
      case promoteOrError k of
        Nothing -> pure (TyPromotedCon v, k)
        Just err -> confesses (NotPromotable v k err)

inferKind (TyOperator left op right) = do
  reason <- get
  kind <- view (names . at op)
  ty <- case kind of
    Nothing -> confesses (NotInScope op)
    Just k ->
      view _3 <$> instantiate Strong Expression k

  ~(Anon lt, c1, _) <- quantifier reason (/= Req) ty
  ~(Anon rt, c2, _) <- quantifier reason (/= Req) c1
  left <- checkKind left lt
  right <- checkKind right rt
  pure (TyOperator left op right, c2)

inferKind (TyVar v) = do
  k <- maybe freshTV pure =<< view (names . at v)
  pure (TyVar v, k)

inferKind (TyWildcard (Just v)) = do
  (v, k) <- inferKind v
  pure (TyWildcard (Just v), k)

inferKind (TyWildcard Nothing) = do
  v <- freshTV
  k <- freshTV
  pure (TyWildcard (Just v), k)

inferKind (TySkol sk) = do
  k <- maybe freshTV pure =<< view (names . at (sk ^. skolIdent))
  pure (raiseT id (TySkol sk), k)

inferKind t@TyApp{} | TyCon v:xs <- appsView t = do
  info <- view (tySyms . at v)
  reason <- get
  scope <- view classes
  ki <-
    case info of
      Just info -> do
        unless (length (info ^. tsArgs) <= length xs) $
          confesses (UnsaturatedTS reason info (length xs))
        pure (info ^. tsKind)
      Nothing -> snd <$> inferKind (TyCon v)

  reason <- get
  let checkOne arg kind = do
        (dom, cod, _) <- quantifier reason (/= Req) kind
        case dom of
          Anon d -> do
            arg <- checkKind arg d
            pure (arg, cod)
          Invisible v k Req -> do
            arg <- checkKind arg =<< fromMaybe freshTV (pure <$> k)
            pure (arg, apply (Map.singleton v arg) cod)
          Invisible{} -> error "inferKind TyApp: visible argument to implicit quantifier"
          Implicit{} -> error "inferKind TyApp: visible argument to implicit quantifier"
      checkSpine fun (arg:args) kind = do
        (arg, kind) <- checkOne arg kind
        (_3 %~ (arg:)) <$> checkSpine (TyApp fun arg) args kind
      checkSpine fun [] k = pure (fun, k, [])

  (fun, result, _) <- checkSpine (TyCon v) xs ki

  case info of
    Just tau | Just (Just t) <- tau ^? tsConstraint -> do
      let Just sub = unifyPure_v (zip args xs)
          TyApps _ xs = fun
          TyApps _ args = t
      var <- genName
      tell (Seq.singleton (ConImplicit reason scope var (apply sub t)))
    _ -> pure ()

  traceM KcI (pretty fun <+> soperator (char '↑') <+> pretty result)
  pure (fun, result)

inferKind (TyApp f x) = do
  reason <- get
  (f, (dom, c, _)) <- secondA (quantifier reason (/= Req)) =<< inferKind f

  case dom of
    Anon d -> do
      x <- checkKind x d
      pure (TyApp f x, c)
    Invisible{} -> error "inferKind TyApp: visible argument to implicit quantifier"
    Implicit{} -> error "inferKind TyApp: visible argument to implicit quantifier"

inferKind (TyTupleL a b) = do
  (a, k_a) <- inferKind a
  (b, k_b) <- inferKind b
  pure (TyTupleL a b, TyTuple k_a k_b)

inferKind (TyRows p rs) = do
  (p, k) <- secondA isType =<< inferKind p
  rs <- for rs $ \(row, ty) -> do
    ty <- checkKind ty k
    pure (row, ty)
  pure (TyRows p rs, k)

inferKind TyType = pure (TyType, TyType)
inferKind (TyWithConstraints cs a) = do
  cs <- for cs $ \(a, b) -> do
    (a, k) <- inferKind a
    b <- checkKind b k
    pure (a, b)
  (a, k) <- inferKind a
  pure (TyWithConstraints cs a, k)

inferKind t = do
  x <- freshTV
  t <- checkKind t x
  pure (t, x)

checkKind :: MonadKind m
          => Type Desugared -> Kind Typed -> KindT m (Type Typed)

checkKind t e | trace KcI (pretty t <+> soperator (char '↓') <+> pretty e) False = undefined
checkKind (TyExactRows rs) k = do
  rs <- for rs $ \(row, ty) -> do
    ty <- checkKind ty k
    pure (row, ty)
  pure (TyExactRows rs)

checkKind (TyTuple a b) ek =
  TyTuple <$> checkKind a ek <*> checkKind b ek

checkKind (TyPi binder b) ek = do
  reason <- get
  -- _ <- isType ek
  case binder of
    Anon t -> do
      _ <- unify reason ek TyType
      TyArr <$> checkKind t TyType <*> checkKind b TyType
    Implicit t -> do
      t' <- checkKind t tyConstraint
      v <- genName
      local (classes %~ insert (annotation reason) LocalAssum v t' (MagicInfo [] Nothing)) $
        TyPi (Implicit t') <$> checkKind b ek

    Invisible v (Just arg) r -> do
      (arg, kind) <- inferKind arg
      _ <- subsumes reason kind ek
      b <- local (names %~ focus (one v arg)) $
        checkKind b ek
      let bind = Invisible v (Just arg) r
      pure $ TyPi bind b

    Invisible v Nothing r -> do
      x <- freshTV
      b <- local (names %~ focus (one v x)) $
        checkKind b ek
      let bind = Invisible v (Just x) r
      pure $ TyPi bind b

checkKind ty u = do
  reason <- get
  (t, k) <- inferKind ty
  _ <- subsumes reason k u
  pure t

inferGadtConKind :: MonadKind m
                 => Constructor Desugared
                 -> Type Desugared
                 -> Var Desugared
                 -> [Var Typed]
                 -> KindT m ()
inferGadtConKind con typ tycon args = go typ (gadtConResult typ) where
  go ty (TyApps (TyCon hd) apps)
    | hd == tycon =
      let fv = toList (foldMap ftv apps)
       in do
         fresh <- replicateM (length fv) freshTV
         local (names %~ focus (teleFromList (zip fv fresh))) $ do
           _ <- checkKind ty TyType
           for_ (zip args apps) $ \(var, arg) -> do
             (_, k) <- inferKind arg
             checkKind (TyVar var) =<< expandType k
  go _ _ = do
    tp <- checkKind typ TyType
    confesses . flip ArisingFrom (BecauseOf con) $ gadtConShape
      (tp, foldl TyApp (TyCon tycon) (map TyVar args))
      (gadtConResult tp)
      (Malformed tp)

isType :: MonadKind m => Kind Typed -> KindT m (Kind Typed)
isType t = do
  blame <- get
  _ <- unify blame t TyType
  pure t

closeOver :: MonadKind m => SomeReason -> Type Typed -> m (Type Typed)
closeOver r a = silence $ do
  names <- view names
  let freevars = ftv a
  let forall :: [Var Typed] -> Type Typed -> Type Typed
      forall [] a = a
      forall vs a = foldr addForall a vs

      addForall v t
        | v `inScope` names = TyPi (Invisible v (Just (names ^. at v . non undefined)) Infer) t
        | otherwise = TyPi (Invisible v Nothing Infer) t

      tyForall v k = TyPi (Invisible v k Infer)

  let kindVars = squish . second toList . runWriter . split where
        squish (x, []) = x
        squish (x, vs) = foldr (flip tyForall (Just TyType)) x vs

        split (TyForall v (Just t@(TyVar x)) ty)
          | v `Set.member` freevars = do
            tell (Set.singleton x)
            tyForall v (Just t) <$> split ty
          | otherwise = tyForall v (Just t) <$> split ty
        split t = pure t
  kindVars . killWildcard <$> annotateKind r (forall (toList freevars) a)

closeOver' :: MonadKind m => Set.Set (Var Typed) -> SomeReason -> Type Typed -> m (Type Typed)
closeOver' vars r a = do
  names <- view names
  let freevars = ftv a

      forall :: [Var Typed] -> Type Typed -> Type Typed
      forall [] a = a
      forall vs a = foldr addForall a vs

      addForall v t
        | v `inScope` names = TyPi (Invisible v (Just (names ^. at v . non undefined)) (vis v)) t
        | otherwise = TyPi (Invisible v Nothing (vis v)) t

      vis v
        | v `Set.member` vars = Spec
        | otherwise = Infer

      kindVars = squish . second toList . runWriter . split where
      squish (x, []) = x
      squish (x, vs) = foldr (flip tyForall (Just TyType)) x vs

      split (TyForall v (Just t@(TyVar x)) ty)
        | v `Set.member` freevars = do
          tell (Set.singleton x)
          tyForall v (Just t) <$> split ty
        | otherwise = tyForall v (Just t) <$> split ty
      split t = pure t

      tyForall t k b = TyPi (Invisible t k (vis t)) b
  kindVars . killWildcard <$> annotateKind r (forall (toList freevars) a)


promoteOrError :: Type Typed -> Maybe Doc
promoteOrError TyWithConstraints{} = Just (string "mentions constraints")
promoteOrError TyTuple{} = Nothing
promoteOrError TyRows{} = Just (string "mentions a record")
promoteOrError TyExactRows{} = Just (string "mentions a record")
promoteOrError (TyApp a b) = promoteOrError a <|> promoteOrError b
promoteOrError (TyPi (Invisible _ a _) b) = join (traverse promoteOrError a) <|> promoteOrError b
promoteOrError (TyPi (Anon a) b) = promoteOrError a <|> promoteOrError b
promoteOrError (TyPi Implicit{} _) = Just (string "has implicit parameters")
promoteOrError TyCon{} = Nothing
promoteOrError TyVar{} = Nothing
promoteOrError TySkol{} = Nothing
promoteOrError TyPromotedCon{} = Nothing
promoteOrError TyType{} = Nothing
promoteOrError TyWildcard{} = Nothing
promoteOrError TyLit{} = Nothing
promoteOrError TyTupleL{} = Nothing
promoteOrError (TyParens p) = promoteOrError p
promoteOrError (TyOperator l _ r) = promoteOrError l <|> promoteOrError r

generalise :: MonadInfer Typed m
           => (Type Typed -> Set.Set (Var Typed)) -> SomeReason -> Type Typed -> m (Type Typed)
generalise ftv r ty =
  let fv = ftv ty in do
    env <- view typeVars
    case Set.toList (fv `Set.difference` env) of
      [] -> pure ty
      vs -> annotateKind r $ foldr (\v rest -> TyPi (Invisible v Nothing Spec) rest) (killWildcard ty) vs

data Dep = NoDep | LotsaDep
