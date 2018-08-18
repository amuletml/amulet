{-# LANGUAGE ConstraintKinds, FlexibleContexts, LambdaCase #-}
module Types.Kinds
  ( resolveKind
  , resolveTyDeclKind
  , annotateKind
  , closeOver
  , checkAgainstKind, getKind, liftType
  , generalise
  )
  where

import Control.Monad.State.Strict
import Control.Monad.Infer
import Control.Applicative
import Control.Arrow
import Control.Lens

import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Traversable
import Data.Foldable
import Data.Triple
import Data.Maybe

import Types.Wellformed (wellformed)
import Types.Infer.Builtin
import Types.Infer.Errors
import Types.Unify (solve)

import Syntax.Subst
import Syntax.Types
import Syntax.Raise
import Syntax.Var
import Syntax

import Text.Pretty.Semantic

type KindT m = StateT SomeReason (WriterT (Seq.Seq (Constraint Typed)) m)

type MonadKind m =
  ( MonadError TypeError m
  , MonadReader Env m
  , MonadNamey m
  )

type Kind = Type

resolveKind :: MonadKind m => SomeReason -> Type Resolved -> m (Type Typed)
resolveKind reason otp = do
  ((ty, _), cs) <- let cont t = do
                        (t, _) <- secondA isType =<< inferKind t
                        pure t
                     in runWriterT (runStateT (cont otp) reason)
  x <- genName

  sub <- case solve x cs of
    Left e -> throwError e
    Right (x, _) -> pure x

  let t = apply sub ty
  wellformed t
  pure t

getKind :: MonadKind m => SomeReason -> Type Resolved -> m (Kind Typed)
getKind r t = solveK pure r (snd <$> inferKind t)

liftType :: MonadKind m => SomeReason -> Type Resolved -> m (Type Typed)
liftType r t = solveK pure r (fst <$> inferKind t)

checkAgainstKind :: MonadInfer Typed m => SomeReason -> Type Resolved -> Type Typed -> m (Type Typed)
checkAgainstKind r t k = do
  ((k, _), x) <- runWriterT (runStateT (checkKind t k) r)
  tell x
  pure k

annotateKind :: MonadKind m => SomeReason -> Type Typed -> m (Type Typed)
annotateKind r ty = do
  ((ty, _), cs) <- runWriterT (runStateT (checkKind (raiseT unTvName ty) TyType) r)
  x <- genName

  sub <- case solve x cs of
    Left e -> throwError e
    Right (x, _) -> pure x

  pure (apply sub ty)

initialKind :: MonadKind m => [TyConArg Resolved] -> KindT m (Type Typed, Telescope Typed)
initialKind (TyVarArg v:as) = do
  (k, t) <- initialKind as
  ty <- freshTV
  pure (TyArr ty k, one v ty <> t)
initialKind (TyAnnArg v k:as) = do
  k <- checkKind k TyType
  (s, t) <- initialKind as
  pure (TyArr k s, t <> one v k)
initialKind [] = pure (TyType, mempty)

resolveTyDeclKind :: MonadKind m
                  => SomeReason
                  -> Var Resolved -> [TyConArg Resolved]
                  -> [Constructor Resolved]
                  -> m (Type Typed, Type Typed, [TyConArg Typed])
resolveTyDeclKind reason tycon args cons = do
  let argTvName (TyVarArg v)   = Just (TvName v)
      argTvName (TyAnnArg v _) = Just (TvName v)
      vs = mapMaybe argTvName args
  k <- solveForKind reason $ do
    (kind, tele) <- initialKind args
    let scope = one tycon kind <> tele

    local (names %~ focus scope) $ do
      for_ cons $ \case
        UnitCon{} -> pure ()
        c@(ArgCon _ t _) -> () <$ checkKind t TyType
                                `catchError` \x -> throwError (propagateBlame (BecauseOf c) x)
        c@(GeneralisedCon _ t _) -> inferGadtConKind c t tycon (mapMaybe argTvName args)
                                      `catchError` \x -> throwError (propagateBlame (BecauseOf c) x)
      pure kind
  let remake (TyVarArg v:as) (TyArr _ k) = TyVarArg (TvName v):remake as k
      remake (TyAnnArg v _:as) (TyArr k _) = TyAnnArg (TvName v) k:remake as k
      remake _ _ = []
  pure (k, foldl TyApp (TyCon (TvName tycon)) (map TyVar vs), remake args k)

solveForKind :: MonadKind m => SomeReason -> KindT m (Type Typed) -> m (Type Typed)
solveForKind reason = solveK (closeOver reason) reason

solveK :: MonadKind m => (Type Typed -> m (Type Typed)) -> SomeReason -> KindT m (Type Typed) -> m (Type Typed)
solveK cont reason k = do
  ((kind, _), cs) <- runWriterT (runStateT k reason)
  x <- genName

  case solve x cs of
    Left e -> throwError e
    Right (x, _) -> cont (apply x kind)

inferKind :: MonadKind m => Type Resolved -> KindT m (Type Typed, Kind Typed)
inferKind (TyCon v) = do
  x <- view (names . at v)
  case x of
    Nothing -> throwError (NotInScope v)
    Just k -> do
      (_, _, k) <- instantiate Expression k
      pure (TyCon (TvName v), k)

inferKind (TyPromotedCon v) = do
  x <- view (names . at v)
  case x of
    Nothing -> throwError (NotInScope v)
    Just k -> do
      (_, _, k) <- instantiate Expression k
      case promoteOrError k of
        Nothing -> pure (TyPromotedCon (TvName v), k)
        Just err -> throwError (NotPromotable (TvName v) k err)

inferKind (TyVar v) = do
  k <- maybe freshTV pure =<< view (names . at v)
  pure (TyVar (TvName v), k)

inferKind (TyWildcard (Just v)) = do
  (v, k) <- inferKind v
  pure (TyWildcard (Just v), k)

inferKind (TyWildcard Nothing) = do
  v <- freshTV
  k <- freshTV
  pure (TyWildcard (Just v), k)

inferKind (TySkol sk) = do
  k <- maybe freshTV pure =<< view (names . at (sk ^. skolIdent))
  pure (raiseT TvName (TySkol sk), k)

inferKind (TyApp f x) = do
  reason <- get
  (f, (dom, c, _)) <- secondA (quantifier reason Don'tSkip) =<< inferKind f

  case dom of
    Anon d -> do
      x <- checkKind x d
      pure (TyApp f x, c)
    Invisible{} -> error "inferKind TyApp: visible argument to implicit quantifier"
    Implicit{} -> error "inferKind TyApp: visible argument to implicit quantifier"

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
          => Type Resolved -> Kind Typed -> KindT m (Type Typed)
checkKind (TyExactRows rs) k = do
  rs <- for rs $ \(row, ty) -> do
    ty <- checkKind ty k
    pure (row, ty)
  pure (TyExactRows rs)

checkKind (TyTuple a b) (TyTuple ak bk) =
  TyTuple <$> checkKind a ak <*> checkKind b bk

checkKind (TyTuple a b) ek =
  TyTuple <$> checkKind a ek <*> checkKind b ek

checkKind (TyPi binder b) ek = do
  reason <- get
  -- _ <- isType ek
  case binder of
    Anon t -> TyArr <$> checkKind t ek <*> checkKind b ek
    Implicit t -> do
      t <- checkKind t ek
      TyPi (Implicit t) <$> checkKind b ek

    Invisible v (Just arg) -> do
      (arg, kind) <- inferKind arg
      _ <- subsumes reason ek kind
      b <- local (names %~ focus (one v arg)) $
        checkKind b ek
      let bind = Invisible (TvName v) (Just arg)
      pure $ TyPi bind b

    Invisible v Nothing -> do
      x <- freshTV
      b <- local (names %~ focus (one v x)) $
        checkKind b ek
      let bind = Invisible (TvName v) (Just x)
      pure $ TyPi bind b

checkKind ty u = do
  reason <- get
  (t, k) <- inferKind ty
  _ <- subsumes reason u k
  pure t

inferGadtConKind :: MonadKind m
                 => Constructor Resolved
                 -> Type Resolved
                 -> Var Resolved
                 -> [Var Typed]
                 -> KindT m ()
inferGadtConKind con typ tycon args = go typ (reverse (spine (gadtConResult typ))) where
  spine :: Type Resolved -> [Type Resolved]
  spine (TyApp f x) = x:spine f
  spine x = [x]

  go ty (hd:apps)
    | TyCon hd <- hd, hd == tycon =
      let fv = map TvName $ toList (foldMap ftv apps)
       in do
         fresh <- replicateM (length fv) freshTV
         local (names %~ focus (teleFromList (zip fv fresh))) $ do
           _ <- checkKind ty TyType
           for_ (zip args apps) $ \(var, arg) -> do
             (_, k) <- inferKind arg
             checkKind (TyVar (unTvName var)) k
  go _ _ = do
    tp <- checkKind typ TyType
    throwError . flip ArisingFrom (BecauseOf con) $ gadtConShape
      (tp, foldl TyApp (TyCon (TvName tycon)) (map TyVar args))
      (gadtConResult tp)
      (Malformed tp)

isType :: MonadKind m => Kind Typed -> KindT m (Kind Typed)
isType t = do
  blame <- get
  _ <- unify blame TyType t
  pure t

closeOver :: MonadKind m => SomeReason -> Type Typed -> m (Type Typed)
closeOver r a = kindVars . killWildcard <$> annotateKind r (forall (toList freevars) a) where
  freevars = ftv a
  forall :: [Var p] -> Type p -> Type p
  forall [] a = a
  forall vs a = foldr (flip TyForall Nothing) a vs

  kindVars = squish . second toList . runWriter . split where
    squish (x, []) = x
    squish (x, vs) = foldr (flip TyForall (Just TyType)) x vs

    split (TyForall v (Just t@(TyVar x)) ty)
      | v `Set.member` freevars = do
        tell (Set.singleton x)
        TyForall v (Just t) <$> split ty
      | otherwise = TyForall v (Just t) <$> split ty
    split t = pure t


promoteOrError :: Type Typed -> Maybe Doc
promoteOrError TyWithConstraints{} = Just (string "mentions constraints")
promoteOrError TyTuple{} = Nothing
promoteOrError TyRows{} = Just (string "mentions a tuple")
promoteOrError TyExactRows{} = Just (string "mentions a tuple")
promoteOrError (TyApp a b) = promoteOrError a <|> promoteOrError b
promoteOrError (TyPi (Invisible _ a) b) = join (traverse promoteOrError a) <|> promoteOrError b
promoteOrError (TyPi (Implicit a) b) = promoteOrError a <|> promoteOrError b
promoteOrError (TyPi (Anon a) b) = promoteOrError a <|> promoteOrError b
promoteOrError TyCon{} = Nothing
promoteOrError TyVar{} = Nothing
promoteOrError TySkol{} = Nothing
promoteOrError TyPromotedCon{} = Nothing
promoteOrError TyType{} = Nothing
promoteOrError TyWildcard{} = Nothing


generalise :: MonadInfer Typed m => (Type Typed -> Set.Set (Var Typed)) -> SomeReason -> Type Typed -> m (Type Typed)
generalise ftv r ty =
  let fv = ftv ty in do
    env <- view typeVars
    case Set.toList (fv `Set.difference` env) of
      [] -> pure ty
      vs -> annotateKind r $ foldr (flip TyForall Nothing) (killWildcard ty) vs
