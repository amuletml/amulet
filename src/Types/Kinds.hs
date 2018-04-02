{-# LANGUAGE ConstraintKinds, FlexibleContexts, LambdaCase #-}
module Types.Kinds (resolveKind, resolveTyDeclKind, annotateKind, closeOver) where

import Control.Monad.State.Strict
import Control.Monad.Infer
import Control.Applicative
import Control.Lens

import qualified Data.Sequence as Seq
import Data.Traversable
import Data.Foldable
import Data.Triple

import Types.Wellformed (wellformed, normType)
import Types.Infer.Builtin
import Types.Infer.Errors
import Types.Unify (solve)

import Syntax.Subst
import Syntax.Raise
import Syntax

import Pretty

type KindT m = StateT SomeReason (WriterT (Seq.Seq (Constraint Typed)) m)

type MonadKind m =
  ( MonadError TypeError m
  , MonadReader Env m
  , MonadGen Int m
  )

type Kind = Type

resolveKind :: MonadKind m => SomeReason -> Type Resolved -> m (Type Typed)
resolveKind reason otp = do
  wellformed otp
  ((ty, _), cs) <- let cont t = do
                        (t, _) <- secondA isType =<< inferKind t
                        pure t
                     in runWriterT (runStateT (cont otp) reason)
  x <- gen

  sub <- case solve x cs of
    Left e -> throwError e
    Right (x, _) -> pure x

  pure (apply sub ty)

annotateKind :: MonadKind m => SomeReason -> Type Typed -> m (Type Typed)
annotateKind r ty = do
  ((ty, _), cs) <- runWriterT (runStateT (fst <$> inferKind (raiseT unTvName ty)) r)
  x <- gen

  sub <- case solve x cs of
    Left e -> throwError e
    Right (x, _) -> pure x

  pure (apply sub ty)

resolveTyDeclKind :: MonadKind m
                  => SomeReason
                  -> Var Resolved -> [Var Resolved]
                  -> [Constructor Resolved]
                  -> m (Type Typed)
resolveTyDeclKind reason tycon args cons = solveForKind reason $ do
  ks <- replicateM (length args) freshTV
  let kind = foldr TyArr TyType ks

  extendManyK ((TvName tycon, kind):zip (map TvName args) ks) $ do
    for_ cons $ \case
      UnitCon{} -> pure ()
      ArgCon _ t _ -> () <$ checkKind t TyType
      GeneralisedCon _ t _ -> inferGadtConKind t tycon (map TvName args)
    pure kind

solveForKind :: MonadKind m => SomeReason -> KindT m (Type Typed) -> m (Type Typed)
solveForKind reason k = do
  ((kind, _), cs) <- runWriterT (runStateT k reason)
  x <- gen

  case solve x cs of
    Left e -> throwError e
    Right (x, _) -> closeOver reason (apply x kind)

inferKind :: MonadKind m => Type Resolved -> KindT m (Type Typed, Kind Typed)
inferKind (TyCon v) = do
  x <- view (types . at v)
  case x of
    Nothing -> throwError (NotInScope v)
    Just k -> pure (TyCon (TvName v), k)

inferKind (TyPromotedCon v) = do
  x <- view (values . at v)
  case x of
    Nothing -> throwError (NotInScope v)
    Just k -> case promoteOrError k of
      Nothing -> pure (TyPromotedCon (TvName v), k)
      Just err -> throwError (NotPromotable (TvName v) k err)

inferKind (TyVar v) = do
  k <- maybe freshTV pure =<< view (types . at v)
  pure (TyVar (TvName v), k)

inferKind (TySkol sk) = do
  k <- maybe freshTV pure =<< view (types . at (sk ^. skolIdent))
  pure (raiseT TvName (TySkol sk), k)

inferKind (TyApp f x) = do
  reason <- get
  (f, (d, c, _)) <- secondA (decompose (Const reason) _TyArr) =<< inferKind f
  x <- checkKind x d
  pure (TyApp f x, c)

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

checkKind (TyTuple a b) ek = do
  TyTuple <$> checkKind a ek <*> checkKind b ek

checkKind (TyPi binder b) ek = do
  reason <- get
  -- _ <- isType ek
  case binder of
    Anon t -> TyArr <$> checkKind t ek <*> checkKind b ek
    Implicit v (Just arg) -> do
      (arg, kind) <- inferKind arg
      _ <- subsumes (Const reason) ek kind
      b <- extendKind (TvName v, arg) $
        checkKind b ek
      let bind = Implicit (TvName v) (Just arg)
      pure $ TyPi bind b
    Implicit v Nothing -> do
      x <- freshTV
      b <- extendKind (TvName v, x) $
        checkKind b ek
      let bind = Implicit (TvName v) (Just x)
      pure $ TyPi bind b

checkKind ty u = do
  reason <- get
  (t, k) <- inferKind ty
  _ <- subsumes (Const reason) u k
  pure t

inferGadtConKind :: MonadKind m
                 => Type Resolved
                 -> Var Resolved
                 -> [Var Typed]
                 -> KindT m ()
inferGadtConKind typ tycon args = go typ (reverse (spine (gadtConResult typ))) where
  spine :: Type Resolved -> [Type Resolved]
  spine (TyApp f x) = x:spine f
  spine x = [x]

  go ty (hd:apps)
    | TyCon hd <- hd, hd == tycon =
      let fv = map TvName $ toList (foldMap ftv apps)
       in do
         fresh <- replicateM (length fv) freshTV
         extendManyK (zip fv fresh) $ do
           _ <- checkKind ty TyType
           for_ (zip args apps) $ \(var, arg) -> do
             (_, k) <- inferKind arg
             checkKind (TyVar (unTvName var)) k
           pure ()
  go _ _ = do
    tp <- checkKind typ TyType
    throwError $ gadtConShape
      (tp, foldl TyApp (TyCon (TvName tycon)) (map TyVar args))
      (gadtConResult tp)
      (Malformed tp)

isType :: MonadKind m => Kind Typed -> KindT m (Kind Typed)
isType t = do
  blame <- get
  _ <- unify (Const blame) TyType t
  pure t

closeOver :: MonadKind m => SomeReason -> Type Typed -> m (Type Typed)
closeOver r a = fmap normType . annotateKind r $ forall (fv a) a where
  fv = toList . ftv
  forall :: [Var p] -> Type p -> Type p
  forall [] a = a
  forall vs a = foldr (flip TyForall Nothing) a vs

promoteOrError :: Type Typed -> Maybe Doc
promoteOrError TyWithConstraints{} = Just (string "mentions constraints")
promoteOrError TyTuple{} = Just (string "mentions a tuple")
promoteOrError TyRows{} = Just (string "mentions a tuple")
promoteOrError TyExactRows{} = Just (string "mentions a tuple")
promoteOrError (TyApp a b) = promoteOrError a <|> promoteOrError b
promoteOrError (TyPi (Implicit _ a) b) = join (traverse promoteOrError a) <|> promoteOrError b
promoteOrError (TyPi (Anon a) b) = promoteOrError a <|> promoteOrError b
promoteOrError TyCon{} = Nothing
promoteOrError TyVar{} = Nothing
promoteOrError TySkol{} = Nothing
promoteOrError TyPromotedCon{} = Nothing
promoteOrError TyType{} = Nothing
