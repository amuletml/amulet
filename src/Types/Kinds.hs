{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}
module Types.Kinds (resolveKind, resolveTyDeclKind) where

import Control.Monad.Infer
import Control.Lens

import qualified Data.Sequence as Seq
import Data.Traversable
import Data.Foldable
import Data.Triple

import Types.Wellformed (wellformed)
import Types.Infer.Builtin
import Types.Infer.Errors
import Types.Unify (solve)

import Syntax.Subst
import Syntax.Raise
import Syntax

import Debug.Trace
import Pretty

type KindT m = WriterT (Seq.Seq (Constraint Typed)) m

type MonadKind m =
  ( MonadError TypeError m
  , MonadReader Env m
  , MonadGen Int m
  )

type Kind = Type

resolveKind :: MonadKind m => Type Resolved -> m (Type Typed)
resolveKind t = do
  wellformed t
  (ty, cs) <- runWriterT (checkKind t (TyUniverse 0))
  x <- gen

  sub <- case solve x cs of
    Left e -> throwError e
    Right (x, _) -> pure x

  pure (apply sub ty)

resolveTyDeclKind :: MonadKind m
                  => Var Resolved -> [Var Resolved]
                  -> [Constructor Resolved]
                  -> m (Type Typed)
resolveTyDeclKind tycon args cons = solveForKind $ do
  ks <- replicateM (length args) freshTV
  let kind = foldr TyArr (TyUniverse 0) ks

  extendManyK ((TvName tycon, kind):zip (map TvName args) ks) $ do
    for_ cons $ \c -> case c of
      UnitCon{} -> pure ()
      ArgCon _ t _ -> () <$ checkKind t (TyUniverse 0)
      GeneralisedCon _ t _ -> inferGadtConKind t tycon (map TvName args)
    pure kind



solveForKind :: ( Substitutable Typed b
                , MonadError TypeError m
                , MonadGen Int m)
             => WriterT (Seq.Seq (Constraint Typed)) m b -> m b
-- this type courtesy of GHC
solveForKind k = do
  (kind, cs) <- runWriterT k
  x <- gen
  for_ cs (flip trace (pure ()) . render . pretty)
  case solve x cs of
    Left e -> throwError e
    Right (x, _) -> pure (apply x kind)

inferKind :: MonadKind m => Type Resolved -> KindT m (Type Typed, Kind Typed)
inferKind (TyCon v) = do
  x <- view (types . at v)
  case x of
    Nothing -> throwError (NotInScope v)
    Just k -> pure (TyCon (TvName v), k)

inferKind (TyVar v) = do
  k <- maybe freshTV pure =<< view (types . at v)
  pure (TyVar (TvName v), k)

inferKind (TySkol sk) = do
  k <- maybe freshTV pure =<< view (types . at (sk ^. skolIdent))
  pure (raiseT TvName (TySkol sk), k)

inferKind ty@(TyApp f x) = do
  (f, (d, c, _)) <- secondA (decompose ty _TyArr) =<< inferKind f
  x <- checkKind x d
  pure (TyApp f x, c)

inferKind ty@(TyRows p rs) = do
  (p, k) <- secondA (isType ty) =<< inferKind p
  rs <- for rs $ \(row, ty) -> do
    ty <- checkKind ty k
    pure (row, ty)
  pure (TyRows p rs, k)

inferKind ty@(TyTuple a b) = do
  (a, k) <- secondA (isType ty) =<< inferKind a
  b <- checkKind b k
  pure (TyTuple a b, k)

inferKind (TyUniverse x) = pure (TyUniverse x, TyUniverse (x + 1))
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

checkKind pitype@(TyPi a b) ek = do
  _ <- isType pitype ek
  case a of
    Anon t -> do
      (a, ik) <- inferKind t
      b <- checkKind b ek
      _ <- subsumes pitype ik ek -- ik <= ek
      pure $ TyArr a b
    Implicit v -> do
      x <- freshTV
      b <- extendKind (TvName v, x) $
        checkKind b ek
      let bind = Implicit (TvName v)
      pure $ TyPi bind b

checkKind ty u = do
  (t, k) <- inferKind ty
  _ <- subsumes ty k u -- k <= u
  pure t

inferGadtConKind :: MonadKind m
                 => Type Resolved
                 -> Var Resolved
                 -> [Var Typed]
                 -> KindT m ()
inferGadtConKind typ tycon args = inferKind typ *> go (reverse (spine (gadtConResult typ))) where
  spine :: Type Resolved -> [Type Resolved]
  spine (TyApp f x) = x:spine f
  spine x = [x]

  go (hd:apps)
    | TyCon hd <- hd, hd == tycon =
      let fv = map TvName $ toList (foldMap ftv apps)
       in do
         fresh <- replicateM (length fv) freshTV
         extendManyK (zip fv fresh) $ do
           for_ (zip args apps) $ \(var, arg) -> do
             (ty, k) <- inferKind arg
             trace (render (pretty ty <+> colon <+> pretty k)) pure ()
             checkKind (TyVar (unTvName var)) k
           pure ()
  go _ = do
    (tp, _) <- inferKind typ
    throwError $ gadtConShape
      (tp, foldl TyApp (TyCon (TvName tycon)) (map TyVar args))
      (gadtConResult tp)
      (Malformed tp)

isType :: MonadKind m => Type Resolved -> Kind Typed -> KindT m (Kind Typed)
isType blame t = do
  _ <- subsumes blame (TyUniverse 0) t
  pure t
