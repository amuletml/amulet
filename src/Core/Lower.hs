{-# LANGUAGE FlexibleContexts, ConstraintKinds, OverloadedStrings #-}
{-# LANGUAGE LambdaCase, TupleSections, ExplicitNamespaces, PatternSynonyms #-}
module Core.Lower
  ( lowerExprTerm
  , lowerType
  , lowerPat
  , lowerProg
  , cotyString, cotyInt, cotyBool, cotyUnit, cotyFloat
  ) where

import Control.Monad.Infer
import Control.Monad.Cont
import Control.Arrow

import Types.Infer (tyString, tyInt, tyBool, tyUnit, tyFloat)

import qualified Data.Text as T
import Data.Traversable
import Data.Function
import Data.Foldable
import Data.Graph
import Data.Span
import Data.List

import qualified Core.Core as C
import Core.Core hiding (Atom, Term, Stmt, Type, Pattern)
import Core.Core (pattern Atom)

import qualified Syntax as S
import Syntax.Let
import Syntax (Var(..), Resolved, Typed, Expr(..), Pattern(..), Lit(..), Skolem(..), Toplevel(..), Constructor(..))

import Pretty (pretty)

type Atom = C.Atom (Var Resolved)
type Term = C.Term (Var Resolved)
type Type = C.Type (Var Resolved)
type Pat = C.Pattern (Var Resolved)
type Stmt = C.Stmt (Var Resolved)

type Lower = ContT Term

type MonadLower m
  = ( MonadGen Int m
    , MonadReader Env m )

cotyString, cotyUnit, cotyBool, cotyInt, cotyFloat :: Type
cotyString = lowerType tyString
cotyUnit = lowerType tyUnit
cotyBool = lowerType tyBool
cotyInt = lowerType tyInt
cotyFloat = lowerType tyFloat

errRef :: Atom
errRef = Ref (TgInternal "error")
                (ForallTy (TgInternal "a")
                            (ArrTy cotyString
                                     (VarTy (TgInternal "a"))))

patternMatchingFail :: MonadLower m => Span -> Type -> Type -> m (Pat, Type, Term)
patternMatchingFail w p t = do
  var <- fresh
  tyApp <- fresh
  let err = Lit (Str (T.pack ("Pattern matching failure at " ++ show (pretty w))))
      errTy = ArrTy cotyString t
  pure (C.Capture var p, p, C.Let (One (tyApp, errTy, C.TyApp errRef t))
                             (C.App (C.Ref tyApp errTy) err))

lowerAtAtom :: MonadLower m => Expr Typed -> Type -> Lower m Atom
lowerAtAtom x t = do x' <- lowerAt x t
                     case x' of
                       C.Atom a -> pure a
                       x' -> ContT $ \k ->
                         fresh >>= \v -> C.Let (One (v, t, x')) <$> k (C.Ref v t)

lowerAtTerm :: MonadLower m => Expr Typed -> Type -> m Term
lowerAtTerm x t = runContT (lowerAt x t) pure

lowerExprAtom :: MonadLower m => Expr Typed -> Lower m Atom
lowerExprAtom e = lowerAtAtom e (lowerType (S.getType e))

lowerExprTerm :: MonadLower m => Expr Typed -> m Term
lowerExprTerm e = lowerAtTerm e (lowerType (S.getType e))

lowerBothAtom :: MonadLower m => Expr Typed -> Lower m (Atom, Type)
lowerBothAtom e = let t = lowerType (S.getType e)
                  in (,t) <$> lowerAtAtom e t

lowerBothTerm :: MonadLower m => Expr Typed -> m (Term, Type)
lowerBothTerm e = let t = lowerType (S.getType e)
                  in (,t) <$> lowerAtTerm e t

lowerAt :: MonadLower m => Expr Typed -> Type -> Lower m Term
lowerAt (Ascription e _ _) t = lowerAt e t

lowerAt (VarRef (TvName p) _) ty = pure (Atom (Ref p ty))
lowerAt (TypeApp f x _) _ = flip TyApp (lowerType x) <$> lowerExprAtom f

lowerAt e (ForallTy vs b) = Atom . Lam (TypeArgument vs StarTy) <$> lowerAtTerm e b

lowerAt (S.Let vs t _) ty = do
  let sccs = depOrder vs
      lowerScc (CyclicSCC vs) = CyclicSCC <$> do
        for vs $ \(TvName var, ex, (_, ty)) -> do
          let ty' = lowerType ty
          (var,ty',) <$> lowerAtTerm ex ty'
      lowerScc (AcyclicSCC (TvName var, ex, (_, ty))) = AcyclicSCC <$> do
        let ty' = lowerType ty
        (var, ty',) <$> lowerAtTerm ex ty'
      foldScc (AcyclicSCC v) = C.Let (One v)
      foldScc (CyclicSCC vs) = C.Let (Many vs)
  vs' <- traverse lowerScc sccs
  let k = foldr ((.) . foldScc) id vs'
  k <$> lowerAtTerm t ty -- TODO scc these
lowerAt (S.If c t e _) ty = do
  c' <- lowerAtAtom c cotyBool
  t' <- lowerAtTerm t ty
  e' <- lowerAtTerm e ty
  let tc = (PatLit LitTrue, cotyBool, t')
      te = (PatLit LitFalse, cotyBool, e')
  pure $ C.Match c' [tc, te]
lowerAt (Fun p bd an) (ArrTy a b) =
  let operational (PType p _ _) = operational p
      operational p = p
   in case operational p of
        S.Capture (TvName v) _ -> Atom . Lam (TermArgument v a) <$> lowerAtTerm bd b
        _ -> do
          (p', bd') <- (,) <$> lowerPat p <*> lowerAtTerm bd b
          arg <- fresh
          fail <- patternMatchingFail (fst an) (lowerType (S.getType p)) b
          pure (Atom (Lam (TermArgument arg a) (C.Match (Ref arg a) [ (p', a, bd'), fail ])))
lowerAt (Begin [x] _) t = lowerAt x t
lowerAt (Begin xs _) t = lowerAtTerm (last xs) t >>= flip (foldrM bind) (init xs) where
  bind e r = flip C.Let r . One <$> (build <$> fresh <*> lowerBothTerm e)
  build a (b, c) = (a, c, b)
lowerAt (S.Match ex cs an) ty = do
  (ex', mt) <- lowerBothAtom ex
  cs' <- for cs $ \(pat, ex) ->
    (,mt,) <$> lowerPat pat <*> lowerAtTerm ex ty
  fail <- patternMatchingFail (fst an) (lowerType (S.getType (fst (head cs)))) ty

  pure $ C.Match ex' (cs' ++ [fail])
lowerAt (Access r k _) ty = do
  (r', rt) <- lowerBothAtom r
  (iv, var) <- (,) <$> fresh <*> fresh
  let cotyRows t [] = t
      cotyRows t xs = RowsTy t xs
      inner =
        case rt of
          RowsTy t rs -> cotyRows t (deleteBy ((==) `on` fst) (k, undefined) rs)
          ExactRowsTy rs -> ExactRowsTy (deleteBy ((==) `on` fst) (k, undefined) rs)
          _ -> error ("not a row type " ++ show rt)
      match = ( PatExtend (C.Capture iv inner) [ (k, C.Capture var ty) ]
              , rt, Atom (Ref var ty ))
  pure $ C.Match r' [match]

lowerAt (BinOp left op right a) t = lowerAt (S.App (S.App op left a) right a) t
lowerAt Hole{} _ = error "holes can't be lowered"

lowerAt (Tuple [x] _) t = lowerAt x t
lowerAt (Tuple (x:xs) _) (ExactRowsTy [(_, a), (_, b)]) = do
  x <- lowerAtAtom x a
  xs <- lowerAtAtom (Tuple xs undefined) b
  pure (Extend (Lit RecNil) [("1", a, x), ("2", b, xs)])

lowerAt e _ = lowerAnyway e

lowerAnyway :: MonadLower m => Expr Typed -> Lower m Term
lowerAnyway (S.Record xs _) = case xs of
  [] -> pure (Atom (Lit RecNil))
  xs -> do
    xs' <- traverse (lowerBothAtom . snd) xs
    pure $ Extend (Lit RecNil) (zipWith build xs xs')
  where build (name, _) (atom, ty) = (name, ty, atom)
lowerAnyway (RecordExt e xs _) = do
  e' <- lowerExprAtom e
  xs' <- traverse (lowerBothAtom . snd) xs
  pure $ Extend e' (zipWith build xs xs')
  where build (name, _) (atom, ty) = (name, ty, atom)

lowerAnyway (Literal l _) = pure . Atom . Lit $ lowerLiteral l
lowerAnyway (S.App f x _) = C.App <$> lowerExprAtom f <*> lowerExprAtom x

lowerAnyway (TypeApp f x _) = flip TyApp (lowerType x) <$> lowerExprAtom f
lowerAnyway (S.Cast e c _) =
  let
  co (S.VarCo (TvName x)) = CoercionVar x
  co (S.ReflCo t) = SameRepr (lowerType t) (lowerType t)
  co (S.AssumedCo t t') = SameRepr (lowerType t) (lowerType t')
  co (S.SymCo c) = Symmetry (co c)
  co (S.AppCo a b) = Application (co a) (co b)
  co (S.ArrCo a b) = Arrow (co a) (co b)
  co (S.ProdCo a b) = ExactRecord [("1", co a), ("2", co b)]
  co (S.RowsCo c rs) = C.Record (co c) (map (second co) rs)
  co (S.ExactRowsCo rs) = C.ExactRecord (map (second co) rs)
  co (S.ForallCo (TvName v) rs) = C.Quantified v (co rs)
  in do
    (e, _) <- lowerBothAtom e
    pure (C.Cast e (co c))

lowerAnyway e = error ("can't lower " ++ show e ++ " without type")

lowerLiteral :: Lit -> Literal
lowerLiteral (LiFloat d) = Float d
lowerLiteral (LiInt i) = Int i
lowerLiteral (LiStr t) = Str t
lowerLiteral (LiBool True) = LitTrue
lowerLiteral (LiBool False) = LitFalse
lowerLiteral LiUnit = Unit

lowerBigLams :: MonadLower m => S.Type Typed -> S.Expr Typed -> m Term
lowerBigLams (S.TyForall (TvName v) (Just k) ty) ex =
  Atom . Lam (TypeArgument v (lowerType k)) <$> lowerBigLams ty ex
lowerBigLams (S.TyForall (TvName v) Nothing ty) ex =
  Atom . Lam (TypeArgument v StarTy) <$> lowerBigLams ty ex
lowerBigLams ty ex = lowerAtTerm ex (lowerType ty)


lowerType :: S.Type Typed -> Type
lowerType t@S.TyTuple{} = go t where
  go (S.TyTuple a b) = ExactRowsTy [("1", lowerType a), ("2", lowerType b)]
  go x = lowerType x
lowerType (S.TyPi bind b)
  | S.Implicit v _ <- bind = ForallTy (S.unTvName v) (lowerType b)
  | S.Anon a <- bind = ArrTy (lowerType a) (lowerType b)
lowerType (S.TyApp a b) = AppTy (lowerType a) (lowerType b)
lowerType (S.TyRows rho vs) = RowsTy (lowerType rho) (map (fmap lowerType) vs)
lowerType (S.TyExactRows vs) = ExactRowsTy (map (fmap lowerType) vs)
lowerType (S.TyVar (TvName v)) = VarTy v
lowerType (S.TyCon (TvName v)) = ConTy v
lowerType (S.TySkol (Skolem _ (TvName v) _ _)) = VarTy v
lowerType (S.TyWithConstraints _ t) = lowerType t
lowerType (S.TyUniverse _) = StarTy

lowerPat :: MonadLower m => Pattern Typed -> m Pat
lowerPat pat = case pat of
  S.Capture (TvName x) (_, t) -> pure $ C.Capture x (lowerType t)
  Wildcard (_, t) -> C.Capture <$> fresh <*> pure (lowerType t)
  Destructure (TvName p) Nothing _ -> pure $ Constr p
  Destructure (TvName p) (Just t) _ -> Destr p <$> lowerPat t
  PType p _ _ -> lowerPat p
  PRecord xs (_, t) ->
    let
      lowerRow (label, pat) = (,) label <$> lowerPat pat
      keys = map fst xs
      realt tp = case tp of
        ExactRowsTy rs -> ExactRowsTy (filter (not . flip elem keys . fst) rs)
        RowsTy rho rs -> RowsTy rho (filter (not . flip elem keys . fst) rs)
        _ -> error $ "not a record type " ++ show (pretty tp)

      fixup (RowsTy rho _) = rho
      fixup x = x

      tidy = fixup . realt . lowerType
     in PatExtend <$> (C.Capture <$> fresh <*> pure (tidy t)) <*> traverse lowerRow xs
  PTuple [] _ -> pure . PatLit $ Unit
  PTuple xs _ ->
    let go [x] = lowerPat x
        go [] = error "no"
        go (x:xs) = do
          x <- lowerPat x
          xs <- go xs
          pure (PatExtend (PatLit RecNil) [("1", x), ("2", xs)])
     in go xs
  PLiteral l _ -> pure . PatLit $ lowerLiteral l

lowerProg :: MonadLower m => [Toplevel Typed] -> m [Stmt]
lowerProg [] = pure []
lowerProg (ForeignVal (TvName t) ex tp _:prg) =
  (Foreign t (lowerType tp) ex:) <$> lowerProg prg
lowerProg (LetStmt vs:prg) =
  (:) <$> (StmtLet <$> for vs (\(TvName v, ex, (_, ant)) -> (v,lowerType ant,) <$> lowerBigLams ant ex))
      <*> lowerProg prg
lowerProg (TypeDecl (TvName var) _ cons:prg) =
  (:) (C.Type var (map (\case
                           UnitCon (TvName p) (_, t) -> (p, lowerType t)
                           ArgCon (TvName p) _ (_, t) -> (p, lowerType t)
                           GeneralisedCon (TvName p) t _ -> (p, lowerType t))
                       cons))
      <$> lowerProg prg
lowerProg (Open _ _:prg) = lowerProg prg
lowerProg (Module _ b:prg) = (++) <$> lowerProg b <*> lowerProg prg
