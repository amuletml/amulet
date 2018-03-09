{-# LANGUAGE FlexibleContexts, ConstraintKinds, OverloadedStrings #-}
{-# LANGUAGE LambdaCase, TupleSections, ExplicitNamespaces, PatternSynonyms #-}
module Core.Lower
  ( lowerExprTerm
  , lowerType
  , lowerPat
  , lowerProg
  , cotyString, cotyInt, cotyBool, cotyUnit
  ) where

import Control.Monad.Infer
import Control.Monad.Cont

import Types.Infer (tyString, tyInt, tyBool, tyUnit)

import qualified Data.Text as T
import Data.Traversable
import Data.Function
import Data.Foldable
import Data.Span
import Data.List

import qualified Core.Core as C
import Core.Core hiding (Atom, Term, Stmt, Type, Pattern)
import Core.Core (pattern Atom)

import qualified Syntax as S
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

cotyString, cotyUnit, cotyBool, cotyInt :: Type
cotyString = lowerType tyString
cotyUnit = lowerType tyUnit
cotyBool = lowerType tyBool
cotyInt = lowerType tyInt

makeBigLams :: S.Type Typed -> Term -> Term
makeBigLams (S.TyForall vs _) =
  let biglam (TvName x:xs) = Atom . Lam Big (x, StarTy) . biglam xs
      biglam [] = id
  in biglam vs
makeBigLams _ = id

errRef :: Atom
errRef = Ref (TgInternal "error")
                (ForallTy (TgInternal "a")
                            (ArrTy cotyString
                                     (VarTy (TgInternal "a"))))

patternMatchingFail :: MonadLower m => Span -> Type -> m (Pat, Type, Term)
patternMatchingFail w t = do
  var <- fresh
  tyApp <- fresh
  let err = Lit (Str (T.pack ("Pattern matching failure at " ++ show (pretty w))))
      errTy = ArrTy cotyString t
  pure (C.Capture var t, t, C.Let [(tyApp, errTy, C.TyApp errRef t)]
                             (C.App (C.Ref tyApp errTy) err))

lowerAtAtom :: MonadLower m => Expr Typed -> Type -> Lower m Atom
lowerAtAtom x t = do x' <- lowerAt x t
                     case x' of
                       C.Atom a -> pure a
                       x' -> ContT $ \k ->
                         fresh >>= \v -> C.Let [(v, t, x')] <$> k (C.Ref v t)

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
lowerAt e (ForallTy vs b) = Atom . Lam Big (vs, StarTy) <$> lowerAtTerm e b
lowerAt (VarRef (TvName p) _) ty = pure (Atom (Ref p ty))
lowerAt (S.Let vs t _) ty = do
  vs' <- for vs $ \(TvName var, ex, (_, ty)) -> do
    let ty' = lowerType ty
    (var,ty',) <$> lowerAtTerm ex ty'
  C.Let vs' <$> lowerAtTerm t ty
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
        S.Capture (TvName v) _ -> Atom . Lam Small (v, a) <$> lowerAtTerm bd b
        _ -> do
          (p', bd') <- (,) <$> lowerPat p <*> lowerAtTerm bd b
          arg <- fresh
          fail <- patternMatchingFail (fst an) b
          pure (Atom (Lam Small (arg, a) (C.Match (Ref arg a) [ (p', a, bd'), fail ])))
lowerAt (Begin [x] _) t = lowerAt x t
lowerAt (Begin xs _) t = lowerAtTerm (last xs) t >>= flip (foldrM bind) (init xs) where
  bind e r = flip C.Let r . pure <$> (build <$> fresh <*> lowerBothTerm e)
  build a (b, c) = (a, c, b)
lowerAt (S.Match ex cs an) ty = do
  (ex', mt) <- lowerBothAtom ex
  cs' <- for cs $ \(pat, ex) ->
    (,mt,) <$> lowerPat pat <*> lowerAtTerm ex ty
  fail <- patternMatchingFail (fst an) ty

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
lowerAt e _ = lowerAnyway e

lowerAnyway :: MonadLower m => Expr Typed -> Lower m Term
lowerAnyway (Record xs _) = case xs of
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

lowerAnyway (Literal l _) = pure . Atom . Lit $ case l of
  LiInt i -> Int i
  LiStr t -> Str t
  LiBool True -> LitTrue
  LiBool False -> LitFalse
  LiUnit -> Unit
lowerAnyway (S.App f x _) = C.App <$> lowerExprAtom f <*> lowerExprAtom x

lowerAnyway (TypeApp f x _) = flip TyApp (lowerType x) <$> lowerExprAtom f
lowerAnyway (Tuple xs _) = do
  xs' <- traverse lowerBothAtom xs
  pure $ Extend (Lit RecNil) (zipWith build [1..] xs')
  where build num (atom, ty) = (T.pack (show (num :: Int)), ty, atom)
lowerAnyway e = error ("can't lower " ++ show e ++ " without type")

lowerType :: S.Type Typed -> Type
lowerType t@S.TyTuple{} = ExactRowsTy (tup2Rec 1 t)
lowerType (S.TyArr a b) = ArrTy (lowerType a) (lowerType b)
lowerType (S.TyForall vs b) = foldr (ForallTy . S.unTvName) (lowerType b) vs
lowerType (S.TyApp a b) = AppTy (lowerType a) (lowerType b)
lowerType (S.TyRows rho vs) = RowsTy (lowerType rho) (map (fmap lowerType) vs)
lowerType (S.TyExactRows vs) = ExactRowsTy (map (fmap lowerType) vs)
lowerType (S.TyVar (TvName v)) = VarTy v
lowerType (S.TyCon (TvName v)) = ConTy v
lowerType (S.TySkol (Skolem _ (TvName v) _)) = VarTy v
lowerType (S.TyWithConstraints _ t) = lowerType t

tup2Rec :: Int -> S.Type Typed -> [(T.Text, Type)]
tup2Rec k (S.TyTuple a b) = (T.pack (show k), lowerType a) : tup2Rec (succ k) b
tup2Rec k b = [(T.pack (show k), lowerType b)]

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
  PTuple xs _ -> do
    let go :: MonadLower m => Int -> Pattern Typed -> m (T.Text, Pat)
        go k x = (,) <$> pure (T.pack (show k))
                     <*> lowerPat x
    PatExtend (PatLit RecNil) <$> zipWithM go [1..] xs

lowerProg :: MonadLower m => [Toplevel Typed] -> m [Stmt]
lowerProg [] = pure []
lowerProg (ForeignVal (TvName t) ex tp _:prg) =
  (Foreign t (lowerType tp) ex:) <$> lowerProg prg
lowerProg (LetStmt vs:prg) =
  (:) <$> (StmtLet <$> for vs (\(TvName v, ex, (_, ant)) -> (v,lowerType ant,) . makeBigLams ant <$> lowerExprTerm ex))
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
