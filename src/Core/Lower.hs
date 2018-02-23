{-# LANGUAGE FlexibleContexts, ConstraintKinds, OverloadedStrings #-}
{-# LANGUAGE LambdaCase, TupleSections #-}
module Core.Lower
  ( lowerExpr
  , lowerType
  , lowerPat
  , lowerProg
  , cotyString, cotyInt, cotyBool, cotyUnit
  ) where


import Control.Monad.Infer

import Types.Infer (tyString, tyInt, tyBool, tyUnit)

import qualified Data.Text as T
import Data.Traversable
import Data.Span

import Core.Core

import Syntax

import Pretty (pretty)

type MonadLower m
  = ( MonadGen Int m
    , MonadReader Env m )

cotyString, cotyUnit, cotyBool, cotyInt :: CoType
cotyString = runGenT (lowerType tyString) mempty
cotyUnit = runGenT (lowerType tyUnit) mempty
cotyBool = runGenT (lowerType tyBool) mempty
cotyInt = runGenT (lowerType tyInt) mempty

makeBigLams :: MonadLower m
            => Type Typed
            -> m (CoTerm -> CoTerm, CoType)
makeBigLams (TyForall vs t) = do
  let biglam (TvName x:xs) = CotLam Big (x, CotyStar) . biglam xs
      biglam [] = id
  (,) (biglam vs) <$> lowerType t
makeBigLams t = (,) id <$> lowerType t

errRef :: CoTerm
errRef = CotRef (TgInternal "error")
                (CotyForall (TgInternal "a")
                            (CotyArr cotyString
                                     (CotyVar (TgInternal "a"))))

patternMatchingFail :: MonadLower m => Span -> CoType -> m (CoPattern, CoType, CoTerm)
patternMatchingFail w t = do
  var <- fresh
  let err = CotLit (ColStr (T.pack ("Pattern matching failure at " ++ show (pretty w))))
  pure (CopCapture var t, t, CotApp (CotTyApp errRef t) err)

lowerExpr :: MonadLower m => Expr Typed -> m CoTerm
lowerExpr e = lowerAt e =<< lowerType (getType e)

lowerAt :: MonadLower m => Expr Typed -> CoType -> m CoTerm
lowerAt (Ascription e _ _) t = lowerAt e t
lowerAt e (CotyForall vs b) = CotLam Big (vs, CotyStar) <$> lowerAt e b
lowerAt (VarRef (TvName p) _) ty = pure (CotRef p ty)
lowerAt (Let vs t _) ty = do
  vs' <- for vs $ \(TvName var, ex, (_, ty)) -> do
    ty' <- lowerType ty
    (,,) var <$> pure ty' <*> lowerAt ex ty'
  CotLet vs' <$> lowerAt t ty
lowerAt (If c t e _) ty = do
  (c', t', e') <- (,,) <$> lowerAt c cotyBool <*> lowerAt t ty <*> lowerAt e ty
  let tc = (CopLit ColTrue, cotyBool, t')
      te = (CopLit ColFalse, cotyBool, e')
  pure (CotMatch c' [tc, te])
lowerAt (App f x _) _ = -- type ignored, ughr
  CotApp <$> lowerExpr f <*> lowerExpr x
lowerAt (Fun p bd an) (CotyArr a b) =
  let operational (PType p _ _) = operational p
      operational p = p
   in case operational p of
        Capture (TvName v) _ -> CotLam Small (v, a) <$> lowerAt bd b
        _ -> do
          (p', bd') <- (,) <$> lowerPat p <*> lowerAt bd b
          arg <- fresh
          fail <- patternMatchingFail (fst an) b
          pure (CotLam Small (arg, a) (CotMatch (CotRef arg a) [ (p', a, bd'), fail ]))
lowerAt (Begin [x] _) t = lowerAt x t
lowerAt (Begin xs _) t = CotBegin <$> traverse lowerExpr (init xs) <*> lowerAt (last xs) t
lowerAt (Match ex cs an) ty = do
  mt <- lowerType (getType ex)
  cs' <- for cs $ \(pat, ex) ->
    (,,) <$> lowerPat pat <*> pure mt <*> lowerAt ex ty
  fail <- patternMatchingFail (fst an) ty
  CotMatch <$> lowerAt ex mt <*> pure (cs' ++ [fail])
lowerAt (BinOp left op right a) t = lowerAt (App (App op left a) right a) t
lowerAt Hole{} _ = error "holes can't be lowered"
lowerAt e _ = lowerAnyway e

lowerAnyway :: MonadLower m => Expr Typed -> m CoTerm
lowerAnyway (Record xs _) = case xs of
  [] -> pure (CotLit ColRecNil)
  xs -> do
    xs' <- for xs $ \(label, ex) ->
      (,,) <$> pure label <*> lowerType (getType ex) <*> lowerExpr ex
    pure (CotExtend (CotLit ColRecNil) xs')
lowerAnyway (RecordExt e xs _) = do
  xs' <- for xs $ \(label, ex) ->
    (label,,) <$> lowerType (getType ex) <*> lowerExpr ex
  CotExtend <$> lowerExpr e <*> pure xs'
lowerAnyway (Literal l _) = pure . CotLit $ case l of
  LiInt i -> ColInt i
  LiStr t -> ColStr t
  LiBool True -> ColTrue
  LiBool False -> ColFalse
  LiUnit -> ColUnit
lowerAnyway (TypeApp f x _) = CotTyApp <$> lowerExpr f <*> lowerType x
lowerAnyway (Tuple xs _) = do
  let go :: MonadLower m => Int -> Expr Typed -> m (T.Text, CoType, CoTerm)
      go k x = (,,) <$> pure (T.pack (show k))
                    <*> lowerType (getType x)
                    <*> lowerExpr x
  CotExtend (CotLit ColRecNil) <$> zipWithM go [1..] xs
lowerAnyway e = error ("can't lower " ++ show e ++ " without type")

lowerType :: MonadLower m => Type Typed -> m CoType
lowerType tt = case tt of
  t@TyTuple{} -> CotyExactRows <$> tup2Rec 1 t
  TyArr a b -> CotyArr <$> lowerType a <*> lowerType b
  TyForall vs b -> do
    b' <- lowerType b
    pure (foldr CotyForall b' (map unTvName vs))
  TyApp a b -> CotyApp <$> lowerType a <*> lowerType b
  TyRows rho vs -> CotyRows <$> lowerType rho <*> do
    for vs $ \(label, tp) -> (,) label <$> lowerType tp
  TyExactRows vs -> CotyExactRows <$> do
    for vs $ \(label, tp) -> (,) label <$> lowerType tp
  TyVar (TvName v) -> pure (CotyVar v)
  TyCon (TvName v) -> pure (CotyCon v)
  TySkol (Skolem _ (TvName v) _) -> pure (CotyVar v)

tup2Rec :: MonadLower m => Int -> Type Typed -> m [(T.Text, CoType)]
tup2Rec k (TyTuple a b) = do
  a' <- lowerType a
  (:) (T.pack (show k), a') <$> tup2Rec (succ k) b
tup2Rec k b = do
  b' <- lowerType b
  pure [(T.pack (show k), b')]

lowerPat :: MonadLower m => Pattern Typed -> m CoPattern
lowerPat pat = case pat of
  Capture (TvName x) (_, t) -> CopCapture x <$> lowerType t
  Wildcard (_, t) -> CopCapture <$> fresh <*> lowerType t
  Destructure (TvName p) Nothing _ -> pure $ CopConstr p
  Destructure (TvName p) (Just t) _ -> CopDestr p <$> lowerPat t
  PType p _ _ -> lowerPat p
  PRecord xs (_, t) ->
    let
      lowerRow (label, pat) = (,) label <$> lowerPat pat
      keys = map fst xs
      realt tp = case tp of
        CotyExactRows rs -> CotyExactRows (filter (not . flip elem keys . fst) rs)
        CotyRows rho rs -> CotyRows rho (filter (not . flip elem keys . fst) rs)
        _ -> error $ "not a record type " ++ show (pretty tp)

      fixup (CotyRows rho _) = rho
      fixup x = x

      tidy = fmap (fixup . realt) . lowerType
     in CopExtend <$> (CopCapture <$> fresh <*> tidy t) <*> traverse lowerRow xs
  PTuple [] _ -> pure . CopLit $ ColUnit
  PTuple xs _ -> do
    let go :: MonadLower m => Int -> Pattern Typed -> m (T.Text, CoPattern)
        go k x = (,) <$> pure (T.pack (show k))
                     <*> lowerPat x
    CopExtend (CopLit ColRecNil) <$> zipWithM go [1..] xs

lowerProg :: MonadLower m => [Toplevel Typed] -> m [CoStmt]
lowerProg [] = pure []
lowerProg (ForeignVal (TvName t) ex tp _:prg) = do
  tp' <- lowerType tp
  (CosForeign t tp' ex:) <$> lowerProg prg
lowerProg (LetStmt vs:prg) =
  (:) <$> (CosLet <$> for vs (\(TvName v, ex, (_, ant)) -> do
                                 (k, _) <- makeBigLams ant
                                 (v,,) <$> lowerType ant <*> (k <$> lowerExpr ex)))
      <*> lowerProg prg
lowerProg (TypeDecl (TvName var) _ cons:prg) =
  (:) <$> (CosType var <$> do
              for cons $ \case
                UnitCon (TvName p) (_, t) -> (,) p <$> lowerType t
                ArgCon (TvName p) _ (_, t) -> (,) p <$> lowerType t)
      <*> lowerProg prg
lowerProg (Open _ _:prg) = lowerProg prg
lowerProg (Module _ b:prg) = (++) <$> lowerProg b <*> lowerProg prg
