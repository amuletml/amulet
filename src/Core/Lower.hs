{-# LANGUAGE FlexibleContexts, ConstraintKinds, OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
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
import Data.Function
import Data.Span
import Data.List

import Core.Core

import Syntax

import Text.PrettyPrint.Leijen (pretty)

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
                (CotyForall [TgInternal "a"]
                            (CotyArr cotyString
                                     (CotyVar (TgInternal "a"))))

patternMatchingFailure :: MonadLower m
                       => (Span, Type Typed) -> Type Typed -> m (CoPattern, CoType, CoTerm)
patternMatchingFailure (ex, ot) it = do
  cap <- CopCapture <$> fresh
  let codomain (TyArr _ cd) = codomain cd
      codomain (TyForall _ t) = codomain t
      codomain x = lowerType x

  it' <- lowerType it

  err <- codomain ot
  (,,) <$> pure (cap it') <*> pure it'
       <*> pure (CotApp (CotTyApp errRef err)
                        (CotLit (ColStr (T.pack (show (pretty ex))))))

lowerExpr :: MonadLower m => Expr Typed -> m CoTerm
lowerExpr expr
  | exprT <- getType expr = case expr of
    VarRef (TvName p) _ -> CotRef p <$> lowerType exprT
    Let vs t _ -> do
      vs' <- for vs $ \(TvName var, ex, (_, ant)) -> do
        (k, _) <- makeBigLams ant
        (,,) <$> pure var
             <*> lowerType ant
             <*> (k <$> lowerExpr ex)
      CotLet vs' <$> lowerExpr t
    If c t e _ -> do
      t' <- lowerExpr t
      e' <- lowerExpr e
      let tc = (CopLit ColTrue, cotyBool, t')
          te = (CopLit ColFalse, cotyBool, e')
      CotMatch <$> lowerExpr c <*> pure [tc, te]
    App f x _ -> CotApp <$> lowerExpr f <*> lowerExpr x
    Fun p bd an -> do
      (k, CotyArr f _) <- makeBigLams exprT
      (x, bd', p') <- (,,) <$> fresh <*> lowerExpr bd <*> lowerPat p
      fail <- patternMatchingFailure an (getType p)
      pure . k $ CotLam Small (x, f)
                  $ CotMatch (CotRef x f) [ (p', f, bd'), fail ]
    Begin xs _ -> case xs of
      [x] -> lowerExpr x
      _ -> CotBegin <$> traverse lowerExpr (init xs) <*> lowerExpr (last xs)
    Literal l _ -> pure . CotLit $ case l of
      LiInt i -> ColInt i
      LiStr t -> ColStr t
      LiBool True -> ColTrue
      LiBool False -> ColFalse
      LiUnit -> ColUnit
    Match ex cs an -> do
      cs' <- for cs $ \(pat, ex) ->
        (,,) <$> lowerPat pat <*> lowerType (getType pat) <*> lowerExpr ex
      fail <- patternMatchingFailure an (getType (fst (head cs)))
      CotMatch <$> lowerExpr ex <*> pure (cs' ++ [fail])
    BinOp left op right _ -> do
      (left', op', right') <- (,,) <$> lowerExpr left
                                   <*> lowerExpr op
                                   <*> lowerExpr right
      pure (CotApp (CotApp op' left') right')
    Hole{} -> error "hole finder prevents these from getting lowered"
    Ascription e _ _ -> lowerExpr e

    Record xs _ -> case xs of
      [] -> pure (CotLit ColRecNil)
      xs -> do
        xs' <- for xs $ \(label, ex) ->
          (,,) <$> pure label <*> lowerType (getType ex) <*> lowerExpr ex
        pure (CotExtend (CotLit ColRecNil) xs')
    RecordExt e xs _ -> do
      xs' <- for xs $ \(label, ex) ->
        (,,) <$> pure label <*> lowerType (getType ex) <*> lowerExpr ex
      CotExtend <$> lowerExpr e <*> pure xs'
    Access ex key (_, t) -> do
      var <- fresh
      rest <- fresh
      t' <- lowerType t
      ext <- lowerType (getType ex)
      let realt = case ext of
            CotyExactRows rs -> CotyExactRows (deleteBy ((==) `on` fst) (key, undefined) rs)
            CotyRows rho rs -> CotyRows rho (deleteBy ((==) `on` fst) (key, undefined) rs)
            _ -> error $ "not a record type " ++ show (pretty ex)

          fixup (CotyRows rho []) = rho
          fixup x = x

          pat = CopExtend (CopCapture rest (fixup realt)) [(key, CopCapture var t')]
          ref = CotRef var t'
      CotMatch <$> lowerExpr ex <*> pure [(pat, ext, ref)]
    Tuple xs _ -> do
      let go :: MonadLower m => Int -> Expr Typed -> m (T.Text, CoType, CoTerm)
          go k x = (,,) <$> pure (T.pack (show k))
                        <*> lowerType (getType x)
                        <*> lowerExpr x
      CotExtend (CotLit ColRecNil) <$> zipWithM go [1..] xs
    TypeApp f x _ -> CotTyApp <$> lowerExpr f <*> lowerType x
    x -> error $ "impossible lowering (desugarer removes): " ++ show (pretty x)

lowerType :: MonadLower m => Type Typed -> m CoType
lowerType tt = case tt of
  t@TyTuple{} -> CotyExactRows <$> tup2Rec 1 t
  TyArr a b -> CotyArr <$> lowerType a <*> lowerType b
  TyForall vs b -> CotyForall (map unTvName vs) <$> lowerType b
  TyApp a b -> CotyApp <$> lowerType a <*> lowerType b
  TyRows rho vs -> CotyRows <$> lowerType rho <*> do
    for vs $ \(label, tp) -> (,) label <$> lowerType tp
  TyExactRows vs -> CotyExactRows <$> do
    for vs $ \(label, tp) -> (,) label <$> lowerType tp
  TyVar (TvName v) -> pure (CotyVar v)
  TyCon (TvName v) -> pure (CotyCon v)
  TySkol (Skolem (TvName v) _ _) -> pure (CotyCon v)

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
lowerProg = traverse lowerTop where
  lowerTop (ForeignVal (TvName t) ex tp _) = do
    tp' <- lowerType tp
    pure $ CosForeign t tp' ex
  lowerTop (LetStmt vs) =
    CosLet <$> for vs (\(TvName v, ex, (_, ant)) -> do
      (k, _) <- makeBigLams ant
      (,,) <$> pure v <*> lowerType ant <*> (k <$> lowerExpr ex))
  lowerTop (TypeDecl (TvName var) _ cons)
    = CosType var <$> do
        for cons $ \case
          UnitCon (TvName p) (_, t) -> (,) p <$> lowerType t
          ArgCon (TvName p) _ (_, t) -> (,) p <$> lowerType t
