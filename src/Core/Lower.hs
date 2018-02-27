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
import Data.Function
import Data.Foldable
import Data.Span
import Data.List

import qualified Core.Core as C
import Core.Core hiding (CoAtom, CoTerm, CoStmt, CoType, CoPattern)

import Syntax

import Pretty (pretty)

type MonadLower m
  = ( MonadGen Int m
    , MonadReader Env m )

type CoAtom = C.CoAtom (Var Resolved)
type CoTerm = C.CoTerm (Var Resolved)
type CoType = C.CoType (Var Resolved)
type CoPattern = C.CoPattern (Var Resolved)
type CoStmt = C.CoStmt (Var Resolved)

cotyString, cotyUnit, cotyBool, cotyInt :: CoType
cotyString = lowerType tyString
cotyUnit = lowerType tyUnit
cotyBool = lowerType tyBool
cotyInt = lowerType tyInt

makeBigLams :: Type Typed -> CoTerm -> CoTerm
makeBigLams (TyForall vs t) =
  let biglam (TvName x:xs) = CotAtom . CoaLam Big (x, CotyStar) . biglam xs
      biglam [] = id
  in biglam vs
makeBigLams _ = id

errRef :: CoAtom
errRef = CoaRef (TgInternal "error")
                (CotyForall (TgInternal "a")
                            (CotyArr cotyString
                                     (CotyVar (TgInternal "a"))))

patternMatchingFail :: MonadLower m => Span -> CoType -> m (CoPattern, CoType, CoTerm)
patternMatchingFail w t = do
  var <- fresh
  tyApp <- fresh
  let err = CoaLit (ColStr (T.pack ("Pattern matching failure at " ++ show (pretty w))))
      errTy = CotyArr cotyString t
  pure (CopCapture var t, t, CotLet [(tyApp, errTy, CotTyApp errRef t)]
                             (CotApp (CoaRef tyApp errTy) err))

onAtom :: MonadLower m => CoTerm -> CoType -> (CoAtom -> CoTerm) -> m CoTerm
onAtom (CotAtom a) _ f = pure (f a)
onAtom x           t f = (\v -> CotLet [(v, t, x)] (f (CoaRef v t))) <$> fresh

onAtomM :: MonadLower m => CoTerm -> CoType -> (CoAtom -> m CoTerm) -> m CoTerm
onAtomM (CotAtom a) _ f = f a
onAtomM x           t f = (\v -> CotLet [(v, t, x)] <$> f (CoaRef v t)) =<< fresh

onAtoms :: MonadLower m => [(CoTerm, CoType)] -> ([CoAtom] -> CoTerm) -> m CoTerm
onAtoms = accum [] where
  accum as [] f = pure (f (reverse as))
  accum as ((CotAtom a,_):xs) f = accum (a:as) xs f
  accum as ((x,tx):xs) f = do
    v <- fresh
    CotLet [(v, tx, x)] <$> accum (CoaRef v tx:as) xs f

lowerExpr :: MonadLower m => Expr Typed -> m CoTerm
lowerExpr e = lowerAt e (lowerType (getType e))

lowerBoth :: MonadLower m => Expr Typed -> m (CoTerm, CoType)
lowerBoth e = let t = lowerType (getType e)
              in (,t) <$> lowerAt e t

lowerAt :: MonadLower m => Expr Typed -> CoType -> m CoTerm
lowerAt (Ascription e _ _) t = lowerAt e t
lowerAt e (CotyForall vs b) = CotAtom . CoaLam Big (vs, CotyStar) <$> lowerAt e b
lowerAt (VarRef (TvName p) _) ty = pure (CotAtom (CoaRef p ty))
lowerAt (Let vs t _) ty = do
  vs' <- for vs $ \(TvName var, ex, (_, ty)) -> do
    let ty' = lowerType ty
    (var,ty',) <$> lowerAt ex ty'
  CotLet vs' <$> lowerAt t ty
lowerAt (If c t e _) ty = do
  (c', t', e') <- (,,) <$> lowerAt c cotyBool <*> lowerAt t ty <*> lowerAt e ty
  let tc = (CopLit ColTrue, cotyBool, t')
      te = (CopLit ColFalse, cotyBool, e')
  onAtom c' cotyBool (flip CotMatch [tc, te])
lowerAt (Fun p bd an) (CotyArr a b) =
  let operational (PType p _ _) = operational p
      operational p = p
   in case operational p of
        Capture (TvName v) _ -> CotAtom . CoaLam Small (v, a) <$> lowerAt bd b
        _ -> do
          (p', bd') <- (,) <$> lowerPat p <*> lowerAt bd b
          arg <- fresh
          fail <- patternMatchingFail (fst an) b
          pure (CotAtom (CoaLam Small (arg, a) (CotMatch (CoaRef arg a) [ (p', a, bd'), fail ])))
lowerAt (Begin [x] _) t = lowerAt x t
lowerAt (Begin xs _) t = lowerAt (last xs) t >>= flip (foldrM bind) (init xs) where
  bind e r = flip CotLet r . pure <$> ((,,) <$> fresh
                                            <*> pure (lowerType (getType e))
                                            <*> lowerExpr e)
lowerAt (Match ex cs an) ty = do
  let mt = lowerType (getType ex)
  cs' <- for cs $ \(pat, ex) ->
    (,,) <$> lowerPat pat <*> pure mt <*> lowerAt ex ty
  fail <- patternMatchingFail (fst an) ty
  ex' <- lowerAt ex mt

  onAtom ex' mt (flip CotMatch (cs' ++ [fail]))
lowerAt (Access r k _) ty = do
  (r', rt) <- lowerBoth r
  (iv, var) <- (,) <$> fresh <*> fresh
  let cotyRows t [] = t
      cotyRows t xs = CotyRows t xs
      inner =
        case rt of
          CotyRows t rs -> cotyRows t (deleteBy ((==) `on` fst) (k, undefined) rs)
          CotyExactRows rs -> CotyExactRows (deleteBy ((==) `on` fst) (k, undefined) rs)
          _ -> error ("not a row type " ++ show rt)
      match = ( CopExtend (CopCapture iv inner) [ (k, CopCapture var ty) ]
              , rt, CotAtom (CoaRef var ty ))
  onAtom r' rt (flip CotMatch [match])

lowerAt (BinOp left op right a) t = lowerAt (App (App op left a) right a) t
lowerAt Hole{} _ = error "holes can't be lowered"
lowerAt e _ = lowerAnyway e

lowerAnyway :: MonadLower m => Expr Typed -> m CoTerm
lowerAnyway (Record xs _) = case xs of
  [] -> pure (CotAtom (CoaLit ColRecNil))
  xs -> do
    xs' <- traverse (lowerBoth . snd) xs
    onAtoms xs' (CotExtend (CoaLit ColRecNil) . zipWith3 build xs xs')
  where build (name, _) (_, ty) atom = (name, ty, atom)
lowerAnyway (RecordExt e xs _) = do
  (e', et) <- lowerBoth e
  xs' <- traverse (lowerBoth . snd) xs
  onAtomM e' et $ \e'' ->
    onAtoms xs' (CotExtend e'' . zipWith3 build xs xs')
  where build (name, _) (_, ty) atom = (name, ty, atom)

lowerAnyway (Literal l _) = pure . CotAtom . CoaLit $ case l of
  LiInt i -> ColInt i
  LiStr t -> ColStr t
  LiBool True -> ColTrue
  LiBool False -> ColFalse
  LiUnit -> ColUnit
lowerAnyway (App f x _) = do
  (f', tf) <- lowerBoth f
  (x', tx) <- lowerBoth x
  onAtomM f' tf $ onAtom x' tx . CotApp

lowerAnyway (TypeApp f x _) = do
  (f', tf) <- lowerBoth f
  onAtom f' tf (flip CotTyApp (lowerType x))
lowerAnyway (Tuple xs _) = do
  xs' <- traverse lowerBoth xs
  onAtoms xs' (CotExtend (CoaLit ColRecNil) . zipWith3 build [1..] xs')
  where build num (_, ty) atom = (T.pack (show (num :: Int)), ty, atom)
lowerAnyway e = error ("can't lower " ++ show e ++ " without type")

lowerType :: Type Typed -> CoType
lowerType t@TyTuple{} = CotyExactRows (tup2Rec 1 t)
lowerType (TyArr a b) = CotyArr (lowerType a) (lowerType b)
lowerType (TyForall vs b) = foldr (CotyForall . unTvName) (lowerType b) vs
lowerType (TyApp a b) = CotyApp (lowerType a) (lowerType b)
lowerType (TyRows rho vs) = CotyRows (lowerType rho) (map (fmap lowerType) vs)
lowerType (TyExactRows vs) = CotyExactRows (map (fmap lowerType) vs)
lowerType (TyVar (TvName v)) = CotyVar v
lowerType (TyCon (TvName v)) = CotyCon v
lowerType (TySkol (Skolem _ (TvName v) _)) = CotyVar v

tup2Rec :: Int -> Type Typed -> [(T.Text, CoType)]
tup2Rec k (TyTuple a b) = (T.pack (show k), lowerType a) : tup2Rec (succ k) b
tup2Rec k b = [(T.pack (show k), lowerType b)]

lowerPat :: MonadLower m => Pattern Typed -> m CoPattern
lowerPat pat = case pat of
  Capture (TvName x) (_, t) -> pure $ CopCapture x (lowerType t)
  Wildcard (_, t) -> CopCapture <$> fresh <*> pure (lowerType t)
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

      tidy = fixup . realt . lowerType
     in CopExtend <$> (CopCapture <$> fresh <*> pure (tidy t)) <*> traverse lowerRow xs
  PTuple [] _ -> pure . CopLit $ ColUnit
  PTuple xs _ -> do
    let go :: MonadLower m => Int -> Pattern Typed -> m (T.Text, CoPattern)
        go k x = (,) <$> pure (T.pack (show k))
                     <*> lowerPat x
    CopExtend (CopLit ColRecNil) <$> zipWithM go [1..] xs

lowerProg :: MonadLower m => [Toplevel Typed] -> m [CoStmt]
lowerProg [] = pure []
lowerProg (ForeignVal (TvName t) ex tp _:prg) = do
  (CosForeign t (lowerType tp) ex:) <$> lowerProg prg
lowerProg (LetStmt vs:prg) =
  (:) <$> (CosLet <$> for vs (\(TvName v, ex, (_, ant)) -> do
                                 (v, lowerType ant, ) . (makeBigLams ant) <$> lowerExpr ex))
      <*> lowerProg prg
lowerProg (TypeDecl (TvName var) _ cons:prg) =
  (:) (CosType var (map (\case
                            UnitCon (TvName p) (_, t) -> (p, lowerType t)
                            ArgCon (TvName p) _ (_, t) -> (p, lowerType t)) cons))
      <$> lowerProg prg
lowerProg (Open _ _:prg) = lowerProg prg
lowerProg (Module _ b:prg) = (++) <$> lowerProg b <*> lowerProg prg
