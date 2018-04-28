{-# LANGUAGE FlexibleContexts, ConstraintKinds, OverloadedStrings #-}
{-# LANGUAGE LambdaCase, TupleSections, ExplicitNamespaces, PatternSynonyms #-}
module Core.Lower
  ( runLowerT
  , lowerExprTerm
  , lowerType
  , lowerPat
  , lowerProg
  ) where

import Control.Monad.Infer
import Control.Monad.Cont
import Control.Arrow

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Traversable
import Data.Function
import Data.Foldable
import Data.Maybe
import Data.Graph
import Data.Span
import Data.List

import qualified Core.Core as C
import qualified Core.Builtin as C
import Core.Optimise (substituteInType)
import Core.Core hiding (Atom, Term, Stmt, Type, Pattern, Arm)
import Core.Core (pattern Atom)
import Core.Types (unify, replaceTy)

import qualified Syntax as S
import Syntax.Let
import Syntax (Var(..), Resolved, Typed, Expr(..), Pattern(..), Lit(..), Skolem(..), Toplevel(..), Constructor(..))

import Pretty (pretty)

type Atom = C.Atom (Var Resolved)
type Term = C.Term (Var Resolved)
type Type = C.Type (Var Resolved)
type Pat = C.Pattern (Var Resolved)
type Stmt = C.Stmt (Var Resolved)
type Arm = C.Arm (Var Resolved)

type Lower = ContT Term

data LowerState = LS { vars :: Map.Map (Var Resolved) (C.Type (Var Resolved))
                     , ctors :: Map.Map (Var Resolved) (C.Type (Var Resolved))
                     }
  deriving (Eq, Show)

type MonadLower m
  = ( MonadGen Int m
    , MonadReader LowerState m )

runLowerT :: MonadGen Int m => ReaderT LowerState m a -> m a
runLowerT = flip runReaderT (LS mempty mempty)

errRef :: Atom
errRef = Ref (TgInternal "error")
                (ForallTy (Relevant (TgInternal "a")) StarTy
                          (ForallTy Irrelevant C.tyString
                                 (VarTy (TgInternal "a"))))

patternMatchingFail :: MonadLower m => Span -> Type -> Type -> m Arm
patternMatchingFail w p t = do
  var <- fresh
  tyApp <- fresh
  let err = Lit (Str (T.pack ("Pattern matching failure at " ++ show (pretty w))))
      errTy = ForallTy Irrelevant C.tyString t
  pure C.Arm { armPtrn = C.Capture var p, armTy = p
             , armBody = C.Let (One (tyApp, errTy, C.TyApp errRef t))
               (C.App (C.Ref tyApp errTy) err)
             , armVars = [(var, p)], armTyvars = []
             }

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
  c' <- lowerAtAtom c C.tyBool
  t' <- lowerAtTerm t ty
  e' <- lowerAtTerm e ty
  let tc = C.Arm (PatLit LitTrue) C.tyBool t' [] []
      te = C.Arm (PatLit LitFalse)  C.tyBool e' [] []
  pure $ C.Match c' [tc, te]
lowerAt (Fun p bd an) (ForallTy Irrelevant a b) =
  let operational (PType p _ _) = operational p
      operational p = p
   in case operational p of
        S.Capture (TvName v) _ -> Atom . Lam (TermArgument v a) <$> lowerAtTerm bd b
        _ -> do
          p' <- lowerPat p
          ts <- patternTyvars p
          bd' <- lowerAtTerm bd b
          arg <- fresh
          fail <- patternMatchingFail (fst an) a b
          pure (Atom (Lam (TermArgument arg a) (C.Match (Ref arg a) [ C.Arm { armPtrn = p', armTy = a, armBody = bd'
                                                                            , armVars = patternVars p', armTyvars = ts }
                                                                    , fail ])))
lowerAt (Begin [x] _) t = lowerAt x t
lowerAt (Begin xs _) t = lowerAtTerm (last xs) t >>= flip (foldrM bind) (init xs) where
  bind e r = flip C.Let r . One <$> (build <$> fresh <*> lowerBothTerm e)
  build a (b, c) = (a, c, b)
lowerAt (S.Match ex cs an) ty = do
  (ex', mt) <- lowerBothAtom ex
  cs' <- for cs $ \(pat, ex) -> do
    p' <- lowerPat pat
    ts <- patternTyvars pat
    ex' <- lowerAtTerm ex ty
    pure C.Arm { armPtrn = p', armTy = mt, armBody = ex'
               , armVars = patternVars p', armTyvars = ts }
  fail <- patternMatchingFail (fst an) mt ty

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
      match = C.Arm { armPtrn = PatExtend (C.Capture iv inner) [ (k, C.Capture var ty) ]
                    , armTy = rt, armBody = Atom (Ref var ty )
                    , armVars = [(iv, inner), (var, ty)], armTyvars = []
                    }
  pure $ C.Match r' [match]

lowerAt (BinOp left op right a) t = lowerAt (S.App (S.App op left a) right a) t
lowerAt Hole{} _ = error "holes can't be lowered"

lowerAt (Tuple [x] _) t = lowerAt x t
lowerAt (Tuple (x:xs) _) (ExactRowsTy [(_, a), (_, b)]) = do
  x <- lowerAtAtom x a
  xs <- lowerAtAtom (Tuple xs undefined) b
  pure (Extend (Lit RecNil) [("1", a, x), ("2", b, xs)])

lowerAt (ExprWrapper wrap e an) ty =
  case wrap of
    S.Cast S.ReflCo{} -> lowerAt e ty
    S.Cast c -> do
      ex' <- lowerExprAtom e
      pure (C.Cast ex' (co c))
    S.TypeApp t -> do
      ex' <- lowerAtAtom e (lowerType (S.getType e))
      pure (C.TyApp ex' (lowerType t))
    S.TypeLam (Skolem (TvName (TgName _ id)) (TvName (TgName n _)) _ _) k ->
      let ty' (ForallTy (Relevant v) _ t) = substituteInType (Map.singleton v (VarTy var)) t
          ty' x = x
          var = TgName n id
       in Atom . Lam (TypeArgument var (lowerType k)) <$> lowerAtTerm e (ty' ty)
    S.TypeLam _ _ -> error "impossible lowerAt TypeLam"
    ws S.:> wy -> lowerAt (ExprWrapper ws (ExprWrapper wy e an) an) ty
    S.WrapVar v -> error $ "Unsolved wrapper variable " ++ show v ++ ". This is a bug"
    S.IdWrap -> lowerAt e ty
  where
    co (S.VarCo (TvName x)) = CoercionVar x
    co (S.ReflCo t) = SameRepr (lowerType t) (lowerType t)
    co (S.AssumedCo t t') = SameRepr (lowerType t) (lowerType t')
    co (S.SymCo c) = Symmetry (co c)
    co (S.AppCo a b) = Application (co a) (co b)
    co (S.ArrCo a b) = C.Quantified Irrelevant (co a) (co b)
    co (S.ProdCo a b) = ExactRecord [("1", co a), ("2", co b)]
    co (S.RowsCo c rs) = C.Record (co c) (map (second co) rs)
    co (S.ExactRowsCo rs) = C.ExactRecord (map (second co) rs)
    co (S.ForallCo (TvName v) cd rs) = C.Quantified (Relevant v) (co cd) (co rs)

lowerAt e _ = lowerAnyway e

lowerAnyway :: MonadLower m => Expr Typed -> Lower m Term
lowerAnyway (S.VarRef (TvName v) (_, ty)) = do
  let lty = lowerType ty
  env <- asks vars
  case Map.lookup v env of
    -- If we've got a type which is different to our expected one then we strip
    -- off one forall and attempt to unify. Once we've found our unified type,
    -- we generate the appropriate type applications.
    Just fty | fty /= lty ->
      let addApps ty' vars
            | Just subst <- unify ty' lty
            -- If unification is successful, loop through each stripped variable
            -- and generate the appropriate tyapp. Yep, this is a fold and
            -- continuation in the same bit of code. I'm sorry.
            = foldrM (\tyvar (prev, ForallTy (Relevant _) _ prevTy) -> do
                         let tyuni = fromMaybe (VarTy tyvar) (Map.lookup tyvar subst)
                             newTy = replaceTy tyvar tyuni prevTy
                         ftv <- fresh
                         ContT $ \k ->
                           C.Let (One (ftv, newTy, TyApp prev tyuni)) <$> k (C.Ref ftv newTy, newTy)
                         ) (C.Ref v fty, fty) vars
          -- Otherwise just add our variable to the stripped list
          addApps (ForallTy (Relevant a) _ ty') vars = addApps ty' (a:vars)
          addApps _ _ = error "impossible"
      in runContT (addApps fty []) (pure . Atom . fst)

    _ -> pure (Atom (Ref v lty))
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

lowerAnyway e = error ("can't lower " ++ show e ++ " without type")

lowerLiteral :: Lit -> Literal
lowerLiteral (LiFloat d) = Float d
lowerLiteral (LiInt i) = Int i
lowerLiteral (LiStr t) = Str t
lowerLiteral (LiBool True) = LitTrue
lowerLiteral (LiBool False) = LitFalse
lowerLiteral LiUnit = Unit

lowerType :: S.Type Typed -> Type
lowerType t@S.TyTuple{} = go t where
  go (S.TyTuple a b) = ExactRowsTy [("1", lowerType a), ("2", lowerType b)]
  go x = lowerType x
lowerType (S.TyPi bind b)
  | S.Implicit v Nothing <- bind = ForallTy (Relevant (S.unTvName v)) StarTy (lowerType b)
  | S.Implicit v (Just c) <- bind = ForallTy (Relevant (S.unTvName v)) (lowerType c) (lowerType b)
  | S.Anon a <- bind = ForallTy Irrelevant (lowerType a) (lowerType b)
lowerType (S.TyApp a b) = AppTy (lowerType a) (lowerType b)
lowerType (S.TyRows rho vs) = RowsTy (lowerType rho) (map (fmap lowerType) vs)
lowerType (S.TyExactRows vs) = ExactRowsTy (map (fmap lowerType) vs)
lowerType (S.TyVar (TvName v)) = VarTy v
lowerType (S.TyCon (TvName v)) = ConTy v
lowerType (S.TyPromotedCon (TvName v)) = ConTy v -- TODO this is in the wrong scope
lowerType (S.TySkol (Skolem (TvName (TgName _ id)) (TvName (TgName n _)) _ _)) = VarTy (TgName n id)
lowerType (S.TySkol _) = error "impossible lowerType TySkol"
lowerType (S.TyWithConstraints _ t) = lowerType t
lowerType S.TyType = StarTy

lowerPat :: MonadLower m => Pattern Typed -> m Pat
lowerPat (S.Capture (TvName x) (_, t)) = pure (C.Capture x (lowerType t))
lowerPat (Wildcard (_, t)) = C.Capture <$> fresh <*> pure( lowerType t)
lowerPat (Destructure (TvName p) Nothing _) = pure (Constr p)
lowerPat (Destructure (TvName p) (Just t) _) = Destr p <$> lowerPat t
lowerPat (PType p _ _) = lowerPat p
lowerPat (PRecord xs (_, t)) =
  let
    lowerRow (label, pat) = (label,) <$> lowerPat pat
    keys = map fst xs
    realt tp = case tp of
      ExactRowsTy rs -> ExactRowsTy (filter (not . flip elem keys . fst) rs)
      RowsTy rho rs -> RowsTy rho (filter (not . flip elem keys . fst) rs)
      _ -> error $ "not a record type " ++ show (pretty tp)

    fixup (RowsTy rho _) = rho
    fixup x = x

    tidy = fixup . realt . lowerType
  in PatExtend <$> (C.Capture <$> fresh <*> pure (tidy t)) <*> traverse lowerRow xs

lowerPat (PTuple [] _) = pure (PatLit Unit)
lowerPat (PTuple xs _) =
  let go [x] = lowerPat x
      go [] = error "no"
      go (x:xs) = do
        x' <- lowerPat x
        xs' <- go xs
        pure (PatExtend (PatLit RecNil) [("1", x'), ("2", xs')])
   in go xs
lowerPat (PLiteral l _) = pure (PatLit (lowerLiteral l))

lowerProg :: MonadLower m => [Toplevel Typed] -> m [Stmt]
lowerProg [] = pure []
lowerProg (ForeignVal (TvName t) ex tp _:prg) =
  (Foreign t (lowerType tp) ex:) <$> lowerProg prg
lowerProg (LetStmt vs:prg) = do
  let env' = Map.fromList (map (\(TvName v, _, (_, ant)) -> (v, lowerType ant)) vs)
  (:) <$> local (\s -> s { vars = env' }) (StmtLet <$> for vs (\(TvName v, ex, (_, ant)) -> (v,lowerType ant,) <$> lowerPolyBind (lowerType ant) ex))
      <*> lowerProg prg
lowerProg (FunStmt vs:prg) = lowerProg prg
lowerProg (TypeDecl (TvName var) _ cons:prg) = do
  let cons' = map (\case
                       UnitCon (TvName p) (_, t) -> (p, lowerType t)
                       ArgCon (TvName p) _ (_, t) -> (p, lowerType t)
                       GeneralisedCon (TvName p) t _ -> (p, lowerType t))
                cons
  (C.Type var cons':) <$> local (\s -> s { ctors = Map.union (Map.fromList cons') (ctors s) }) (lowerProg prg)
lowerProg (Open _ _:prg) = lowerProg prg
lowerProg (Module _ b:prg) = (++) <$> lowerProg b <*> lowerProg prg


lowerPolyBind :: MonadLower m => Type -> Expr Typed -> m Term
lowerPolyBind ty ex = doIt (needed ex ty) (go ty ex) (lowerExprTerm ex) where
  go _ ex 0 = lowerExprTerm ex
  go (ForallTy (Relevant v) kind ty) ex n
    | n >= 1 = Atom . Lam (TypeArgument v kind) <$> go ty ex (n - 1)
  go _ _ _ = error "impossible"

  needed ex ty
    | countForalls ty > countLams ex = Just (countForalls ty - countLams ex)
    | otherwise = Nothing -- trust tc

  doIt x a b = case x of
    Just n -> a n
    Nothing -> b

  countLams :: Expr Typed -> Integer
  countLams (ExprWrapper wrp _ _) = go wrp 0 where
    go S.TypeLam{} ac = ac + 1
    go (S.TypeLam{} S.:> xs) ac = go xs (ac + 1)
    go _ ac = ac
  countLams _ = 0

  countForalls :: Type -> Integer
  countForalls (ForallTy Relevant{} _ t) = 1 + countForalls t
  countForalls _ = 0

patternVars :: Pat -> [(Var Resolved, Type)]
patternVars (C.Capture v ty) = [(v, ty)]
patternVars (Destr _ p) = patternVars p
patternVars (PatExtend p ps) = patternVars p ++ concatMap (patternVars . snd) ps
patternVars Constr{} = []
patternVars PatLit{} = []

patternTyvars :: MonadLower m => Pattern Typed -> m [(Var Resolved, Type)]
patternTyvars = asks . flip (go . ctors)
  where
    go _ (S.Capture _ _) = []
    go _ (Wildcard _) = []
    go _ (Destructure _ Nothing _) = []
    go s (Destructure (TvName p) (Just t) (_, _)) =
      let tty' = lowerType (S.getType t)

          Just (skolm, ctty, _) = rootType mempty <$> Map.lookup p s
          Just uni =  unify ctty tty'
      in mapMaybe (extS skolm) (Map.toList uni) ++ go s t
    go s (PType p _ _) = go s p
    go s (PRecord xs _) = concatMap (go s . snd) xs
    go s (PTuple xs _) = concatMap (go s) xs
    go _ (PLiteral _ _) = []

    rootType fs (ForallTy Irrelevant f c) =
      let skolem = Map.restrictKeys fs (Set.difference (freeInTy f) (freeInTy c))
      in (skolem, f, c)
    rootType fs (ForallTy (Relevant v) f r) = rootType (Map.insert v f fs) r
    rootType _ _ = error "impossible constructor"

    extS sk (v, t) = case Map.lookup v sk of
                       Nothing -> Nothing
                       Just k -> case t of
                                   VarTy t' -> Just (t', k)
                                   _ -> error ("must replace skolem tyvar with tyvar " ++ show (pretty t))
