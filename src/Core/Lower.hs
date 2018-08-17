{-# LANGUAGE LambdaCase, TupleSections, ExplicitNamespaces,
    PatternSynonyms, RankNTypes, ScopedTypeVariables, FlexibleContexts,
    ConstraintKinds, OverloadedStrings #-}
module Core.Lower
  ( runLowerT, runLowerWithCtors
  , lowerExprTerm
  , lowerType
  , lowerPat
  , lowerProg
  ) where

import Control.Monad.Reader
import Control.Monad.Namey
import Control.Monad.State
import Control.Monad.Cont
import Control.Arrow
import Control.Lens

import qualified Data.VarMap as VarMap
import qualified Data.VarSet as VarSet
import qualified Data.Map as Map
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
import Core.Optimise (substituteInType, substituteInTys, fresh, freshFrom)
import Core.Core hiding (Atom, Term, Stmt, Type, Pattern, Arm)
import Core.Core (pattern Atom)
import Core.Types (unify, replaceTy)
import Core.Var

import qualified Syntax as S
import Syntax.Let
import Syntax.Var (Var(..), Resolved, Typed)
import Syntax (Expr(..), Pattern(..), Lit(..), Skolem(..), Toplevel(..), Constructor(..))
import Syntax.Transform

import Text.Pretty.Semantic (pretty)

type Atom = C.Atom CoVar
type Term = C.Term CoVar
type Type = C.Type CoVar
type Pat = C.Pattern CoVar
type Stmt = C.Stmt CoVar
type Arm = C.Arm CoVar

type Lower = ContT Term

data LowerState = LS { vars :: Map.Map (Var Resolved) Type
                     , ctors :: Map.Map (Var Resolved) Type
                     }
  deriving (Eq, Show)

type LowerTrack = VarMap.Map CoVar

type MonadLower m
  = ( MonadNamey m
    , MonadState LowerTrack m
    , MonadReader LowerState m )

runLowerT :: MonadNamey m => ReaderT LowerState (StateT LowerTrack m) a -> m a
runLowerT = runLowerWithCtors mempty

runLowerWithCtors :: MonadNamey m => Map.Map (Var Resolved) Type -> ReaderT LowerState (StateT LowerTrack m) a -> m a
runLowerWithCtors ct = flip evalStateT mempty . flip runReaderT (LS mempty ct)

errRef :: Atom
errRef = Ref C.vError
                (ForallTy (Relevant C.tyvarA) StarTy
                          (ForallTy Irrelevant C.tyString
                                 (VarTy C.tyvarA)))

patternMatchingFail :: MonadLower m => Span -> Type -> Type -> m Arm
patternMatchingFail w p t = do
  var <- fresh ValueVar
  tyApp <- fresh ValueVar
  let err = Lit (Str (T.pack ("Pattern matching failure at " ++ show (pretty w))))
      errTy = ForallTy Irrelevant C.tyString t
  pure C.Arm { _armPtrn = C.Capture var p, _armTy = p
             , _armBody = C.Let (One (tyApp, errTy, C.TyApp errRef t))
               (C.App (C.Ref tyApp errTy) err)
             , _armVars = [(var, p)], _armTyvars = []
             }

onAtom :: MonadLower m => Term -> Type -> Lower m Atom
onAtom (C.Atom a) _ = pure a
onAtom x' t = ContT $ \k -> fresh ValueVar >>= \v -> C.Let (One (v, t, x')) <$> k (C.Ref v t)

lowerAtAtom :: MonadLower m => Expr Typed -> Type -> Lower m Atom
lowerAtAtom x t = lowerAt x t >>= flip onAtom t

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
  vs' <- lowerLet vs
  let k = foldr ((.) . C.Let) id vs'
  k <$> lowerAtTerm t ty
lowerAt (S.If c t e _) ty = do
  c' <- lowerAtAtom c C.tyBool
  t' <- lowerAtTerm t ty
  e' <- lowerAtTerm e ty
  let tc = C.Arm (PatLit LitTrue) C.tyBool t' [] []
      te = C.Arm (PatLit LitFalse)  C.tyBool e' [] []
  pure $ C.Match c' [tc, te]
lowerAt (Fun param bd an) (ForallTy Irrelevant a b) =
  let p = case param of
        S.ImplParam p -> p
        S.PatParam p -> p
      operational (PType p _ _) = operational p
      operational p = p
   in case operational p of
        S.Capture (TvName v) _ -> Lam (TermArgument (mkVal v) a) <$> lowerAtTerm bd b
        _ -> do
          p' <- lowerPat p
          ts <- patternTyvars p
          bd' <- lowerAtTerm bd b
          arg <- fresh ValueVar
          fail <- patternMatchingFail (fst an) a b
          pure (Lam (TermArgument arg a) (C.Match (Ref arg a) [ C.Arm { _armPtrn = p', _armTy = a, _armBody = bd'
                                                                      , _armVars = patternVars p', _armTyvars = ts }
                                                              , fail ]))
lowerAt (Begin [x] _) t = lowerAt x t
lowerAt (Begin xs _) t = lowerAtTerm (last xs) t >>= flip (foldrM bind) (init xs) where
  bind e r = flip C.Let r . One <$> (build <$> fresh ValueVar <*> lowerBothTerm e)
  build a (b, c) = (a, c, b)
lowerAt (S.Match ex cs an) ty = do
  (ex', mt) <- lowerBothAtom ex
  cs' <- for cs $ \(pat, ex) -> do
    p' <- lowerPat pat
    ts <- patternTyvars pat
    ex' <- lowerAtTerm ex ty
    pure C.Arm { _armPtrn = p', _armTy = mt, _armBody = ex'
               , _armVars = patternVars p', _armTyvars = ts }
  fail <- patternMatchingFail (fst an) mt ty

  pure $ C.Match ex' (cs' ++ [fail])
lowerAt (Access r k _) ty = do
  (r', rt) <- lowerBothAtom r
  (iv, var) <- (,) <$> fresh ValueVar <*> fresh ValueVar
  let cotyRows t [] = t
      cotyRows t xs = RowsTy t xs
      inner =
        case rt of
          RowsTy t rs -> cotyRows t (deleteBy ((==) `on` fst) (k, undefined) rs)
          ExactRowsTy rs -> ExactRowsTy (deleteBy ((==) `on` fst) (k, undefined) rs)
          _ -> error ("not a row type " ++ show rt)
      match = C.Arm { _armPtrn = PatExtend (C.Capture iv inner) [ (k, C.Capture var ty) ]
                    , _armTy = rt, _armBody = Atom (Ref var ty )
                    , _armVars = [(iv, inner), (var, ty)], _armTyvars = []
                    }
  pure $ C.Match r' [match]

lowerAt (BinOp left op right a) t = lowerAt (S.App (S.App op left a) right a) t
lowerAt Hole{} _ = error "holes can't be lowered"

lowerAt (Tuple [x] _) t = lowerAt x t
lowerAt (Tuple (x:xs) _) (ExactRowsTy [(_, a), (_, b)]) = do
  x <- lowerAtAtom x a
  xs <- lowerAtAtom (Tuple xs undefined) b
  pure (Extend (Lit RecNil) [("_1", a, x), ("_2", b, xs)])

lowerAt (ExprWrapper wrap e an) ty =
  case wrap of
    S.WrapFn f -> lowerExprTerm (S.runWrapper f e)
    S.TypeAsc ty -> lowerExprTerm (Ascription e ty (fst an, ty))
    S.Cast S.ReflCo{} -> lowerAt e ty
    S.Cast c -> do
      ex' <- lowerExprAtom e
      pure (C.Cast ex' (squishCoercion (co c)))
    S.TypeApp t -> do
      ex' <- lowerAtAtom e (lowerType (S.getType e))
      pure (C.TyApp ex' (lowerType t))
    S.ExprApp x -> lowerExprTerm (S.App e x an)
    S.TypeLam (Skolem (TvName (TgName _ id)) (TvName (TgName n _)) _ _) k ->
      let ty' (ForallTy (Relevant v) _ t) = substituteInType (VarMap.singleton v (VarTy var)) t
          ty' x = x
          var = CoVar id n TypeVar
       in Lam (TypeArgument var (lowerType k)) <$> lowerAtTerm e (ty' ty)
    S.TypeLam _ _ -> error "impossible lowerAt TypeLam"
    ws S.:> wy -> lowerAt (ExprWrapper ws (ExprWrapper wy e an) an) ty
    S.WrapVar v -> error $ "Unsolved wrapper variable " ++ show v ++ ". This is a bug"
    S.IdWrap -> lowerAt e ty
  where
    co (S.VarCo (TvName x)) = CoercionVar (mkCo x)
    co (S.ReflCo t) = SameRepr (lowerType t) (lowerType t)
    co (S.AssumedCo t t') = SameRepr (lowerType t) (lowerType t')
    co (S.SymCo c) = Symmetry (co c)
    co (S.AppCo a b) = Application (co a) (co b)
    co (S.ArrCo a b) = C.Quantified Irrelevant (co a) (co b)
    co (S.ProdCo a b) = ExactRecord [("_1", co a), ("_2", co b)]
    co (S.RowsCo c rs) = C.Record (co c) (map (second co) rs)
    co (S.ExactRowsCo rs) = C.ExactRecord (map (second co) rs)
    co (S.ProjCo rs rs') = Projection (map (second mkReflexive) rs) (map (second co) rs')
    co (S.ForallCo (TvName v) cd rs) = C.Quantified (Relevant (mkCo v)) (co cd) (co rs)
    mkReflexive = join SameRepr . lowerType

lowerAt e _ = lowerAnyway e

lowerAnyway :: MonadLower m => Expr Typed -> Lower m Term
lowerAnyway (S.VarRef (TvName v) (_, ty)) = do
  let lty = lowerType ty
  env <- asks vars

  ctor <- asks (Map.member v . ctors)
  let kind = if ctor then DataConVar else ValueVar

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
                         let tyuni = fromMaybe (VarTy tyvar) (VarMap.lookup tyvar subst)
                             newTy = replaceTy tyvar tyuni prevTy
                         ftv <- fresh ValueVar
                         ContT $ \k ->
                           C.Let (One (ftv, newTy, TyApp prev tyuni)) <$> k (C.Ref ftv newTy, newTy)
                         ) (C.Ref (mkVar kind v) fty, fty) vars
          -- Otherwise just add our variable to the stripped list
          addApps (ForallTy (Relevant a) _ ty') vars = addApps ty' (a:vars)
          addApps _ _ = error "impossible"
      in runContT (addApps fty []) (pure . Atom . fst)

    -- If we're accessing a natively boxed operator, generate some stubs for it.
    -- This is horrible, and would be nicer as part of the stdlib, but this is
    -- the only solution for now.
    _ | TgInternal{} <- v
      , v' <- mkVar kind v
      , Just _ <- VarMap.lookup v' boxedTys -> do
          injects <- get
          Atom . flip Ref lty <$> case VarMap.lookup v' injects of
            Just e -> pure e
            Nothing -> do
              e <- freshFrom v'
              put (VarMap.insert v' e injects)
              pure e

      -- Just emit as normal
    _ -> pure (Atom (Ref (mkVar kind v) lty))

lowerAnyway (S.Record xs _) = case xs of
  [] -> pure (Atom (Lit RecNil))
  xs -> Extend (Lit RecNil) . zipWith build xs <$>
    traverse (lowerBothAtom . view S.fExpr) xs
  where build (S.Field name _ _) (atom, ty) = (name, ty, atom)
lowerAnyway (RecordExt e xs _) = do
  e' <- lowerExprAtom e
  xs' <- traverse (lowerBothAtom . view S.fExpr) xs
  pure $ Extend e' (zipWith build xs xs')
  where build (S.Field name _ _) (atom, ty) = (name, ty, atom)

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
  go (S.TyTuple a b) = ExactRowsTy [("_1", lowerType a), ("_2", lowerType b)]
  go x = lowerType x
lowerType (S.TyPi bind b)
  | S.Invisible v Nothing <- bind = ForallTy (Relevant (mkTyvar (S.unTvName v))) StarTy (lowerType b)
  | S.Invisible v (Just c) <- bind = ForallTy (Relevant (mkTyvar (S.unTvName v))) (lowerType c) (lowerType b)
  | S.Implicit a <- bind = ForallTy Irrelevant (lowerType a) (lowerType b)
  | S.Anon a <- bind = ForallTy Irrelevant (lowerType a) (lowerType b)
lowerType (S.TyApp a b) = AppTy (lowerType a) (lowerType b)
lowerType (S.TyRows rho vs) = RowsTy (lowerType rho) (map (fmap lowerType) vs)
lowerType (S.TyExactRows []) = NilTy
lowerType (S.TyExactRows vs) = ExactRowsTy (map (fmap lowerType) vs)
lowerType (S.TyVar (TvName v)) = VarTy (mkTyvar v)
lowerType (S.TyCon (TvName v)) = ConTy (mkType v)
lowerType (S.TyPromotedCon (TvName v)) = ConTy (mkCon v) -- TODO this is in the wrong scope
lowerType (S.TySkol (Skolem (TvName (TgName _ id)) (TvName (TgName n _)) _ _)) = VarTy (CoVar id n TypeVar)
lowerType (S.TySkol _) = error "impossible lowerType TySkol"
lowerType (S.TyWithConstraints _ t) = lowerType t
lowerType S.TyType = StarTy

lowerPat :: MonadLower m => Pattern Typed -> m Pat
lowerPat (S.Capture (TvName x) (_, t)) = pure (C.Capture (mkVal x) (lowerType t))
lowerPat (Wildcard (_, t)) = C.Capture <$> fresh ValueVar <*> pure( lowerType t)
lowerPat (Destructure (TvName p) Nothing _) = pure (Constr (mkCon p))
lowerPat (Destructure (TvName p) (Just t) _) = Destr (mkCon p) <$> lowerPat t
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
  in PatExtend <$> (C.Capture <$> fresh ValueVar <*> pure (tidy t)) <*> traverse lowerRow xs

lowerPat (PTuple [] _) = pure (PatLit Unit)
lowerPat (PTuple xs _) =
  let go [x] = lowerPat x
      go [] = error "no"
      go (x:xs) = do
        x' <- lowerPat x
        xs' <- go xs
        pure (PatExtend (PatLit RecNil) [("_1", x'), ("_2", xs')])
   in go xs
lowerPat (PLiteral l _) = pure (PatLit (lowerLiteral l))
lowerPat (PWrapper _ p _) = lowerPat p


lowerProg :: forall m. MonadLower m => [Toplevel Typed] -> m [Stmt]
lowerProg stmt = do
  stmt' <- lowerProg' stmt
  ops <- gets VarMap.toList
  foldrM (\x xs -> (:xs) <$> genOp x) stmt' ops
  where
    genOp (op, var) = do
      let Just ty = VarMap.lookup op boxedTys
      (body, ty') <- genWrapper id (Ref op ty) ty
      pure . StmtLet . One $ (var, ty', body)

    genWrapper :: (Term -> Term) -> Atom -> Type -> m (Term, Type)
    genWrapper build bod  (ForallTy (Relevant a) l r) = do
      -- Generate forall wrapper
      t <- fresh TypeVar
      v <- fresh ValueVar

      let r' = substituteInType (VarMap.singleton a (VarTy t)) r
      (bod', ty') <- genWrapper (build . C.Let (One (v, r', TyApp bod (VarTy t))))
                       (Ref v r') r'
      pure ( Lam (TypeArgument t l) bod'
           , ForallTy (Relevant a) l ty' )

    genWrapper build bod (ForallTy Irrelevant t@(ValuesTy ts) r) = do
      -- Generate nested functions for each unboxed argument
      tvars <- traverse (\t -> (,t) <$> fresh ValueVar) ts
      tvar  <- fresh ValueVar

      pure ( foldr (\(v, ty) -> Lam (TermArgument v ty))
             (build (C.Let (One (tvar, t, Values (map (uncurry Ref) tvars)))
                     (C.App bod (Ref tvar t))))
             tvars
           , foldr (ForallTy Irrelevant) r ts )

    genWrapper build bod ty = pure (build (Atom bod), ty)

lowerProg' :: forall m. MonadLower m => [Toplevel Typed] -> m [Stmt]
lowerProg' [] = pure []
lowerProg' (ForeignVal (TvName v) ex tp _:prg) =
  (Foreign (mkVal v) (lowerType tp) ex:) <$> lowerProg' prg
lowerProg' (LetStmt vs:prg) = do
  let env' = Map.fromList (foldMap lowerBind vs)
      lowerBind bind =
        let ty = lowerType (bind ^. (S.bindAnn . _2))
        in map (\(TvName v) -> (v, ty)) (bindVariables bind)

  local (\s -> s { vars = env' }) $ do
    vs' <- lowerLet vs
    foldr ((.) . ((:) . C.StmtLet)) id vs' <$> lowerProg' prg
lowerProg' (TypeDecl (TvName var) _ cons:prg) = do
  let cons' = map (\case
                       UnitCon (TvName p) (_, t) -> (p, mkCon p, lowerType t)
                       ArgCon (TvName p) _ (_, t) -> (p, mkCon p, lowerType t)
                       GeneralisedCon (TvName p) t _ -> (p, mkCon p, lowerType t))
                cons
      ccons = map (\(_, a, b) -> (a, b)) cons'
      scons = map (\(a, _, b) -> (a, b)) cons'

  (C.Type (mkType var) ccons:) <$> local (\s -> s { ctors = Map.union (Map.fromList scons) (ctors s) }) (lowerProg' prg)
lowerProg' (Open _ _:prg) = lowerProg' prg
lowerProg' (Module _ b:prg) = lowerProg' (b ++ prg)

lowerLet :: MonadLower m => [S.Binding Typed] -> m [Binding CoVar]
lowerLet bs =
  let sccs = depOrder bs

      lowerScc (CyclicSCC vs) = pure . Many <$> do
        -- Cyclic bindings will only ever be normal. Well, I
        -- jolly hope so anyway
        for vs $ \(S.Binding (TvName var) ex _ (_, ty)) -> do
          let ty' = lowerType ty
          (mkVal var,ty',) <$> lowerPolyBind ty' ex

      lowerScc (AcyclicSCC (S.Binding (TvName var) ex _ (_, ty))) = pure . One <$> do
        let ty' = lowerType ty
        (mkVal var, ty',) <$> lowerPolyBind ty' ex

      lowerScc (AcyclicSCC S.ParsedBinding{}) = error "ParsedBinding{} in Lower"
      lowerScc (AcyclicSCC S.Matching{}) = error "Matching{} in Lower"

      lowerScc (AcyclicSCC (S.TypedMatching p ex (pos, ty) bound)) = do
        let ty' = lowerType ty
            boundVarMap = Map.fromList bound
        ts <- patternTyvars p
        ex' <- lowerPolyBind ty' ex

        case boundWith p of
          [] -> do
            -- Generate `let x = match expr with | ... -> ()
            var <- fresh ValueVar
            pure . One . (var, C.tyUnit, ) <$> patternWrap pos p ts ex' ty' C.tyUnit (Atom (Lit Unit)) C.tyUnit
          [bind] ->
            -- Generate `let x = match expr with | ... x' ... -> x`
            pure <$> patternExtract pos p ts ex' ty' (lowerType (boundVarMap Map.! fst bind)) bind
          vs -> do
            var <- fresh ValueVar
            let cont bind = patternExtract pos p ts (Atom (Ref var ty')) ty' (lowerType (boundVarMap Map.! fst bind)) bind
            -- Generate `let a = expr`
            (One (var, ty', ex'):)
              <$> traverse cont vs

      -- | Strip all variables from a pattern aside from the given one,
      -- which is replaced with @n@
      stripPtrn v n = transformPatternTyped go id where
        go (S.Capture v' a) | v == v' = S.Capture n a
                            | otherwise = S.Wildcard a
        go p = p

      patternExtract :: MonadLower m
                     => Span -> Pattern Typed -> [(CoVar, Type)]
                     -- ^ The pattern's position, the pattern and all bound vars
                     -> Term -> Type -- ^ The variable we're binding from and its type
                     -> Type -- ^ The quantified variable we're binding to's type
                     -> (Var Typed, S.Ann Typed) -- ^ The pattern variable we're binding to
                     -> m (Binding CoVar)
      patternExtract pos p ts test ty outerTy (TvName var, (_, innerTy)) = do
        let var' = mkVal var
            innerTy' = lowerType innerTy
        pvar@(CoVar vn vt _) <- freshFrom var'
        let p' = stripPtrn (TvName var) (TvName (TgName vt vn)) p

        -- Generate `let x = match test with | ... x' ... -> x`
        One  . (var', outerTy, ) <$> patternWrap pos p' ts test ty outerTy (Atom (Ref pvar innerTy')) innerTy'

      -- | Wrap an expression in a pattern match, generating the
      -- appropriate type lambdas and performing correct substitutions.
      patternWrap :: MonadLower m
                  => Span -> Pattern Typed -> [(CoVar, Type)]
                     -- ^ The pattern's position, the pattern and all bound vars
                  -> Term -> Type -- ^ The variable we're binding from and its type
                  -> Type -- ^ The quantified variable we're binding to's type
                  -> Term -> Type -- ^ The inner term and its resulting type
                  -> m Term
      patternWrap pos p ts test ty outerTy inner innerTy = do
        p' <- lowerPat p

        -- Generate `let x = match test with | ... x' ... -> x`
        flip runContT pure $ do
          test' <- onAtom test ty
          genWrapper id mempty test' ty (requiredVars outerTy) $ \subst ptrnTy res -> do
            fail <- patternMatchingFail pos ptrnTy innerTy

            -- We substitute the whole match to use our new type arguments, as it's
            -- easier than substituting each pattern + pattern binds
            pure . substituteInTys subst $ C.Match res
              [ C.Arm { _armPtrn = p', _armTy = ptrnTy, _armBody = inner
                      , _armVars = patternVars p', _armTyvars = ts }
              , fail ]


      requiredVars :: Type -> VarSet.Set
      requiredVars (ForallTy (Relevant v) _ rest) = VarSet.insert v (requiredVars rest)
      requiredVars _ = mempty

      genWrapper :: MonadLower m
                 => (Term -> Term) -- ^ Wraps the inner lambda body
                 -> VarMap.Map Type -- ^ Accumulator tyvar substitution map
                 -> Atom -> Type -- ^ The variable and type we'll generate against
                 -> VarSet.Set -- ^ "Interesting" tyvars we should generate lambdas for
                 -> (VarMap.Map Type -> Type -> Atom -> m Term)
                 -> m Term
      genWrapper wrap subst bod (ForallTy (Relevant a) l r) vs result
        | VarSet.member a vs = do
            -- If we're an interesting tyvar (we appear in the forall) then
            -- generate a type lambda.
            t <- freshFrom a
            v <- fresh ValueVar
            Lam (TypeArgument t l) <$> worker v (VarTy t)
        | otherwise = do
            -- Otherwise just replace with unit
            v <- fresh ValueVar
            worker v C.tyUnit
        where
          -- | The worker performs a tyapp within the lambda body (the @wrap@
          -- function), extends the variable substitution and visits the inner
          -- type.
          worker bind rTy =
            let r' = substituteInType (VarMap.singleton a rTy) r
            in genWrapper (wrap . C.Let (One (bind, r', TyApp bod rTy)))
                          (VarMap.insert a rTy subst)
                          (Ref bind r') r' vs result

      genWrapper wrap subst bod ty _ res = wrap <$> res subst ty bod

  in concat <$> traverse lowerScc sccs

lowerPolyBind :: MonadLower m => Type -> Expr Typed -> m Term
lowerPolyBind ty ex = doIt (needed ex ty) (go ty ex) (lowerExprTerm ex) where
  go _ ex 0 = lowerExprTerm ex
  go (ForallTy (Relevant v) kind ty) ex n
    | n >= 1 = Lam (TypeArgument v kind) <$> go ty ex (n - 1)
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

patternVars :: Pat -> [(CoVar, Type)]
patternVars (C.Capture v ty) = [(v, ty)]
patternVars (Destr _ p) = patternVars p
patternVars (PatExtend p ps) = patternVars p ++ concatMap (patternVars . snd) ps
patternVars (PatValues ps) = concatMap patternVars ps
patternVars Constr{} = []
patternVars PatLit{} = []

patternTyvars :: MonadLower m => Pattern Typed -> m [(CoVar, Type)]
patternTyvars = asks . flip (go . ctors)
  where
    go _ (S.Capture _ _) = []
    go _ (Wildcard _) = []
    go _ (Destructure _ Nothing _) = []
    go s (Destructure (TvName p) (Just t) (_, _)) =
      let tty' = lowerType (S.getType t)

          Just (skolm, ctty, _) = rootType mempty <$> Map.lookup p s
          Just uni =  unify ctty tty'
      in mapMaybe (extS skolm) (VarMap.toList uni) ++ go s t
    go s (PType p _ _) = go s p
    go s (PRecord xs _) = concatMap (go s . snd) xs
    go s (PTuple xs _) = concatMap (go s) xs
    go _ (PLiteral _ _) = []
    go s (PWrapper _ p _) = go s p

    rootType fs (ForallTy Irrelevant f c) =
      let d = VarSet.difference (freeInTy f) (freeInTy c)
          skolem = Map.filterWithKey (\v _ -> v `VarSet.member` d) fs
      in (skolem, f, c)
    rootType fs (ForallTy (Relevant v) f r) = rootType (Map.insert v f fs) r
    rootType _ _ = error "impossible constructor"

    extS sk (v, t) = case Map.lookup v sk of
                       Nothing -> Nothing
                       Just k -> case t of
                                   VarTy t' -> Just (t', k)
                                   _ -> error ("must replace skolem tyvar with tyvar " ++ show (pretty t))

mkTyvar, mkVal, mkType, mkCo, mkCon :: Var Resolved -> CoVar
mkTyvar = mkVar TypeVar
mkVal = mkVar ValueVar
mkType = mkVar TypeConVar
mkCo = mkVar CastVar
mkCon = mkVar DataConVar

mkVar :: VarInfo -> Var Resolved -> CoVar
mkVar k (TgName t i) = CoVar i t k
mkVar k (TgInternal name) = CoVar (builtin name) name k where
  builtin name = fromMaybe (error ("Cannot find builtin " ++ show name)) (Map.lookup name builtins)

  builtins = Map.fromList (map ex C.builtinTyList
                           ++ map (ex . fst) (C.builtinVarList :: [(CoVar, Type)])
                           ++ map ex [C.tyvarA, C.tyvarB]
                          )

  ex :: CoVar -> (T.Text, Int)
  ex (CoVar v n _) = (n, v)

boxedTys :: VarMap.Map Type
boxedTys = VarMap.fromList
           . filter (flip VarSet.member boxed . fst)
           $ C.builtinVarList where
  boxed = VarSet.fromList
    [ C.vOpAdd, C.vOpSub, C.vOpMul, C.vOpDiv, C.vOpExp
    , C.vOpLt,  C.vOpGt,  C.vOpLe,  C.vOpGe

    , C.vOpAddF, C.vOpSubF, C.vOpMulF, C.vOpDivF, C.vOpExpF
    , C.vOpLtF,  C.vOpGtF,  C.vOpLeF,  C.vOpGeF

    , C.vOpConcat

    , C.vOpEq, C.vOpNe
    ]
