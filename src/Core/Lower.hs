{-# LANGUAGE TupleSections,
    PatternSynonyms, RankNTypes, ScopedTypeVariables, FlexibleContexts,
    ConstraintKinds, OverloadedStrings, TypeFamilies #-}
module Core.Lower
  ( LowerState, defaultState
  , runLowerT, runLowerWithEnv
  , lowerExprTerm
  , lowerType
  , lowerProg, lowerProgEnv
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
import Data.Foldable
import Data.Maybe
import Data.Graph
import Data.Span

import qualified Core.Core as C
import qualified Core.Builtin as C
import Core.Optimise (substituteInType, substituteInTys, fresh, freshFrom)
import Core.Core hiding (Term, Stmt, Pattern, Arm)
import Core.Types (unify, unifyClosed, replaceTy)
import Core.Lower.TypeRepr
import Core.Lower.Pattern
import Core.Lower.Basic
import Core.Intrinsic
import Core.Var

import qualified Syntax as S
import Syntax.Let
import Syntax.Var (Var, Typed, VarResolved(..))
import Syntax.Transform
import Syntax (Expr(..), Pattern(..), Skolem(..), ModuleTerm(..), Toplevel(..), Arm(..))

import Text.Pretty.Semantic (pretty)

type Term = C.Term CoVar
type Stmt = C.Stmt CoVar

type Lower = ContT Term

defaultState :: LowerState
defaultState = LS mempty ctors types where
  ctors :: VarMap.Map C.Type
  ctors = VarMap.fromList
           [ (C.vCONS,
              ForallTy (Relevant name) StarTy $
                VarTy name `prodTy` AppTy C.tyList (VarTy name) `arrTy` AppTy C.tyList (VarTy name))
           , (C.vNIL, ForallTy (Relevant name) StarTy $ AppTy C.tyList (VarTy name))]
  types :: VarMap.Map TypeRepr
  types = VarMap.fromList
            ( (C.vList, SumTy (VarSet.fromList [C.vCONS, C.vNIL]))
            : map (,OpaqueTy) [ C.vBool, C.vInt, C.vString, C.vFloat, C.vUnit
                              , C.vLazy, C.vArrow, C.vProduct, C.vRefTy ] )
  name = C.tyvarA
  arrTy = ForallTy Irrelevant
  prodTy a b = RowsTy NilTy [("_1", a), ("_2", b)]

runLowerT :: MonadNamey m => ReaderT LowerState (StateT LowerTrack m) a -> m a
runLowerT = runLowerWithEnv defaultState

-- | Run lower with a given state
-- to constructors.
runLowerWithEnv :: MonadNamey m
                  => LowerState
                  -> ReaderT LowerState (StateT LowerTrack m) a
                  -> m a
runLowerWithEnv ls = flip evalStateT mempty . flip runReaderT ls

errRef :: Atom
errRef = Ref C.vError
                (ForallTy (Relevant C.tyvarA) StarTy
                          (ForallTy Irrelevant C.tyString
                                 (VarTy C.tyvarA)))

patternMatchingError :: MonadLower m => String -> Span -> Type -> m Term
patternMatchingError str w t = do
  tyApp <- fresh ValueVar
  let err = Lit (Str (T.pack ("Pattern matching failure in " ++ str ++ " at " ++ show (pretty w))))
      errTy = ForallTy Irrelevant C.tyString t
  pure $ C.Let (One (tyApp, errTy, C.TyApp errRef t)) (C.App (C.Ref tyApp errTy) err)

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

lowerAt (S.Let _ vs t _) ty = do
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
  let p = param ^. S.paramPat
      operational (PType p _ _) = operational p
      operational p = p
   in case operational p of
        S.Capture v _ -> Lam (TermArgument (mkVal v) a) <$> lowerAtTerm bd b
        _ -> do
          bd' <- lowerAtTerm bd b
          arg <- freshFromPat p
          fail <- patternMatchingError "pattern-matching function" (fst an) b
          Lam (TermArgument arg a) <$> lowerMatch' arg a [ (p, Nothing, bd')
                                                         , (S.Wildcard undefined, Nothing, fail) ]

lowerAt x@Fun{} t = error ("lower function " ++ show (pretty x) ++ " at " ++ show (pretty t))

lowerAt (Begin [x] _) t = lowerAt x t
lowerAt (Begin xs _) t = lowerAtTerm (last xs) t >>= flip (foldrM bind) (init xs) where
  bind e r = flip C.Let r . One <$> (build <$> fresh ValueVar <*> lowerBothTerm e)
  build a (b, c) = (a, c, b)

lowerAt (S.Match ex cs _ an) ty = do
  (ex', _) <- lowerBothAtom ex
  cs' <- for cs (\(Arm pat g e _) -> (pat,,)
                  <$> traverse (`lowerAtTerm` C.tyBool) g
                  <*> lowerAtTerm e ty)
  fail <- patternMatchingError "match expression" (fst an) ty
  lowerMatch ex' (cs' ++ [(S.Wildcard undefined, Nothing, fail)])

lowerAt (Access r k _) ty = do
  (r', rt) <- lowerBothAtom r
  var <- fresh ValueVar
  let match = C.Arm { _armPtrn = PatRecord [(k, C.Capture var ty)]
                    , _armTy = rt, _armBody = Atom (Ref var ty )
                    , _armVars = [(var, ty)], _armTyvars = []
                    }
  pure $ C.Match r' [match]

lowerAt (BinOp left op right a) t = lowerAt (S.App (S.App op left a) right a) t
lowerAt Hole{} _ = error "holes can't be lowered"

lowerAt (S.OpenIn _ x _) t = lowerAt x t

lowerAt (Tuple [x] _) t = lowerAt x t
lowerAt (Tuple (x:xs) _) (ExactRowsTy [(_, a), (_, b)]) = do
  x <- lowerAtAtom x a
  xs <- lowerAtAtom (Tuple xs undefined) b
  pure (Extend (Lit RecNil) [("_1", a, x), ("_2", b, xs)])

lowerAt (ExprWrapper wrap e an) ty =
  case wrap of
    S.WrapFn f -> lowerExprTerm (S.runWrapper f e)
    S.TypeAsc ty -> lowerExprTerm (Ascription e ty (fst an, ty))
    S.ExprApp f -> lowerAt (S.App e f an) ty
    S.Cast c -> do
      let from = lowerType (S.getType e)
      ex' <- lowerAt e from
      if ty `unifyClosed` from
      then pure ex'
      else (\x -> C.Cast x ty (squishCoercion (co c))) <$> onAtom ex' from
    S.TypeApp t -> do
      ex' <- lowerAtAtom e (lowerType (S.getType e))
      pure (C.TyApp ex' (lowerType t))
    S.TypeLam (Skolem (TgName _ id) (TgName n _) _ _) k ->
      let ty' (ForallTy (Relevant v) _ t) = substituteInType (VarMap.singleton v (VarTy var)) t
          ty' x = x
          var = CoVar id (Just n) TypeVar
       in Lam (TypeArgument var (lowerType k)) <$> lowerAtTerm e (ty' ty)
    S.TypeLam _ _ -> error "impossible lowerAt TypeLam"
    ws S.:> wy -> lowerAt (ExprWrapper ws (ExprWrapper wy e an) an) ty
    S.WrapVar v -> error $ "Unsolved wrapper variable " ++ show v ++ ". This is a bug"
    S.IdWrap -> lowerAt e ty

lowerAt e _ = lowerAnyway e

co :: S.Coercion Typed -> Coercion
co (S.VarCo x) = CoercionVar (mkCo x)
co (S.ReflCo t) = SameRepr (lowerType t) (lowerType t)
co (S.AssumedCo t t') = SameRepr (lowerType t) (lowerType t')
co (S.SymCo c) = Symmetry (co c)
co (S.TransCo x y) = Trans (co x) (co y)
co (S.AppCo a b) = Application (co a) (co b)
co (S.ArrCo a b) = C.Quantified Irrelevant (co a) (co b)
co (S.ProdCo a b) = ExactRecord [("_1", co a), ("_2", co b)]
co (S.RowsCo c rs) = C.Record (co c) (map (second co) rs)
co (S.ExactRowsCo rs) = C.ExactRecord (map (second co) rs)
co (S.ProjCo rs rs') = Projection (map (second mkReflexive) rs) (map (second co) rs') where
  mkReflexive = join SameRepr . lowerType
co (S.ForallCo v _ cd rs) = C.Quantified (Relevant (mkCo v)) (co cd) (co rs)
co (S.P1 v) = Nth (mkCo v) 0
co (S.P2 v) = Nth (mkCo v) 1
co S.MvCo{} = error "Unsolved coercion metavariable"
co (S.InstCo ax ts) = Axiom (mkCo ax) (map co ts)

lowerAnyway :: MonadLower m => Expr Typed -> Lower m Term
lowerAnyway (S.VarRef v (_, ty)) = do
  let lty = lowerType ty
  env <- asks vars

  ctor <- asks (VarMap.member (mkCon v) . ctors)
  let kind = if ctor then DataConVar else ValueVar
      v' = mkVar kind v

  case VarMap.lookup v' env of
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
                         ) (C.Ref v' fty, fty) vars
          -- Otherwise just add our variable to the stripped list
          addApps (ForallTy (Relevant a) _ ty') vars = addApps ty' (a:vars)
          addApps _ _ = error "impossible"
      in runContT (addApps fty []) (pure . Atom . fst)

    -- If we're accessing a natively boxed operator, generate some stubs for it.
    -- This is horrible, and would be nicer as part of the stdlib, but this is
    -- the only solution for now.
    _ | TgName _ n <- v, n < 0
      , Just _ <- VarMap.lookup v' boxedTys -> do
          injects <- get
          Atom . flip Ref lty <$> case VarMap.lookup v' injects of
            Just e -> pure e
            Nothing -> do
              e <- freshFrom v'
              put (VarMap.insert v' e injects)
              pure e

      -- Just emit as normal
    _ -> pure (Atom (Ref v' lty))

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

lowerAnyway (S.OpenIn _ x _) =
  -- This is safe to do, as we know x will only be a reference/load, and so has
  -- been lowered already.
  lowerAnyway x

lowerAnyway (Literal l _) = pure . Atom . Lit $ lowerLiteral l
lowerAnyway (S.App f x _) = C.App <$> lowerExprAtom f <*> lowerExprAtom x

lowerAnyway e = error ("can't lower " ++ show (pretty e) ++ " without type")

lowerProg :: MonadLower m => [Toplevel Typed] -> m [Stmt]
lowerProg = fmap snd . lowerProgEnv

lowerProgEnv :: MonadLower m => [Toplevel Typed] -> m (LowerState, [Stmt])
lowerProgEnv stmt = do
  (ls, stmt') <- lowerProg' stmt
  ops <- gets VarMap.toList
  (ls,) <$> foldrM (\x xs -> (:xs) <$> genOp x) stmt' ops
  where
    genOp (op, var) = do
      let Just ty = VarMap.lookup op boxedTys
      (body, ty') <- lowerBoxedFun (Ref op ty) ty
      pure . StmtLet . One $ (var, ty', body)

(<$$>) :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(<$$>) = (<$>) . fmap

lowerModule :: forall m. MonadLower m => ModuleTerm Typed -> m (LowerState, [Stmt])
lowerModule (ModStruct ms _) = lowerProg' ms
lowerModule ModRef{} = asks (,[])
lowerModule ModImport{} = asks (,[])
lowerModule ModTargetImport{} = asks (,[])

lowerProg' :: forall m. MonadLower m => [Toplevel Typed] -> m (LowerState, [Stmt])

lowerProg' [] = asks (,[])

lowerProg' (Include m:prg) = do
  (s, ms) <- lowerModule m
  (ms++) <$$> local (const s) (lowerProg' prg)

lowerProg' (Open m:prg) = do
  (s, ms) <- lowerModule m
  (ms++) <$$> local (const s) (lowerProg' prg)

lowerProg' (Module _ _ m:prg) = do
  (s, ms) <- lowerModule m
  (ms++) <$$> local (const s) (lowerProg' prg)

-- ∨ TC desugars all of these to TypeDecl + Let
lowerProg' (Class{}:prg) = lowerProg' prg
lowerProg' (Instance{}:prg) = lowerProg' prg
lowerProg' (TySymDecl{}:prg) = lowerProg' prg
lowerProg' (TypeFunDecl{}:prg) = lowerProg' prg
lowerProg' (DeriveInstance{}:prg) = lowerProg' prg

lowerProg' (ForeignVal _ v ex tp _:prg) =
  let tyB = lowerType tp
      vB = mkVal v
      ex' = case intrinsicOf ex of
        Nothing -> ForeignCode ex
        Just i -> Intrinsic i
  in case unboxedTy tyB of
       Nothing -> (Foreign vB tyB ex':) <$$> lowerProg' prg
       Just tyU -> do
         vU <- freshFrom vB
         (wrap, _) <- lowerBoxedFun (Ref vU tyU) tyU
         ([ Foreign vU tyU ex'
          , StmtLet (One (vB, tyB, wrap))
          ]++) <$$> lowerProg' prg

  where
    unboxedTy (ForallTy (Relevant v) l r) = ForallTy (Relevant v) l <$> unboxedTy r
    unboxedTy (ForallTy Irrelevant l r) = unwrap [l] r
    unboxedTy _ = Nothing

    unwrap as (ForallTy Irrelevant a r) = unwrap (a:as) r
    unwrap [_] _ = Nothing
    unwrap as r = Just (ForallTy Irrelevant (ValuesTy (reverse as)) r)


lowerProg' (LetStmt _ _ vs _:prg) = do
  let env' = VarMap.fromList (foldMap lowerBind vs)
      lowerBind bind =
        let ty = lowerType (bind ^. (S.bindAnn . _2))
        in map (\v -> (mkVal v, ty)) (bindVariables bind)

  local (\s -> s { vars = env' }) $ do
    vs' <- lowerLet vs
    foldr ((.) . ((:) . C.StmtLet)) id vs' <$$> lowerProg' prg

lowerProg' (TypeDecl _ var _ cons  _:prg) = do
  ~(tyStmts@(C.Type _ cs:_), repr) <- getTypeRepr (mkType var) cons
  (tyStmts++) <$$> local (\s ->
    s { ctors = VarMap.union (VarMap.fromList cs) (ctors s)
      , types = VarMap.insert (mkType var) repr (types s)
      }) (lowerProg' prg)

lowerLet :: MonadLower m => [S.Binding Typed] -> m [Binding CoVar]
lowerLet bs =
  let sccs = depOrder bs

      lowerScc (CyclicSCC vs) = pure . Many <$> do
        -- Cyclic bindings will only ever be normal. Well, I
        -- jolly hope so anyway
        for vs $ \(S.Binding var _ ex _ (_, ty)) -> do
          let ty' = lowerType ty
          (mkVal var,ty',) <$> lowerPolyBind ty' ex

      lowerScc (AcyclicSCC (S.Binding var _ ex _ (_, ty))) = pure . One <$> do
        let ty' = lowerType ty
        (mkVal var, ty',) <$> lowerPolyBind ty' ex

      lowerScc (AcyclicSCC S.Matching{}) = error "Matching{} in Lower"

      lowerScc (AcyclicSCC (S.TypedMatching p ex (pos, ty) bound)) = do
        let ty' = lowerType ty
            boundVarMap = Map.fromList bound
        ex' <- lowerPolyBind ty' ex

        case boundWith p of
          [] -> do
            -- Generate `let x = match expr with | ... -> ()
            var <- fresh ValueVar
            pure . One . (var, C.tyUnit, ) <$> patternWrap pos p ex' ty' C.tyUnit (Atom (Lit Unit)) C.tyUnit
          [bind] ->
            -- Generate `let x = match expr with | ... x' ... -> x`
            pure <$> patternExtract pos p ex' ty' (lowerType (boundVarMap Map.! fst bind)) bind
          vs -> do
            var <- fresh ValueVar
            let cont bind =
                  patternExtract pos p (Atom (Ref var ty')) ty' (lowerType (boundVarMap Map.! fst bind)) bind
            -- Generate `let a = expr`
            (One (var, ty', ex'):)
              <$> traverse cont vs

      -- | Strip all variables from a pattern aside from the given one,
      -- which is replaced with @n@
      stripPtrn v n = transformPatternTyped go id where
        go (S.Capture v' a) | v == v' = S.Capture n a
                            | otherwise = S.Wildcard a
        go (S.PAs p v' a) | v == v' = S.PAs p n a
                          | otherwise = p
        go p = p

      patternExtract :: MonadLower m
                     => Span -> Pattern Typed
                     -- ^ The pattern's position and the pattern
                     -> Term -> Type -- ^ The variable we're binding from and its type
                     -> Type -- ^ The quantified variable we're binding to's type
                     -> (Var Typed, S.Ann Typed) -- ^ The pattern variable we're binding to
                     -> m (Binding CoVar)
      patternExtract pos p test ty outerTy (var, (_, innerTy)) = do
        let var' = mkVal var
            innerTy' = lowerType innerTy
        pvar@(CoVar vn _ _) <- freshFrom var'
        let p' = stripPtrn var (TgName (covarDisplayName pvar) vn) p

        -- Generate `let x = match test with | ... x' ... -> x`
        One  . (var', outerTy, ) <$> patternWrap pos p' test ty outerTy (Atom (Ref pvar innerTy')) innerTy'

      -- | Wrap an expression in a pattern match, generating the
      -- appropriate type lambdas and performing correct substitutions.
      patternWrap :: MonadLower m
                  => Span -> Pattern Typed
                     -- ^ The pattern's position and the pattern
                  -> Term -> Type -- ^ The variable we're binding from and its type
                  -> Type -- ^ The quantified variable we're binding to's type
                  -> Term -> Type -- ^ The inner term and its resulting type
                  -> m Term
      patternWrap pos p test ty outerTy inner innerTy =
        -- Generate `let x = match test with | ... x' ... -> x`
        flip runContT pure $ do
          test' <- onAtom test ty
          genWrapper id mempty test' ty (requiredVars outerTy) $ \subst res -> do
            -- We substitute the whole match to use our new type arguments, as it's
            -- easier than substituting each pattern + pattern binds
            fail <- patternMatchingError "let expression" pos innerTy
            substituteInTys subst <$> lowerMatch res [ (p, Nothing, inner)
                                                     , (S.Wildcard undefined, Nothing, fail) ]

      requiredVars :: Type -> VarSet.Set
      requiredVars (ForallTy (Relevant v) _ rest) = VarSet.insert v (requiredVars rest)
      requiredVars _ = mempty

      genWrapper :: MonadLower m
                 => (Term -> Term) -- ^ Wraps the inner lambda body
                 -> VarMap.Map Type -- ^ Accumulator tyvar substitution map
                 -> Atom -> Type -- ^ The variable and type we'll generate against
                 -> VarSet.Set -- ^ "Interesting" tyvars we should generate lambdas for
                 -> (VarMap.Map Type -> Atom -> m Term)
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

      genWrapper wrap subst bod _ _ res = wrap <$> res subst bod

  in concat <$> traverse lowerScc sccs

-- | Generate a wrapper for some function which takes an unboxed tuple
--
-- This takes the original function's name and type, and returns the new
-- name and type.
lowerBoxedFun :: forall m. MonadLower m => Atom -> Type -> m (Term, Type)
lowerBoxedFun = genWrapper id where
  genWrapper :: (Term -> Term) -> Atom -> Type -> m (Term, Type)
  genWrapper build bod (ForallTy (Relevant a) l r) = do
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

lowerPolyBind :: MonadLower m => Type -> Expr Typed -> m Term
lowerPolyBind ty ex = doIt (needed ex ty) (go ty ex) (lowerExprTerm ex) where
  go _ ex 0 = lowerExprTerm ex
  go (ForallTy (Relevant v) kind ty) ex n
    | n >= 1 = Lam (TypeArgument v kind) <$> go ty ex (n - 1)
  go _ _ _ = error "impossible"

  needed ex ty
    | countForalls ty > countLams ex = Just (countForalls ty - countLams ex)
    | otherwise = Nothing -- trust tc

  doIt x a b = maybe b a x

  countLams :: Expr Typed -> Integer
  countLams (ExprWrapper wrp e _) = go wrp (countLams e) where
    go S.TypeLam{} ac = ac + 1
    go (S.TypeLam{} S.:> xs) ac = go xs (ac + 1)
    go _ ac = ac
  countLams (Ascription e _ _) = countLams e
  countLams _ = 0

countForalls :: Type -> Integer
countForalls (ForallTy Relevant{} _ t) = 1 + countForalls t
countForalls _ = 0

boxedTys :: VarMap.Map Type
boxedTys = VarMap.fromList
           . filter (flip VarSet.member boxed . fst)
           $ C.builtinVarList where
  boxed = VarSet.fromList
    [ C.vOpApp, C.vAssign, C.vExtend, C.vRestrict
    , C.tcEqTypeRep
    , C.tcTypeableApp, C.tcTypeableKnownKnown
    ]
