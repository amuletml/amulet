{-# LANGUAGE FlexibleContexts, TupleSections, GADTs #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
module Types.Infer
  ( inferProgram
  , builtinsEnv
  , closeOver
  , tyString, tyInt, tyBool, tyUnit, tyFloat

  , infer, check
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Traversable
import Data.Generics
import Data.Triple
import Data.Graph
import Data.Span

import Control.Monad.State
import Control.Monad.Infer
import Control.Arrow (first, (***))
import Control.Lens

import Syntax.Resolve.Toplevel
import Syntax.Subst
import Syntax.Raise
import Syntax.Let
import Syntax

import Types.Infer.Constructor
import Types.Infer.Pattern
import Types.Infer.Builtin
import Types.Wellformed
import Types.Unify
import Types.Kinds

import Pretty

-- Solve for the types of lets in a program
inferProgram :: MonadGen Int m => [Toplevel Resolved] -> m (Either TypeError ([Toplevel Typed], Env))
inferProgram ct = fmap fst <$> runInfer builtinsEnv (inferProg ct)

mkTyApps :: Applicative f
         => Expr Resolved
         -> Map.Map (Var Typed) (Type Typed)
         -> Type Typed
         -> Type Typed
         -> f (Expr Typed, Type Typed)
mkTyApps (VarRef k a) mp (TyForall vs c) _ = pure (insts (reverse vs)) where
  insts []     = (VarRef (TvName k) (a, c), c)
  insts (x:xs) = let (e, ty) = insts xs
                     s = mp Map.! x
                     ty' = apply (Map.singleton x s) ty
                 in (TypeApp e s (a, ty'), ty')
mkTyApps _ _ _ _ = undefined

correct :: Type Typed -> Expr Typed -> Expr Typed
correct ty = gmapT (mkT go) where
  go :: Ann Typed -> Ann Typed
  go (a, _) = (a, ty)

check :: MonadInfer Typed m => Expr Resolved -> Type Typed -> m (Expr Typed)
check e ty@TyForall{} = do -- This is rule Decl∀L from [Complete and Easy]
  e <- check e =<< skolemise ByAscription ty -- gotta be polymorphic - don't allow instantiation
  pure (correct ty e)

check (Hole v a) t = do
  tell [ConFail (TvName v) t]
  pure (Hole (TvName v) (a, t))

check (Begin [] _) _ = error "impossible: check empty Begin"
check (Begin xs a) t = do
  let start = init xs
      end = last xs
  start <- traverse (fmap fst . infer) start
  end <- check end t
  pure (Begin (start ++ [end]) (a, t))

check (Let ns b an) t = do
  (ns, ts) <- inferLetTy id ns
  extendMany ts $ do
    b <- check b t
    pure (Let ns b (an, t))

check ex@(Fun pat e an) ty = let tvs = boundTvs pat in do
  (dom, cod, _) <- decompose ex _TyArr ty
  (p, vs, cs) <- checkPattern pat dom
  implies (Arm pat e) dom cs $ do
    e <- local (typeVars %~ Set.union tvs) . extendMany vs $ check e cod
    pure (Fun p e (an, ty))

check (If c t e an) ty = If <$> check c tyBool <*> check t ty <*> check e ty <*> pure (an, ty)

check (Match t ps a) ty = do
  (t, tt) <- infer t
  ps <- for ps $ \(p, e) -> let tvs = boundTvs p in do
    (p', ms, cs) <- checkPattern p tt
    (p',) <$> implies (Arm p e) tt cs
      (local (typeVars %~ Set.union tvs)
        (extendMany ms (check e ty)))
  pure (Match t ps (a, ty))

check (Access rc key a) ty = do
  rho <- freshTV
  Access <$> check rc (TyRows rho [(key, ty)]) <*> pure key <*> pure (a, ty)

-- Tuple is an introduction form, so it really should be inferring, but
-- this is a lot more elegant.
check ex@(Tuple es an) ty = Tuple <$> go es ty <*> pure (an, ty) where
  go [] _ = error "not a tuple"
  go [x] t = (:[]) <$> check x t
  go (x:xs) t = do
    (left, right, _) <- decompose ex _TyTuple t
    (:) <$> check x left <*> go xs right

-- This is _very_ annoying, but we need it for nested ascriptions
check ex@(Ascription e ty an) goal = do
  (ty, _) <- resolveKind ty
  e' <- check e ty
  _ <- subsumes ex ty goal
  pure (Ascription e' ty (an, goal))

check TypeApp{} _ = error "impossible: check TypeApp"

check e ty = do
  (e', t) <- infer e
  _ <- subsumes e ty t
  pure e'

-- [Complete and Easy]: See https://www.cl.cam.ac.uk/~nk480/bidir.pdf

infer :: MonadInfer Typed m => Expr Resolved -> m (Expr Typed, Type Typed)
infer expr@(VarRef k a) = do
  (inst, old, new) <- lookupTy' k
  if Map.null inst
     then pure (VarRef (TvName k) (a, new), new)
     else mkTyApps expr inst old new

infer (Fun p e an) = let blame = Arm p e in do
  (p, dom, ms, cs) <- inferPattern p
  let tvs = boundTvs p
  _ <- leakEqualities blame cs
  (e, cod) <- local (typeVars %~ Set.union (Set.map unTvName tvs)) . extendMany ms $ infer e
  pure (Fun p e (an, TyArr dom cod), TyArr dom cod)

infer (Literal l an) = pure (Literal l (an, ty), ty) where
  ty = case l of
    LiInt{} -> tyInt
    LiStr{} -> tyString
    LiBool{} -> tyBool
    LiUnit{} -> tyUnit
    LiFloat{} -> tyFloat

infer ex@(Ascription e ty an) = do
  (ty, _) <- resolveKind ty `catchError` \e -> throwError (ArisingFrom e (BecauseOf ex))
  e <- check e ty
  pure (Ascription (correct ty e) ty (an, ty), ty)

infer ex@(App f x a) = do
  (f, (d, c, k)) <- secondA (decompose ex _TyArr) =<< infer f
  x <- check x d
  pure (App (k f) x (a, c), c)

infer ex@(BinOp l o r a) = do
  (o, (ld, c, k1)) <- secondA (decompose ex _TyArr) =<< infer o
  (rd, c, k2) <- decompose ex _TyArr c
  (l, r) <- (,) <$> check l ld <*> check r rd
  pure (App (k2 (App (k1 o) l (a, TyArr rd c))) r (a, c), c)

infer ex@(Match t ps a) = do
  (t', tt) <- infer t
  ps' <- for ps $ \(p, e) -> do
    (p', ms, cs) <- checkPattern p tt
    let tvs = boundTvs p
    leakEqualities ex cs
    (e', ty) <- local (typeVars %~ Set.union tvs) $ extendMany ms (infer e)
    pure (p', e', ty, Arm p e)
  let (_, _, t, _) = head ps'
  ps' <- for ps' $ \(p, e, t', blame) -> do
    _ <- unify blame t t' -- TODO: the blame can also come from the pattern.
    pure (p, e)
  pure (Match t' ps' (a, t), t)

infer (Record rows a) = do
  (rows, rowts) <- unzip <$> inferRows rows
  let ty = TyExactRows rowts
   in pure (Record rows (a, ty), ty)

infer (RecordExt rec rows a) = do
  (rec, rho) <- infer rec
  (rows, newts) <- unzip <$> inferRows rows
  let ty = TyRows rho newts
   in pure (RecordExt rec rows (a, ty), ty)

infer ex = do
  x <- freshTV
  ex' <- check ex x
  pure (ex', x)

inferRows :: MonadInfer Typed m
          => [(T.Text, Expr Resolved)]
          -> m [((T.Text, Expr Typed), (T.Text, Type Typed))]
inferRows rows = for rows $ \(var', val) -> do
  (val', typ) <- infer val
  pure ((var', val'), (var', typ))

inferProg :: MonadInfer Typed m
          => [Toplevel Resolved] -> m ([Toplevel Typed], Env)
inferProg (LetStmt ns:prg) = do
  (ns', ts) <- inferLetTy closeOver ns
                   `catchError` (throwError . flip ArisingFrom (BecauseOf (LetStmt ns)))
  extendMany ts $
    consFst (LetStmt ns') $
      inferProg prg
inferProg (ForeignVal v d t ann:prg) = do
  (t', _) <- resolveKind t
  let t'' = normType (closeOver t')
  extend (TvName v, t'') $
    consFst (ForeignVal (TvName v) d t'' (ann, t'')) $
      inferProg prg
inferProg (TypeDecl n tvs cs:prg) = do
  kind <- resolveTyDeclKind n tvs cs
  let retTy = foldl TyApp (TyCon (TvName n)) (map (TyVar . TvName) tvs)
   in extendKind (TvName n, kind) $ do
     (ts, cs') <- unzip <$> for cs (\con ->
       inferCon retTy con `catchError` \x -> throwError (ArisingFrom x (BecauseOf con)))
     extendMany ts $
       consFst (TypeDecl (TvName n) (map TvName tvs) cs') $
         inferProg prg
inferProg (Open mod pre:prg) =
  -- Currently open doesn't need to do anything as we'll be in scope anyway
  consFst (Open (TvName mod) pre) $ inferProg prg
inferProg (Module name body:prg) = do
  (body', env) <- inferProg body

  let (vars, tys) = extractToplevels body
      vars' = map (\x -> (TvName x, env ^. values . at x . non undefined)) vars
      tys' = map (\x -> (TvName x, env ^. types . at x . non undefined)) tys

  extendMany vars' . extendManyK tys' $
    consFst (Module (TvName name) body') $
    inferProg prg

inferProg [] = asks ([],)


data Origin
  = Supplied -- the programmer supplied this type
  | Guessed -- the compiler invented this type
  deriving Show

-- For polymorphic recursion, mostly
approxType :: MonadInfer Typed m => Expr Resolved -> StateT Origin m (Type Typed)
approxType (Fun (PType _ t _) e _) = TyArr . fst <$> resolveKind t <*> approxType e
approxType (Ascription _ t _) = fst <$> resolveKind t
approxType (Fun _ e _) = do
  put Guessed
  TyArr <$> freshTV <*> approxType e
approxType _ = do
  put Guessed
  lift freshTV

inferLetTy :: forall m. MonadInfer Typed m
           => (Type Typed -> Type Typed)
           -> [(Var Resolved, Expr Resolved, Ann Resolved)]
           -> m ( [(Var Typed, Expr Typed, Ann Typed)]
                , [(Var Typed, Type Typed)]
                )
inferLetTy closeOver vs =
  let sccs = depOrder vs

      wasGuessed :: Origin -> Bool
      wasGuessed Guessed = True
      wasGuessed _ = False

      blameSkol :: TypeError -> (Var Resolved, SomeReason) -> TypeError
      blameSkol e (v, r) = ArisingFrom (Note e (string "in the inferred type for" <+> pretty v)) r

      figureOut :: (Var Resolved, SomeReason) -> Type Typed -> [Constraint Typed] -> m (Type Typed, Expr Typed -> Expr Typed)
      figureOut blame ty cs = do
        cur <- gen
        (x, vt) <- case solve cur cs of
          Right x -> pure (x, normType (closeOver (apply x ty)))
          Left e -> throwError (ArisingFrom e (snd blame))
        unless (null (skols vt)) $
          throwError (blameSkol (EscapedSkolems (Set.toList (skols vt)) vt) blame)
        pure (closeOver vt, applyInExpr x . raiseE id (\(a, t) -> (a, normType (apply x t))))

      generalise :: Type Typed -> m (Type Typed)
      generalise ty =
        let fv = ftv ty
         in do
           env <- Set.map TvName <$> view typeVars
           pure $ case Set.toList (fv `Set.difference` env) of
             [] -> ty
             vs -> TyForall vs ty

      approximate :: (Var Resolved, Expr Resolved, Span) -> m (Origin, (Var Typed, Type Typed))
      approximate (v, e, _) = do
        (ty, st) <- runStateT (approxType e) Supplied
        ty' <- generalise ty
        pure (st, (TvName v, if not (wasGuessed st) then ty' else ty))

      tcOne :: SCC (Var Resolved, Expr Resolved, Ann Resolved)
            -> m ( [(Var Typed, Expr Typed, Ann Typed)]
                 , [(Var Typed, Type Typed)] )

      tcOne (AcyclicSCC decl@(var, exp, ann)) = do
        (origin, tv) <- approximate decl
        ((exp', ty), cs) <- listen . extend tv $
          case origin of
            Supplied -> do
              exp' <- check exp (snd tv)
              pure (exp', snd tv)
            Guessed -> do
              (exp', ty) <- infer exp
              _ <- unify exp (snd tv) ty
              pure (exp', ty)
        (tp, k) <- figureOut (var, BecauseOf exp) ty cs
        pure ( [(TvName var, k exp', (ann, tp))], [(TvName var, tp)] )

      tcOne (CyclicSCC vars) = do
        (origins, tvs) <- unzip <$> traverse approximate vars
        (vs, cs) <- listen . extendMany tvs $
          ifor (zip tvs vars) $ \i ((_, tyvar), (var, exp, ann)) ->
            case origins !! i of
              Supplied -> do
                exp' <- check exp tyvar
                pure (TvName var, exp', ann, tyvar)
              Guessed -> do
                (exp', ty) <- infer exp
                _ <- unify exp tyvar ty
                pure (TvName var, exp', ann, ty)
        cur <- gen
        solution <- case solve cur cs of
          Right x -> pure x
          Left e -> throwError e
        let solveOne :: (Var Typed, Expr Typed, Span, Type Typed)
                     -> m ((Var Typed, Expr Typed, Ann Typed), (Var Typed, Type Typed))
            solveOne (var, exp, ann, ty) =
              let figure = apply solution
                  ty' = closeOver (figure ty)
               in do
                  unless (null (skols ty')) $
                    throwError (blameSkol (EscapedSkolems (Set.toList (skols ty')) ty') (unTvName var, BecauseOf exp))
                  pure ( (var, applyInExpr solution (raiseE id (\(a, t) -> (a, normType (figure t))) exp), (ann, ty'))
                       , (var, ty') )
         in fmap unzip . traverse solveOne $ vs

      tc :: [SCC (Var Resolved, Expr Resolved, Ann Resolved)] -> m ( [(Var Typed, Expr Typed, Ann Typed)] , [(Var Typed, Type Typed)] )
      tc (s:cs) = do
        (vs', binds) <- tcOne s
        fmap ((vs' ++) *** (binds ++)) . extendMany binds $ tc cs
      tc [] = pure ([], [])
   in tc sccs

applyInExpr :: Map.Map (Var Typed) (Type Typed) -> Expr Typed -> Expr Typed
applyInExpr ss = everywhere (mkT go) where
  go :: Expr Typed -> Expr Typed
  go (TypeApp f t a) = TypeApp f (apply ss t) a
  go x = x


consFst :: Functor m => a -> m ([a], b) -> m ([a], b)
consFst = fmap . first . (:)

{-
  Note [Freedom of the press]:
    This used to be censor (const mempty) - but that leads us to a
    problem (a real one!). Consider:

      let map f
        = let map_accum acc x
          = match x with
            | Cons (h, t) -> map_accum (Cons (f h, acc)) t
            | Nil -> acc
          in map_accum Nil ;;

    The only way we learn of `f`'s type is inside the `let`. By
    censoring it, we prevent the constraints that led to the
    discovery of `f`'s type from arising - which means we can't find
    it again! Thus, the function gets the rather strange type

      val map : ∀ 'a 'b 'c. 'c -> list 'a -> list 'b

    Instead of the correct

      val map : V 'a 'b. ('a -> 'b) -> list 'a -> list 'b

    By removing the censor, we allow the compiler to recover `'c` - which,
    if you look under e.g. the ghci debugger, has a substitution!
-}
