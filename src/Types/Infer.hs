{-# LANGUAGE FlexibleContexts, TupleSections, GADTs #-}
{-# LANGUAGE ScopedTypeVariables, ViewPatterns, RankNTypes #-}
module Types.Infer
  ( inferProgram
  , builtinsEnv
  , closeOver
  , tyString, tyInt, tyBool, tyUnit
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Traversable
import Data.Generics
import Data.Triple
import Data.List (sort)

import Control.Monad.Infer
import Control.Arrow (first)
import Syntax.Subst
import Syntax.Raise
import Syntax

import Types.Infer.Pattern
import Types.Infer.Builtin
import Types.Wellformed
import Types.Unify
import Types.Holes
import Types.Kinds

-- Solve for the types of lets in a program
inferProgram :: MonadGen Int m => [Toplevel Resolved] -> m (Either TypeError ([Toplevel Typed], Env))
inferProgram ct = fmap fst <$> runInfer builtinsEnv (inferAndCheck ct) where
  inferAndCheck prg = do
    (prg', env) <- inferProg prg
    case findHoles prg' of
      xs@(_:_) -> throwError (FoundHole xs)
      [] -> pure (prg', env)

mkTyApps :: Applicative f
         => Expr Resolved
         -> Map.Map (Var Typed) (Type Typed)
         -> Type Typed
         -> Type Typed
         -> f (Expr Typed, Type Typed)
mkTyApps (VarRef k a) mp ot@(TyForall vs _) nt = do
  let insts (t:ts) (TyForall (_:vs) c) = insts ts (TyForall vs c) . \x -> TypeApp x t (a, t)
      insts _ _ = id
  pure (insts (map (mp Map.!) vs) ot (VarRef (TvName k) (a, nt)), nt)
mkTyApps _ _ _ _ = undefined

check :: MonadInfer Typed m => Expr Resolved -> Type Typed -> m (Expr Typed)
check expr@(VarRef k a) tp = do
  (_, old, _) <- lookupTy' k
  _ <- subsumes expr tp old
  pure (VarRef (TvName k) (a, tp))
check (Hole v a) t = pure (Hole (TvName v) (a, t))
check expr@(Literal c a) t = do
  t' <- case c of
    LiStr{}  -> unify expr t tyString
    LiUnit{} -> unify expr t tyUnit
    LiBool{} -> unify expr t tyBool
    LiInt{}  -> unify expr t tyInt
  pure $ Literal c (a, t')
check ex@(Fun p b a) ty = do
  (d, c) <- decompose ex _TyArr ty
  (p', t, ms) <- inferPattern p
  _ <- unify ex t d
  b' <- extendMany ms $ check b c
  pure (Fun p' b' (a, ty))
check ex@(Begin [] _) _ = throwError (EmptyBegin ex)
check (Begin xs a) t = do
  let start = init xs
      end = last xs
  start' <- traverse (fmap fst . infer) start
  end' <- check end t
  pure (Begin (start' ++ [end']) (a, t))
check (Let ns b an) t = do
  ks <- for ns $ \(a, _, _) -> do
    tv <- freshTV
    pure (TvName a, tv)
  extendMany ks $ do
    (ns', ts) <- inferLetTy id ks ns
    extendMany ts $ do
      b' <- check b t
      pure (Let ns' b' (an, t))
check (If c t e an) ty = If <$> check c tyBool <*> check t ty <*> check e ty <*> pure (an, ty)
check ex@(App f x a) ty = do
  (f', (c, d)) <- secondA (decompose ex _TyArr) =<< infer f
  App f' <$> check x c <*> fmap (a,) (unify ex d ty)
check ex@(Match t ps a) ty = do
  (t', tt) <- infer t
  ps' <- for ps $ \(p, e) -> do
    (p', pt, ms) <- inferPattern p
    _ <- unify ex pt tt
    (,) <$> pure p' <*> extendMany ms (check e ty)
  pure (Match t' ps' (a, ty))
check ex@(BinOp l o r a) ty = do
  (o', to) <- infer o
  (el, to') <- decompose ex _TyArr to
  (er, d) <- decompose ex _TyArr to'
  BinOp <$> check l el <*> pure o' <*> check r er <*> fmap (a,) (unify ex d ty)
check ex@(Ascription e ty an) ty' = do
  (e', it) <- infer e
  (nty, _) <- resolveKind ty
  _ <- subsumes ex it nty
  _ <- unify ex nty ty'
  pure (Ascription e' nty (an, nty))
check ex@(Record rows a) ty = do
  (rows', rowts) <- unzip <$> inferRows rows
  Record rows' . (a,) <$> unify ex ty (TyExactRows rowts)
check ex@(RecordExt rec rows a) ty = do
  (rec', rho) <- infer rec
  (rows', newts) <- unzip <$> inferRows rows
  RecordExt rec' rows' . (a,) <$> unify ex ty (TyRows rho newts)
check (Access rc key a) ty = do
  rho <- freshTV
  Access <$> check rc (TyRows rho [(key, ty)]) <*> pure key <*> pure (a, ty)
check ex@(Tuple es an) ty = Tuple <$> go es ty <*> pure (an, ty) where
  go [] _ = error "not a tuple"
  go [x] t = (:[]) <$> check x t
  go (x:xs) t = do
    (left, right) <- decompose ex _TyTuple t
    (:) <$> check x left <*> go xs right
check ex@(TypeApp pf tx an) ty = do
  (tx', _) <- resolveKind tx
  (pf', tp) <- infer pf
  at <- case pf' of
    VarRef var _ -> do
      tp <- asks (Map.lookup (unTvName var) . values)
      case tp of
        Just (normType -> TyForall (v:vs) x) ->
          unify ex ty (normType (TyForall vs (apply (Map.singleton v tx') x)))
        Just tp -> throwError (IllegalTypeApp ex tp tx')
        Nothing -> throwError (NotInScope (unTvName var))
    _ -> throwError (IllegalTypeApp ex tp tx')
  pure (TypeApp pf' tx' (an, at))
check x _ = error $ "desugarer should remove " ++ show x

infer :: MonadInfer Typed m => Expr Resolved -> m (Expr Typed, Type Typed)
infer expr@(VarRef k a) =  do
  (inst, old, new) <- lookupTy' k
  if Map.null inst
     then pure (VarRef (TvName k) (a, new), new)
     else mkTyApps expr inst old new
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
  ks <- for ns $ \(a, _, _) -> do
    tv <- freshTV
    vl <- lookupTy a `catchError` const (pure tv)
    pure (TvName a, vl)
  extendMany ks $ do
    (ns', ts) <- inferLetTy closeOver ks ns
                   `catchError` (throwError . flip ArisingFrom (LetStmt ns))
    extendMany ts $
      consFst (LetStmt ns')
        $ inferProg prg
inferProg (ForeignVal v d t ann:prg) = do
  (t', _) <- resolveKind t
  extend (TvName v, t') $
    consFst (ForeignVal (TvName v) d t' (ann, t')) $
      inferProg prg
inferProg (TypeDecl n tvs cs:prg) = do
  kind <- resolveTyDeclKind n tvs cs
  let retTy = foldl TyApp (TyCon (TvName n)) (map (TyVar . TvName) tvs)
   in extendKind (TvName n, kind) $ do
     (ts, cs') <- unzip <$> for cs (\con ->
       inferCon retTy con `catchError` \x -> throwError (ArisingFrom x con))
     extendMany ts $
       consFst (TypeDecl (TvName n) (map TvName tvs) cs') $
         inferProg prg
inferProg [] = asks ([],)

inferCon :: MonadInfer Typed m
         => Type Typed
         -> Constructor Resolved
         -> m ( (Var Typed, Type Typed)
              , Constructor Typed)
inferCon ret (ArgCon nm t ann) = do
  (ty', _) <- resolveKind t
  let res = closeOver $ TyArr ty' ret
  pure ((TvName nm, res), ArgCon (TvName nm) ty' (ann, res))
inferCon ret' (UnitCon nm ann) =
  let ret = closeOver ret'
   in pure ((TvName nm, ret), UnitCon (TvName nm) (ann, ret))

inferLetTy :: (MonadInfer Typed m)
           => (Type Typed -> Type Typed)
           -> [(Var Typed, Type Typed)]
           -> [(Var Resolved, Expr Resolved, Ann Resolved)]
           -> m ( [(Var Typed, Expr Typed, Ann Typed)]
                , [(Var Typed, Type Typed)])
inferLetTy _ ks [] = pure ([], ks)
inferLetTy closeOver ks ((va, ve, vann):xs) = extendMany ks $ do
  ((ve', ty), c) <- listen (infer ve) -- See note [1]
  cur <- gen
  (x, vt) <- case solve cur mempty (sort c) of
    Left e -> throwError e
    Right x -> pure (x, closeOver (apply x ty))
  let r (a, t) = (a, apply x t)
      ex = applyInExpr x (raiseE id r ve')
  consFst (TvName va, ex, (vann, vt)) $
    inferLetTy closeOver (updateAlist (TvName va) vt ks) xs

applyInExpr :: Map.Map (Var Typed) (Type Typed) -> Expr Typed -> Expr Typed
applyInExpr ss = everywhere (mkT go) where
  go :: Expr Typed -> Expr Typed
  go (TypeApp f t a) = TypeApp f (apply ss t) a
  go x = x

-- Monomorphic so we can use "close enough" equality
updateAlist :: Eq a
            => a -> b
            -> [(a, b)] -> [(a, b)]
updateAlist n v (x@(n', _):xs)
  | n == n' = (n, v):updateAlist n v xs
  | otherwise = x:updateAlist n v xs
updateAlist _ _ [] = []

closeOver :: Type Typed -> Type Typed
closeOver a = normType $ forall (fv a) a where
  fv = Set.toList . ftv
  forall :: [Var p] -> Type p -> Type p
  forall [] a = a
  forall vs a = TyForall vs a

consFst :: Functor m => a -> m ([a], b) -> m ([a], b)
consFst = fmap . first . (:)

{-
  Commentary


  Note [1]:
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

  Note [2]:
    TODO: Since we don't have an unique `main` anymore, this code finds
    the earliest main and type checks against that. This could be a
    problem, which is why this is a TODO - Would finding the *latest*
    defined `main` be better?
-}
