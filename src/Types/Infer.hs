{-# OPTIONS_GHC -Wno-missing-local-signatures #-}
{-# LANGUAGE FlexibleContexts, OverloadedStrings, TupleSections, GADTs #-}
{-# LANGUAGE ScopedTypeVariables, ViewPatterns #-}
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
import Data.Span (Span)
import Data.Foldable

import Control.Monad.Infer
import Control.Arrow (first, (&&&))

import Syntax.Subst
import Syntax.Raise
import Syntax

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

tyUnit, tyBool, tyInt, tyString :: Type Typed
tyInt = TyCon (TvName (TgInternal "int"))
tyString = TyCon (TvName (TgInternal "string"))
tyBool = TyCon (TvName (TgInternal "bool"))
tyUnit = TyCon (TvName (TgInternal "unit"))

forall :: [Var p] -> Type p -> Type p
forall = TyForall

app, arr :: Type p -> Type p -> Type p
arr = TyArr
app = TyApp

var, con :: Var p -> Type p
var = TyVar
con = TyCon

builtinsEnv :: Env
builtinsEnv = Env (Map.fromList ops) (Map.fromList tps) where
  op :: T.Text -> Type Typed -> (Var Resolved, Type Typed)
  op x t = (TgInternal x, t)
  tp :: T.Text -> (Var Resolved, Kind Typed)
  tp x = (TgInternal x, KiStar)

  boolOp = tyBool `arr` (tyBool `arr` tyBool)
  intOp = tyInt `arr` (tyInt `arr` tyInt)
  stringOp = tyString `arr` (tyString `arr` tyString)
  intCmp = tyInt `arr` (tyInt `arr` tyBool)

  cmp = forall [name] $ var name `arr` (var name `arr` tyBool)
    where name = TvName (TgInternal "a")-- TODO: This should use TvName/TvFresh instead
  ops = [ op "+" intOp, op "-" intOp, op "*" intOp, op "/" intOp, op "**" intOp
        , op "^" stringOp
        , op "<" intCmp, op ">" intCmp, op ">=" intCmp, op "<=" intCmp
        , op "==" cmp, op "<>" cmp
        , op "||" boolOp, op "&&" boolOp ]
  tps :: [(Var Resolved, Kind Typed)]
  tps = [ tp "int", tp "string", tp "bool", tp "unit" ]

unify :: MonadInfer Typed m => Expr Resolved -> Type Typed -> Type Typed -> m ()
unify e a b = tell [ConUnify (raiseE TvName (id &&& const undefined) e) a b]

itIs :: Monad m
     => ((Span, Type Typed) -> f Typed)
     -> Span -> Type Typed -> m (f Typed, Type Typed)
itIs f a t = pure (f (a, t), t)

mkTT :: (Show (Var p), Show (Ann p)) => Type p -> [Type p] -> Type p
mkTT x xs = TyTuple x (go xs) where
  go [] = error $ "mkTT fucked up: " ++ show x ++ " " ++ show xs
  go [x] = x
  go (x:xs) = TyTuple x (go xs)

infer :: MonadInfer Typed m => Expr Resolved -> m (Expr Typed, Type Typed)
infer expr
  = case expr of
      VarRef k a -> itIs (VarRef (TvName k)) a =<< lookupTy k
      Hole v ann -> itIs (Hole (TvName v)) ann =<< freshTV
      Literal c a -> case c of
        LiStr _ -> pure (Literal c (a, tyString), tyString)
        LiUnit  -> pure (Literal c (a, tyUnit), tyUnit)
        LiBool _-> pure (Literal c (a, tyBool), tyBool)
        LiInt _ -> pure (Literal c (a, tyInt), tyInt)
      Fun p b a -> do
        (p', tc, ms) <- inferPattern (unify expr) p
        (b', tb) <- extendMany ms $ infer b
        pure (Fun p' b' (a, TyArr tc tb), TyArr tc tb)
      Begin [] _ -> throwError (EmptyBegin expr)
      Begin xs a -> do
        (xs', txs) <- unzip <$> traverse infer xs
        pure (Begin xs' (a, last txs), last txs)
      If c t e a -> do
        (c', tc) <- infer c
        (t', tt) <- infer t
        (e', te) <- infer e
        unify c tyBool tc
        unify expr tt te
        pure (If c' t' e' (a, te), te)
      App e1 e2 a -> do
        (e1', t1) <- infer e1
        (e2', t2) <- infer e2
        tv <- freshTV
        unify expr (TyArr t2 tv) t1
        pure (App e1' e2' (a, tv), tv)
      Let ns b ann -> do
        ks <- for ns $ \(a, _, _) -> do
          tv <- freshTV
          pure (TvName a, tv)
        extendMany ks $ do
          (ns', ts) <- inferLetTy id ks ns
          extendMany ts $ do
            (b', ty) <- infer b
            pure (Let ns' b' (ann, ty), ty)
      Match t ps a -> do
        (t', tt) <- infer t
        (ps', tbs) <- unzip <$> for ps
            (\ (p, e) -> do
              (p', pt, ks) <- inferPattern (unify expr) p
              unify expr tt pt
              (e', ty) <- extendMany ks (infer e)
              pure ((p', e'), ty))
        ty <- case tbs of
                [] -> throwError (EmptyMatch expr)
                [x] -> pure x
                (ty:xs) -> do
                  traverse_ (unify expr ty) xs
                  pure ty
        pure (Match t' ps' (a, ty), ty)
      BinOp l o r a -> do
        (l', tl) <- infer l
        (o', to) <- infer o
        (r', tr) <- infer r
        tv <- freshTV
        unify expr (TyArr tl (TyArr tr tv)) to
        pure (BinOp l' o' r' (a, tv), tv)
      Record rows a -> do
        itps <- inferRows rows
        let (rows', rowts) = unzip itps
        itIs (Record rows') a (TyExactRows rowts)
        -- pure (Record rows' (a, TyExactRows), TyExactRows rowts)
      RecordExt rec rows a -> do
        itps <- inferRows rows
        let (rows', newTypes) = unzip itps
        (rec', rho) <- infer rec
        itIs (RecordExt rec' rows') a (TyRows rho newTypes)
      Access rec key a -> do
        (rho, ktp) <- (,) <$> freshTV <*> freshTV
        (rec', tp) <- infer rec
        let rows = TyRows rho [(key, ktp)]
        unify expr tp rows
        itIs (Access rec' key) a ktp
      Tuple es an -> do
        es' <- traverse infer es
        case es' of
          [] -> itIs (Tuple []) an tyUnit
          [(x', t)] -> pure (x', t)
          ((x', t):xs) -> itIs (Tuple (x':map fst xs)) an (mkTT t (map snd xs))
      Ascription e g an -> do
        (e', t') <- infer e
        (g', _) <- resolveKind g
        unify expr t' g'
        itIs (Ascription e' t') an g'
      LeftSection{} -> error "desugarer removes right sections"
      RightSection{} -> error "desugarer removes left sections"
      BothSection{} -> error "desugarer removes both-side sections"
      AccessSection{} -> error "desugarer removes access sections"

inferRows :: MonadInfer Typed m
          => [(T.Text, Expr Resolved)]
          -> m [((T.Text, Expr Typed), (T.Text, Type Typed))]
inferRows rows = for rows $ \(var', val) -> do
  (val', typ) <- infer val
  pure ((var', val'), (var', typ))

inferPattern :: MonadInfer Typed m
             => (Type Typed -> Type Typed -> m ())
             -> Pattern Resolved
             -> m ( Pattern Typed -- the pattern
                  , Type Typed -- type of what the pattern matches
                  , [(Var Typed, Type Typed)] -- captures
                  )
inferPattern _ (Wildcard ann) = do
  x <- freshTV
  pure (Wildcard (ann, x), x, [])
inferPattern _ (Capture v ann) = do
  x <- freshTV
  pure (Capture (TvName v) (ann, x), x, [(TvName v, x)])
inferPattern unify (Destructure cns ps ann)
  | Nothing <- ps = do
    pty <- lookupTy cns
    pure (Destructure (TvName cns) Nothing (ann, pty), pty, [])
  | Just p <- ps = do
    (tup, res, _) <- constructorTy <$> lookupTy cns
    (p', pt, pb) <- inferPattern unify p
    unify tup pt
    pure (Destructure (TvName cns) (Just p') (ann, res), res, pb)
  where constructorTy :: Type Typed -> (Type Typed, Type Typed, Type Typed)
        constructorTy t
          | TyArr tup res <- t = (tup, res, t)
          | otherwise = undefined
inferPattern unify (PRecord rows ann) = do
  rho <- freshTV
  (rowps, rowts, caps) <- unzip3 <$> for rows (\(var, pat) -> do
    (p', t, caps) <- inferPattern unify pat
    pure ((var, p'), (var, t), caps))
  pure (PRecord rowps (ann, TyRows rho rowts), TyRows rho rowts, concat caps)
inferPattern unify (PType p t ann) = do
  (p', pt, vs) <- inferPattern unify p
  (t', _) <- resolveKind t
  unify pt t'
  case p' of
    Capture v _ -> pure (PType p' t' (ann, t'), t', [(v, t')])
    _ -> pure (PType p' t' (ann, t'), t', vs)
inferPattern unify (PTuple elems ann)
  | [] <- elems = pure (PTuple [] (ann, tyUnit), tyUnit, [])
  | [x] <- elems = inferPattern unify x
  | otherwise = do
    (ps, t:ts, cps) <- unzip3 <$> traverse (inferPattern unify) elems
    pure (PTuple ps (ann, mkTT t ts), mkTT t ts, concat cps)

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
  let retTy = foldl app (con (TvName n)) (map (var . TvName) tvs)
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
  (x, vt) <- case solve cur mempty c of
    Left e -> throwError e
    Right x -> pure (x, closeOver (apply x ty))
  let r (a, t) = (a, apply x t)
      ex = raiseE id r ve'
  consFst (TvName va, ex, (vann, vt)) $
    inferLetTy closeOver (updateAlist (TvName va) vt ks) xs

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
