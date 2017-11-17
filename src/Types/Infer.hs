{-# OPTIONS_GHC -Wno-missing-local-signatures #-}
{-# LANGUAGE FlexibleContexts, OverloadedStrings, TupleSections, GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Types.Infer (inferProgram, builtinsEnv, inferCon) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Span (internal, Span)

import Control.Monad.Infer
import Control.Arrow (first)

import Syntax.Subst
import Syntax.Raise
import Syntax

import Types.Wellformed
import Types.Unify
import Types.Holes

import Data.List


-- Solve for the types of lets in a program
inferProgram :: MonadGen Int m => [Toplevel Resolved] -> m (Either TypeError ([Toplevel Typed], Env))
inferProgram ct = fmap fst <$> runInfer builtinsEnv (inferAndCheck ct) where
  inferAndCheck prg = do
    (prg', env) <- inferProg prg
    case findHoles prg' of
      xs@(_:_) -> throwError (FoundHole xs)
      [] -> pure (prg', env)

tyUnit, tyBool, tyInt, tyString :: Type Typed
tyInt = TyCon (TvName Rigid (TgInternal "int") TyStar)
tyString = TyCon (TvName Rigid (TgInternal "string") TyStar)
tyBool = TyCon (TvName Rigid (TgInternal "bool") TyStar)
tyUnit = TyCon (TvName Rigid (TgInternal "unit") TyStar)

star :: Ann p ~ Span => Type p
star = TyStar

forall :: Ann p ~ Span => [Var p] -> Type p -> Type p
forall = TyForall

app, arr :: Ann p ~ Span => Type p -> Type p -> Type p
arr = TyArr
app = TyApp

var, con :: Ann p ~ Span => Var p -> Type p
var = TyVar
con = TyCon

builtinsEnv :: Env
builtinsEnv = Env (M.fromList ops) (M.fromList tps) where
  op :: T.Text -> Type Typed -> (Var Resolved, Type Typed)
  op x t = (ReName (TgInternal x), t)
  tp :: T.Text -> (Var Resolved, Type Typed)
  tp x = (ReName (TgInternal x), star)

  boolOp = tyBool `arr` (tyBool `arr` tyBool)
  intOp = tyInt `arr` (tyInt `arr` tyInt)
  stringOp = tyString `arr` (tyString `arr` tyString)
  intCmp = tyInt `arr` (tyInt `arr` tyBool)

  cmp = forall [name] $ var name `arr` (var name `arr` tyBool)
    where name = TvName Flexible (TgInternal "a") star -- TODO: This should use TvName/TvFresh instead
  ops = [ op "+" intOp, op "-" intOp, op "*" intOp, op "/" intOp, op "**" intOp
        , op "^" stringOp
        , op "<" intCmp, op ">" intCmp, op ">=" intCmp, op "<=" intCmp
        , op "==" cmp, op "<>" cmp
        , op "||" boolOp, op "&&" boolOp ]
  tps :: [(Var Resolved, Type Typed)]
  tps = [ tp "int", tp "string", tp "bool", tp "unit" ]

unify :: MonadInfer Typed m => Expr Resolved -> Type Typed -> Type Typed -> m ()
unify e a b = tell [ConUnify (raiseE (`tag` TyStar) id e) a b]

tag :: Var Resolved -> Type Typed -> Var Typed
tag (ReName v) = TvName Flexible v

infer :: MonadInfer Typed m => Expr Resolved -> m (Expr Typed, Type Typed)
infer expr
  = case expr of
      VarRef k a -> do
        x <- lookupTy k
        pure (VarRef (tag k x) a, x)
      Hole v ann -> do
        tv <- freshTV
        pure (Hole (tag v tv) ann, tv)
      Literal c a -> case c of
        LiInt _ -> pure (Literal c a, tyInt)
        LiStr _ -> pure (Literal c a, tyString)
        LiBool _ -> pure (Literal c a, tyBool)
        LiUnit   -> pure (Literal c a, tyUnit)
      Fun p b a -> do
        (p', tc, ms) <- inferPattern (unify expr) p
        (b', tb) <- extendMany ms $ infer b
        pure (Fun p' b' a, TyArr tc tb)
      Begin [] _ -> throwError (EmptyBegin expr)
      Begin xs a -> do
        (xs', txs) <- unzip <$> mapM infer xs
        pure (Begin xs' a, last txs)
      If c t e a -> do
        (c', tc) <- infer c
        (t', tt) <- infer t
        (e', te) <- infer e
        unify c tyBool tc
        unify expr tt te
        pure (If c' t' e' a, te)
      App e1 e2 a -> do
        (e1', t1) <- infer e1
        (e2', t2) <- infer e2
        tv <- freshTV
        unify expr t1 (TyArr t2 tv)
        pure (App e1' e2' a, tv)
      Let ns b ann -> do
        ks <- forM ns $ \(a, _) -> do
          tv <- freshTV
          pure (tag a tv, tv)
        extendMany ks $ do
          (ns', ts) <- inferLetTy id ks ns
          extendMany ts $ do
            (b', ty) <- infer b
            pure (Let ns' b' ann, ty)
      Match t ps a -> do
        (t', tt) <- infer t
        (ps', tbs) <- unzip <$> forM ps
            (\ (p, e) -> do
              (p', pt, ks) <- inferPattern (unify expr) p
              unify expr tt pt
              (e', ty) <- extendMany ks (infer e)
              pure ((p', e'), ty))
        ty <- case tbs of
                [] -> throwError (EmptyMatch expr)
                [x] -> pure x
                (ty:xs) -> do
                  mapM_ (unify expr ty) xs
                  pure ty
        pure (Match t' ps' a, ty)
      BinOp l o r a -> do
        (l', tl) <- infer l
        (o', to) <- infer o
        (r', tr) <- infer r
        tv <- freshTV
        unify expr to (TyArr tl (TyArr tr tv))
        pure (BinOp l' o' r' a, tv)
      Record rows a -> do
        itps <- inferRows rows
        let (rows', rowts) = unzip itps
        pure (Record rows' a, TyExactRows rowts)
      RecordExt rec rows a -> do
        itps <- inferRows rows
        let (rows', newTypes) = unzip itps
        (rec', rho) <- infer rec
        pure (RecordExt rec' rows' a, TyRows rho newTypes)
      Access rec key a -> do
        (rho, ktp) <- (,) <$> freshTV <*> freshTV
        (rec', tp) <- infer rec
        let rows = TyRows rho [(key, ktp)]
        unify expr tp rows
        pure (Access rec' key a, ktp)
      Tuple es an -> do
        es' <- mapM infer es
        case es' of
          [] -> pure (Tuple [] an, tyUnit)
          [(x', t)] -> pure (x', t)
          ((x', t):xs) -> pure (Tuple (x':map fst xs) an, foldl TyTuple t (map snd xs))
      EHasType e g an -> do
        (e', t') <- infer e
        (g', _) <- inferKind g
        unify expr g' t'
        pure (EHasType e' t' an, g')
      LeftSection{} -> error "desugarer removes right sections"
      RightSection{} -> error "desugarer removes left sections"
      BothSection{} -> error "desugarer removes both-side sections"
      AccessSection{} -> error "desugarer removes access sections"

inferRows :: MonadInfer Typed m => [(T.Text, Expr Resolved)] -> m [((T.Text, Expr Typed), (T.Text, Type Typed))]
inferRows rows = forM rows $ \(var', val) -> do
  (val', typ) <- infer val
  pure ((var', val'), (var', typ))

inferKind :: MonadInfer a m => Type Resolved -> m (Type Typed, Type Typed)
inferKind TyStar = pure (TyStar, TyStar)
inferKind (TyVar v) = do
  x <- lookupKind v `catchError` const (pure TyStar)
  pure (TyVar (tag v x), x)
inferKind (TyCon v) = do
  x <- lookupKind v
  pure (TyCon (tag v x), x)
inferKind (TyForall vs k) = do
  (k, t') <- extendManyK (zip (map (`tag` TyStar) vs) (repeat TyStar)) $
    inferKind k
  pure (TyForall (map (`tag` TyStar) vs) k, t')
inferKind (TyArr a b) = do
  (a', ka) <- inferKind a
  (b', kb) <- inferKind b
  -- TODO: A "proper" unification system
  when (ka /= TyStar) $ throwError (NotEqual ka TyStar)
  when (kb /= TyStar) $ throwError (NotEqual kb TyStar)
  pure (TyArr a' b', TyStar)
inferKind tp@(TyRows rho rows) = do
  wellformed tp
  case rho of
    TyRows rho' rows' -> inferKind (TyRows rho' (rows' `union` rows))
    TyExactRows rows' -> inferKind (TyExactRows (rows' `union` rows))
    TyVar{} -> do
      (rho', _) <- inferKind rho
      ks <- forM rows $ \(var, typ) -> do
        (typ', _) <- inferKind typ
        pure (var, typ')
      pure (TyRows rho' ks, TyStar)
    _ -> error "wellformedness check rejects this"
inferKind (TyExactRows rows) = do
  ks <- forM rows $ \(var, typ) -> do
    (typ', _) <- inferKind typ
    pure (var, typ')
  pure (TyExactRows ks, TyStar)
inferKind ap@(TyApp a b) = do
  (a', x) <- inferKind a
  case x of
    TyArr t bd -> do
      (b', xb) <- inferKind b
      when (t /= xb) $ throwError (NotEqual t xb)
      pure (TyApp a' b', bd)
    _ -> throwError (ExpectedArrow ap x a')
inferKind (TyTuple a b) = do
  (a', _) <- inferKind a
  (b', _) <- inferKind b
  pure (TyTuple a' b', TyStar)
inferKind (TyCons cs b) = do
  cs' <- forM cs $ \(Equal ta tb an) -> do
    (ta', _) <- inferKind ta
    (tb', _) <- inferKind tb
    pure (Equal ta' tb' an)
  (b', k) <- inferKind b
  pure (TyCons cs' b', k)

-- Returns: Type of the overall thing * type of captures
inferPattern :: MonadInfer a m
             => (Type Typed -> Type Typed -> m ())
             -> Pattern Resolved
             -> m (Pattern Typed, Type Typed, [(Var Typed, Type Typed)])
inferPattern _ (Wildcard ann) = do
  x <- freshTV
  pure (Wildcard ann, x, [])
inferPattern _ (Capture v ann) = do
  x <- freshTV
  pure (Capture (tag v x) ann, x, [(tag v x, x)])
inferPattern unify (Destructure cns ps ann)
  | Nothing <- ps = do
    pty <- lookupTy cns
    pure (Destructure (tag cns pty) Nothing ann, pty, [])
  | Just p <- ps = do
    (tup, res, ct) <- constructorTy <$> lookupTy cns
    (p', pt, pb) <- inferPattern unify p
    unify tup pt
    pure (Destructure (tag cns ct) (Just p') ann, res, pb)
  where constructorTy :: Type Typed -> (Type Typed, Type Typed, Type Typed)
        constructorTy t
          | TyArr tup res <- t
          = (tup, res, t)
          | TyCons cs x <- t
          , (tup', res,_ ) <- constructorTy x
          = (tup', TyCons cs res, t)
          | otherwise = undefined
inferPattern unify (PRecord rows ann) = do
  rho <- freshTV
  (rowps, rowts, caps) <- unzip3 <$> forM rows (\(var, pat) -> do
    (p', t, caps) <- inferPattern unify pat
    pure ((var, p'), (var, t), caps))
  pure (PRecord rowps ann, TyRows rho rowts, concat caps)
inferPattern unify pat@(PType p t ann) = do
  (p', pt, vs) <- inferPattern unify p
  (t', _) <- inferKind t `catchError` \x -> throwError (ArisingFrom x pat)
  unify pt t'
  pure (PType p' t' ann, t', vs)
inferPattern unify (PTuple elems ann)
  | [] <- elems = pure (PTuple [] ann, tyUnit, [])
  | [x] <- elems = inferPattern unify x
  | otherwise = do
    (ps, t:ts, cps) <- unzip3 <$> mapM (inferPattern unify) elems
    pure (PTuple ps ann, foldl TyTuple t ts, concat cps)

inferProg :: MonadInfer Typed m => [Toplevel Resolved] -> m ([Toplevel Typed], Env)
inferProg (LetStmt ns ann:prg) = do
  ks <- forM ns $ \(a, _) -> do
    tv <- freshTV
    vl <- lookupTy a `catchError` const (pure tv)
    pure (tag a tv, vl)
  extendMany ks $ do
    (ns', ts) <- inferLetTy closeOver ks ns
    extendMany ts $
      consFst (LetStmt ns' ann) $ inferProg prg
inferProg (ValStmt v t ann:prg) = do
  (t', _) <- inferKind t
  extend (tag v t', closeOver t') $
    consFst (ValStmt (tag v t') t' ann) $ inferProg prg
inferProg (ForeignVal v d t ann:prg) = do
  (t', _) <- inferKind t
  extend (tag v t', closeOver t') $
    consFst (ForeignVal (tag v t') d t' ann) $ inferProg prg
inferProg (TypeDecl n tvs cs ann:prg) =
  let mkk :: [a] -> Type Typed
      mkk [] = star
      mkk (_:xs) = arr star (mkk xs)
      retTy = foldl app (con (n `tag` star)) (map (var . flip tag star) tvs)
      n' = tag n (mkk tvs)
      arisingFromConstructor = "Perhaps you're missing a * between two types of a constructor?"
   in extendKind (n', mkk tvs) $ do
     (ts, cs') <- unzip <$> mapM (inferCon retTy) cs
                               `catchError` \x -> throwError (Note x arisingFromConstructor)
     extendMany ts $
       consFst (TypeDecl n' (map (`tag` star) tvs) cs' ann) $
         inferProg prg
inferProg [] = do
  let ann = internal
      isMain (ReName (TgName x _)) = x == "main"
      isMain _ = False
      key (ReName (TgName k _)) = k
      key _ = undefined
  (_, c) <- censor (const mempty) . listen $ do
    main <- head . sortOn key . filter isMain . M.keys <$> asks values -- Note [2]
    x <- lookupTy main -- TODO: Actually look up the correct name
    unify (VarRef main ann) x (TyArr tyUnit tyUnit)
  x <- gen
  case solve x mempty c of
    Left e -> throwError (Note e "main must be a function from unit to some type")
    Right _ -> ([],) <$> ask


inferCon :: MonadInfer Typed m
         => Type Typed
         -> Constructor Resolved
         -> m ( (Var Typed, Type Typed)
              , Constructor Typed)
inferCon ret (ArgCon nm t ann) = do
  (ty', _) <- inferKind t
  let res = closeOver $ TyArr ty' ret
  pure ((tag nm res, res), ArgCon (tag nm res) ty' ann)
inferCon ret' (UnitCon nm ann) =
  let ret = closeOver ret'
   in pure ((tag nm ret, ret), UnitCon (tag nm ret) ann)
inferCon ret (GADTCon nm ty ann) = extendManyK (mentionedTVs ret) $ do
  (res, hole) <- gadtConTy ty
  -- This is a game of matching up paramters of the return type with the
  -- stated parameters, and introducing the proper constraints
  cons <- matchUp res ret
  let TyForall vars resTp = closeOver (TyCons cons (hole ret))
  vars' <- mapM rigidify vars
  let tp = TyForall vars' resTp
  pure ((tag nm tp, tp), GADTCon (tag nm tp) tp ann)
  where mentionedTVs :: Type Typed -> [(Var Typed, Type Typed)]
        mentionedTVs (TyApp r (TyVar v)) = (v, TyStar):mentionedTVs r
        mentionedTVs _ = []

        matchUp :: MonadInfer a m => Type Resolved
                -> Type Typed -> m [GivenConstraint Typed]
        matchUp (TyCon _ )   (TyCon _ )  = pure []
        matchUp (TyApp i x ) (TyApp j y) = do
          (x', _) <- inferKind x
          (:) <$> pure (Equal x' y ann) <*> matchUp i j
        matchUp _ _ = error "impossible because of how GADTs work"

        rigidify :: MonadInfer Typed m => Var Typed -> m (Var Typed)
        rigidify v@(TvName _ nm an) = do
          x <- asks types
          case M.lookup (eraseVarTy v) x of
            Just _ -> pure (TvName Flexible nm an)
            Nothing -> pure (TvName Rigid nm an)

        gadtConTy :: MonadInfer a m => Type Resolved
                  -> m ( Type Resolved
                       , Type Typed -> Type Typed)
        gadtConTy ty = case ty of
          TyArr a b -> do
            (b, hole) <- gadtConTy b
            (a', _) <- inferKind a
            pure (b, TyArr a' . hole)
          TyForall vs t -> do
            (t, hole) <- gadtConTy t
            pure (t, TyForall (map (`tag` TyStar) vs) . hole)
          x | x `instanceOf` ret -> pure (x, id)
            | otherwise          -> throwError (IllegalGADT x)

instanceOf :: Type Resolved -> Type Typed -> Bool
instanceOf (TyCon a) (TyCon b) = a == eraseVarTy b
instanceOf (TyApp a _) (TyApp b _) = a `instanceOf` b
instanceOf _ _ = False


inferLetTy :: (MonadInfer Typed m)
           => (Type Typed -> Type Typed)
           -> [(Var Typed, Type Typed)]
           -> [(Var Resolved, Expr Resolved)]
           -> m ( [(Var Typed, Expr Typed)]
                , [(Var Typed, Type Typed)])
inferLetTy _ ks [] = pure ([], ks)
inferLetTy closeOver ks ((va, ve):xs) = extendMany ks $ do
  ((ve', ty), c) <- listen (infer ve) -- See note [1]
  cur <- gen
  (x, vt) <- case solve cur mempty c of
    Left e -> throwError e
    Right x -> pure (x, closeOver (apply x ty))
  let r (TvName r n t) = TvName r n (apply x t)
      ex = raiseE r id ve'
  (vt', _) <- inferKind (raiseT eraseVarTy id vt)
  consFst (tag va vt', ex) $ inferLetTy closeOver (updateAlist (tag va vt') vt' ks) xs

-- Monomorphic so we can use "close enough" equality
updateAlist :: Var Typed
            -> b
            -> [(Var Typed, b)] -> [(Var Typed, b)]
updateAlist n v (x@(n', _):xs)
  | n == n' = (n, v):updateAlist n v xs
  | n `closeEnough` n' = (n, v):updateAlist n v xs
  | otherwise = x:updateAlist n v xs
updateAlist _ _ [] = []

closeOver :: Type Typed -> Type Typed
closeOver a = normType $ forall (fv a) a where
  fv = S.toList . ftv
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
