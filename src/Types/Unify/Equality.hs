{-# LANGUAGE MultiWayIf, FlexibleContexts, ScopedTypeVariables,
   TupleSections, ConstraintKinds, CPP, TypeFamilies, OverloadedStrings,
   RecordWildCards #-}
{-# OPTIONS_GHC -fmax-pmcheck-iterations=5000000 #-}
module Types.Unify.Equality
  ( unify
  , flatten
  , removeTypeFamApps, reduceTyFuns
  , unifyPure, unifyPure_v
  , overlap
  , InField(..)
  ) where

import Control.Monad.Except
import Control.Applicative
import Control.Monad.Infer
import Control.Lens hiding (Empty, (:>))

import Control.Exception (assert)

import Types.Wellformed

import Syntax.Implicits hiding (overlap)
import Syntax.Transform
import Syntax.Builtin
import Syntax.Subst
import Syntax.Types
import Syntax

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import qualified Data.List as L
import Data.Traversable
import Data.Semigroup hiding (First(..))
import Data.Foldable
import Data.Function
import Data.Reason
import Data.Either
import Data.Maybe
import Data.These
import Data.Text (Text)
import Data.Ord

import Text.Pretty.Semantic

import Types.Unify.Magic
import Types.Unify.Trace
import Types.Unify.Base

import Prelude hiding (lookup)

bind :: MonadSolve m => ImplicitScope ClassInfo Typed -> Var Typed -> Type Typed -> m (Coercion Typed) --- {{{
bind scope var ty
  | TyVar var == ty = pure (ReflCo ty)
  -- /\ Var-var deletion
  | TyWildcard (Just (TyVar v)) <- ty, v == var = pure (ReflCo ty)
  -- /\ Predicativity checking
  | otherwise = do
      env <- use solveTySubst
      assum <- use solveAssumptions
      noTouch <- view don'tTouch
      ty <- pure (apply (env `compose` assum) ty) -- shadowing

      when (occurs var ty) $
        confesses (Occurs var ty)

      if | TyVar var == ty -> pure (ReflCo ty)
         | otherwise ->
            case Map.lookup var env of
              Nothing -> do
                if | var `Set.notMember` noTouch ->
                     solveTySubst .= env `compose` Map.singleton var ty
                     -- /\ We're allowed to bind the variable, so just
                     -- do it.

                   | var `Set.member` noTouch, TyVar v <- ty, v `Set.notMember` noTouch ->
                     solveTySubst .= (env `compose` Map.singleton v (TyVar var))
                     -- /\ The equality is of the form v1 := v2, where
                     -- v1 is bad but v2 is free. So what we do is add
                     -- v2 := v1 to the environment.

                   | otherwise -> confesses =<< unequal (TyVar var) ty
                   -- No can do.
                pure (ReflCo ty)
              Just ty' -> unify scope ty (apply env ty')
--- }}}

-- FOR BOTH UNIFY AND SUBSUME:
--  unify have want
--  subsume k have want
-- i.e. The irst argument is the type *we have*.
unify :: MonadSolve m => ImplicitScope ClassInfo Typed -> Type Typed -> Type Typed -> m (Coercion Typed) -- {{{

unify _ a b
  | trace EquS (keyword "unify:" <+> pretty a <+> soperator (char '~') <+> pretty b) False = undefined

unify scope (TySkol x) (TySkol y) -- {{{
  | x == y = pure (ReflCo (TySkol y))
  | otherwise = do
    sub <- use solveAssumptions
    info <- view solveInfo
    let assumption = (Right <$> sub ^. at (x ^. skolIdent)) <|> (Left <$> sub ^. at (y ^. skolIdent))
    case assumption of
      Just assum ->
        case assum of
          Right x -> unify scope x (TySkol y)
          Left y -> unify scope (TySkol x) y
      Nothing ->
        case lookupEquality info scope sub (TySkol x) (TySkol y) of
          (x:_) -> pure x
          _ -> do
            canWe <- view bindSkol
            if canWe
               then do
                 solveAssumptions . at (x ^. skolIdent) ?= TySkol y
                 pure (AssumedCo (TySkol x) (TySkol y))
               else confesses (SkolBinding x (TySkol y))
-- }}}

unify _ ta@(TyPromotedCon a) tb@(TyPromotedCon b)
  | a == b = pure (ReflCo tb)
  | otherwise = confesses =<< unequal ta tb

unify _ ta@(TyLit a) tb@(TyLit b)
  | a == b = pure (ReflCo tb)
  | otherwise = confesses =<< unequal ta tb

unify scope (TyVar a) b = bind scope a b
unify scope a (TyVar b) = SymCo <$> bind scope b a

unify scope (TyWildcard (Just a)) b = unify scope a b
unify scope a (TyWildcard (Just b)) = SymCo <$> unify scope b a

unify scope skt@(TySkol t@(Skolem sv _ _ _)) b = do -- {{{
  sub <- use (solveAssumptions . at sv)
  subst <- use solveAssumptions
  info <- view solveInfo
  case sub of
    Just ty -> do
      let ty' = apply subst ty
      _ <- unify scope b ty'
      pure (AssumedCo (TySkol t) ty')
    Nothing -> case b of
      TyVar v -> bind scope v (TySkol t)
      TyWildcard (Just tau) -> unify scope skt tau
      TyApps (TyCon v) xs | Just (Right tf) <- Map.lookup v info ->
        SymCo <$> unifyTyFunApp tf scope xs skt
      _ ->
        case lookupEquality info scope subst skt b of
          (x:_) -> pure x
          _ -> do
            canWe <- view bindSkol
            if canWe
               then if t `Set.notMember` skols b
               then do
                 solveAssumptions . at sv ?= b
                 pure (AssumedCo (TySkol t) b)
               else confesses (Occurs sv b)
               else confesses (SkolBinding t b)
unify scope b (TySkol t) = SymCo <$> unify scope (TySkol t) b -- }}}

-- ((->) a b) = a -> b
unify scope (TyApp (TyApp (TyCon v) l) r) (TyArr l' r')
  | v == tyArrowName = ArrCo <$> unify scope l l' <*> unify scope r r'

unify scope (TyArr l r) (TyApp (TyApp (TyCon v) l') r')
  | v == tyArrowName = ArrCo <$> unify scope l l' <*> unify scope r r'

unify scope x@(TyApp f g) y@(TyArr l r) =
  rethrow x y $ AppCo <$> unify scope f (TyApp (TyCon tyArrowName) l) <*> unify scope g r

unify scope x@(TyArr l r) y@(TyApp f g) =
  rethrow x y $ AppCo <$> unify scope (TyApp (TyCon tyArrowName) l) f <*> unify scope r g

unify scope (TyArr a b) (TyArr a' b') = ArrCo <$> unify scope a a' <*> unify scope b b'
unify scope (TyPi (Implicit a) b) (TyPi (Implicit a') b') =
  ArrCo <$> unify scope a a' <*> unify scope b b' -- Technically cheating but yay desugaring

unify scope ta@(TyCon a) tb@(TyCon b)
  | a == b = pure (ReflCo tb)
  | otherwise = do
      i <- view solveInfo
      x <- use solveAssumptions
      case lookupEquality i scope x ta tb of
        (x:_) -> pure x
        _ -> confesses =<< unequal ta tb

unify scope (TyForall v Nothing ty) (TyForall v' Nothing ty') = do
  fresh <- freshTV
  let (TyVar tv) = fresh

  ForallCo tv (ReflCo TyType) <$>
    unify scope (apply (Map.singleton v fresh) ty) (apply (Map.singleton v' fresh) ty')

unify scope (TyForall v (Just k) ty) (TyForall v' (Just k') ty') = do
  c <- unify scope k k'
  fresh <- freshTV
  let (TyVar tv) = fresh

  ForallCo tv c <$>
    unify scope (apply (Map.singleton v fresh) ty) (apply (Map.singleton v' fresh) ty')

unify scope (TyRows rho arow) (TyRows sigma brow)
  | overlaps <- overlap arow brow
  , rhoNew <- L.deleteFirstsBy ((==) `on` fst) (L.sortOn fst arow) (L.sortOn fst brow)
  , sigmaNew <- L.deleteFirstsBy ((==) `on` fst) (L.sortOn fst brow) (L.sortOn fst arow) =
    do
      let mk t rs = if rs /= [] then TyRows t rs else t
      tau <- freshTV
      cs <- traverse (unifRow scope) overlaps
      if null sigmaNew && null rhoNew
         then RowsCo <$> unify scope rho sigma <*> pure cs
         else do
           co <- unify scope rho (mk tau sigmaNew) -- yes
           _ <- unify scope sigma (mk tau rhoNew) -- it's backwards
           pure (RowsCo co cs)

unify scope ta@TyExactRows{} tb@TyRows{} = SymCo <$> unify scope tb ta

unify scope tb@(TyRows rho brow) ta@(TyExactRows arow)
  | overlaps <- overlap brow arow
  , rhoNew <- L.deleteFirstsBy ((==) `on` fst) (L.sortOn fst arow) (L.sortOn fst brow)
  = if | length overlaps < length brow -> confesses (NoOverlap tb ta)
       | otherwise -> do
          cs <- traverse (unifRow scope) overlaps
          _ <- unify scope rho (TyExactRows rhoNew)
          pure (SymCo (ProjCo rhoNew cs))

unify scope ta@(TyExactRows arow) tb@(TyExactRows brow)
  | overlaps <- overlap arow brow
  = do when (length overlaps /= length arow || length overlaps /= length brow)
         $ confesses (NoOverlap ta tb)
       cs <- traverse (unifRow scope) overlaps
       pure (ExactRowsCo cs)

unify _ x tp@TyRows{} = confesses (Note (CanNotInstance tp x) isRec)
unify _ tp@TyRows{} x = confesses (Note (CanNotInstance tp x) isRec)

-- ((*) a b) = a * b
unify scope (TyApp (TyApp (TyCon v) l) r) (TyTuple l' r')
  | v == tyTupleName = ProdCo <$> unify scope l l' <*> unify scope r r'

unify scope (TyTuple l r) (TyApp (TyApp (TyCon v) l') r')
  | v == tyTupleName = ProdCo <$> unify scope l l' <*> unify scope r r'

unify scope x@(TyApp f g) y@(TyTuple l r) =
  rethrow x y $ AppCo <$> unify scope f (TyApp (TyCon tyTupleName) l) <*> unify scope g r
unify scope x@(TyTuple l r) y@(TyApp f g) =
  rethrow x y $ AppCo <$> unify scope (TyApp (TyCon tyTupleName) l) f <*> unify scope r g

unify scope (TyOperator l v r) (TyTuple l' r')
  | v == tyTupleName = ProdCo <$> unify scope l l' <*> unify scope r r'

unify scope (TyTuple l r) (TyOperator l' v r')
  | v == tyTupleName = ProdCo <$> unify scope l l' <*> unify scope r r'

unify scope (TyTuple a b) (TyTuple a' b') =
  ProdCo <$> unify scope a a' <*> unify scope b b'

unify scope x@(TyOperator l v r) y@(TyApp f g) =
  rethrow x y $ AppCo <$> unify scope (TyApp (TyCon v) l) f <*> unify scope g r
unify scope x@(TyApp f g) y@(TyOperator l v r) =
  rethrow x y $ AppCo <$> unify scope f (TyApp (TyCon v) l) <*> unify scope r g

unify scope (TyOperator l v r) (TyOperator l' v' r')
  | v == v' = AppCo <$> unify scope l l' <*> unify scope r r'

unify scope ta@(TyApps (TyCon v) xs@(_:_)) b = do
  x <- view solveInfo

  case x ^. at v of
    Just (Right tf) -> unifyTyFunApp tf scope xs b
    _ -> case b of
      TyApps (TyCon v') _
        | Just (Right _) <- x ^. at v' -> do
          doWork (unequal ta b)
          SymCo <$> unify scope b ta
        | v /= v' -> confesses =<< unequal ta b

      TyApps f ys@(_:_) | length xs == length ys -> rethrow ta b $ do
        heads <- unify scope (TyCon v) f
        tails <- polyInstSafe $
          traverse (uncurry (unify scope)) (zip xs ys)
        pure (foldl AppCo heads tails)

      TyApps f ys@(_:_) | length ys < length xs -> rethrow ta b $ do
        case f of
          TyCon{} -> confesses =<< unequal ta b
          _ -> pure ()

        let ys_l = length ys
            xs_l = length xs
            xs_a = take (xs_l - ys_l) xs
            xs_b = drop (xs_l - ys_l) xs

        polyInstSafe $ do
          heads <- unify scope (TyApps (TyCon v) xs_a) f
          tails <- traverse (uncurry (unify scope)) (zip xs_b ys)
          pure (foldl AppCo heads tails)

      _ -> do
        doWork (unequal ta b)
        (confesses =<< unequal ta b)
          `catchChronicle` \_ -> fmap SymCo (unify scope b ta)

unify scope a tb@(TyApps (TyCon _) (_:_)) = rethrow a tb $ SymCo <$> unify scope tb a

unify scope (TyApp f x) (TyApp g y) = AppCo <$> unify scope f g <*> unify scope x y

unify _ TyType TyType = pure (ReflCo TyType)
unify _ a b = confesses =<< unequal a b -- }}}

unifyPure :: Type Typed -> Type Typed -> Maybe (Subst Typed)
unifyPure a b = unifyPure_v [(a, b)]

unifyPure_v :: [(Type Typed, Type Typed)] -> Maybe (Subst Typed)
unifyPure_v ts = fst . flip runNamey firstName $ do
  x <- runChronicleT $ do
    (_, st) <- runSolve False mempty emptyState (traverse_ (uncurry (unify mempty)) ts)
    pure (st ^. solveTySubst)
  case x of
    These e x | null e -> pure (Just x)
    That x -> pure (Just x)
    _ -> pure Nothing

unifRow :: MonadSolve m => ImplicitScope ClassInfo Typed -> (Text, Type Typed, Type Typed) -> m (Text, Coercion Typed)
unifRow scope (t, a, b) = do
  co <- retcons (flip Note (InField t)) (unify scope a b)
  pure (t, co)

unifyTyFunApp, unifyTyFunApp' :: forall m. MonadSolve m
                              => TySymInfo
                              -> ImplicitScope ClassInfo Typed
                              -> [Type Typed]
                              -> Type Typed
                              -> m (Coercion Typed)
tyFunByEval :: forall m. MonadSolve m
            => TySymInfo
            -> ImplicitScope ClassInfo Typed
            -> [Type Typed]
            -> Type Typed
            -> m (Maybe (Coercion Typed))

unifyTyFunApp ti _ args tb
  | trace EquS
      (keyword "[W] TF:"
        <+> pretty (TyApps (TyCon (ti ^. tsName)) args)
        <+> soperator (char '~')
        <+> pretty tb)
      False
  = undefined

unifyTyFunApp TySymInfo{} _ _ _ = undefined
unifyTyFunApp ti@(TyFamInfo tn _ _ _ _)   scope args tb@(TyApps (TyCon tn') args') | tn == tn' = do
  x <- memento $ foldl AppCo (ReflCo (TyCon tn)) <$> traverse (uncurry (unify scope)) (zip args args')
  case x of
    Left _ -> unifyTyFunApp' ti scope args tb
    Right x -> pure x
unifyTyFunApp ti scope args (TyVar v) = bind scope v (TyApps (TyCon (ti ^. tsName)) args)
unifyTyFunApp ti scope args tb = unifyTyFunApp' ti scope args tb

unifyTyFunApp' info scope args tb = do
  x <- tyFunByEval info scope args tb
  solve_info <- view solveInfo
  case x of
    Just t -> pure t
    Nothing -> case tb of
      TyApps (TyCon tn') their@(_:_) | Just (Right info') <- Map.lookup tn' solve_info -> do
        y <- tyFunByEval info' scope their (TyApps (TyCon (info ^. tsName)) args)
        maybe don't pure y
      _ -> don't
  where
    don't = do
      var <- genName
      tell [ConImplicit (It'sThis (BecauseInternal "deferred tyfam equality")) scope var
              (TyApps tyEq [TyApps (TyCon (info ^. tsName)) args, tb])]
      pure (VarCo var)

-- tyFunByEval ti scope args (TyVar v) = pure <$> bind scope v (TyApps (TyCon (ti ^. tsName)) args)
tyFunByEval (TyFamInfo tn _ _ _ _) scope args tb | Just solve <- magicTyFun tn = solve scope args tb
tyFunByEval (TyFamInfo tn eqs relevant _ con) scope args tb = do
  doWork (unequal (TyApps (TyCon tn) args) tb)
  info <- view solveInfo
  assum <- use solveAssumptions
  () <- case con of
    Just tau -> do
      let Just sub = unifyPure_v (zip decl_args args)
          TyApps _ decl_args = tau
      var <- genName
      tell [ConImplicit (It'sThis (BecauseInternal "tyfam constraint")) scope var (apply sub tau)]
    Nothing -> pure ()

  case lookupEquality info scope assum (TyApps (TyCon tn) args) tb of
    (x:_) -> pure (Just x)
    _ -> go [] eqs

  where
    go skipped ((declared', result', con):eqs) | null eqs --> all prettyConcrete (take (length relevant) args) = do
      info <- view solveInfo
      assum <- use solveAssumptions

      let vars = Set.toList (ftv declared')
      fresh <- fmap Map.fromList . for vars $ \v -> (v,) <$> freshTV
      let (declared, result) = (apply fresh declared', apply fresh result')
          rest_args = drop (length declared) args

      x <- ack (zip declared args)

      case x of
        Just (sub, cos) -> do
          (flat, _) <- flatten assum info (TyApps (TyCon tn) (apply sub declared))

          if all (apart flat) skipped

             then do
               let r_t = foldl TyApp (apply sub result) (apply sub rest_args)

               traceM EquS (keyword "[D]:" <+> pretty r_t <+> "~" <+> pretty tb)
               other_co <- unify scope r_t tb
               pure (pure (foldl AppCo (InstCo con cos) (map ReflCo rest_args) `TransCo` other_co))

             else pure Nothing

        _ -> go (TyApps (TyCon tn) declared:skipped) eqs
    go skipped (_:eqs) = go skipped eqs
    go _ [] = pure Nothing

    apart = (isNothing .) . unifyPure

    ack :: [(Type Typed, Type Typed)] -> m (Maybe (Subst Typed, [Coercion Typed]))
    ack ts = do
      (x, state) <- capture $ memento $ traverse (uncurry (unify scope)) ts
      case x of
        Left _ -> pure Nothing
        Right x -> pure (pure (state ^. solveTySubst, x))

    p --> q = not p || q

tyFunByEval _ _ _ _ = undefined

lookupEquality :: SolverInfo
               -> ImplicitScope ClassInfo Typed
               -> Map.Map (Var Typed) (Type Typed)
               -> Type Typed
               -> Type Typed
               -> [Coercion Typed] -- {{{
lookupEquality class_info scope assum a b = normal <|> fundepEquality where
  -- Try instances a ~ b (which must be LocalAssum).
  normal =
    let choices = filter ((/= Superclass) . view implSort) $
          find (TyApps tyEq [a, b]) scope <> find (TyApps tyEq [b, a]) scope

        equality_invariants xs =
             all ((== LocalAssum) . view implSort) xs
          && none (not . null . view implPre) xs
     in assert (equality_invariants choices) (map makeCo choices)

  makeCo ImplChoice{..} =
    case _implClass of
      Proj1 -> P1 _implVar
      Proj2 -> P2 _implVar
      Var   -> VarCo _implVar

  find t scope = used Var   (find_ffs t scope)
             <|> used Proj1 (find_ffs (TyTuple t a) scope)
             <|> used Proj2 (find_ffs (TyTuple a t) scope)
    where a = TyVar (TgInternal "a")
          a :: Type Typed
          used x = map (implClass .~ x)

  find_ffs t = map snd . filter (null . view implPre . snd) . filter (matches tau . view implHead . snd) . concatMap splat . Map.toList . keys where
    tau = transformType go t
    splat (x, t) = map (x,) t
    go (TySkol v) | Just x <- assum ^. at (v ^. skolIdent) = x
    go t = t

  fundepEquality =
    let
      all_instances = Map.keys (keys scope)

      inst_pairs =
            [ (x, y) | x@(TyApps (TyCon c) _) <- all_instances
                     , y@(TyApps (TyCon c') _) <- all_instances, c == c', x /= y ]
        <|> [ (x, y) | TyTuple x y <- all_instances
                     , let TyApps (TyCon c) _ = x, let TyApps (TyCon c') _ = y, c == c' ]

      pair_fds =
        [ (xs, tail (appsView y), need, det)
        | (x, y) <- inst_pairs
        , let TyApps (TyCon class_tc) xs = x
        , let Just (Left clss) = class_info ^. at class_tc
        , (need, det, _) <- clss ^. ciFundep
        ]

      implied_eqs =
        [ AssumedCo (ta !! det) (tb !! det)
        -- TODO: We shouldn't blindly assume equalities here, even
        -- though Lint will be happy with that. The principled thing to
        -- do is have each fundep add a coercion axiom with type:
        --   forall 'a 'b 'c 'd. 'a ~ 'b -> c 'a 'c -> c 'a 'd -> 'c ~ 'd
        -- then we use that here.
        | (ta, tb, needs, dets) <- pair_fds
        , flip all needs $ \idx ->
               prettyConcrete (ta !! idx)
            && prettyConcrete (tb !! idx)
            && ta !! idx == tb !! idx
        , det <- dets
        ]
     in implied_eqs
-- }}}

reduceTyFuns :: MonadSolve m => ImplicitScope ClassInfo Typed -> Type Typed -> m (Type Typed)
reduceTyFuns scope orig = do
  assum <- use solveAssumptions
  sub <- use solveTySubst
  info <- view solveInfo

  (tau, funs) <- (_1 %~ apply sub) <$> flatten assum info orig
  traceM EquS ("flattened: " <+> pretty tau)
  let eqs = Map.toList funs

  (_, state) <- censor (const mempty) . capture . for eqs $ \(var, TyApps (TyCon tyfun) args) -> do
    let Just (Right tf) = Map.lookup tyfun info
    unifyTyFunApp' tf scope args (TyVar var)

  if null eqs
     then pure orig
     else pure (apply (state ^. solveTySubst) tau)

removeTypeFamApps :: (MonadReader Env m, MonadNamey m) => Type Typed -> m (Type Typed, Subst Typed)
removeTypeFamApps tau = do
  x <- getSolveInfo
  flatten mempty x tau

flatten :: forall m. MonadNamey m => Subst Typed -> SolverInfo -> Type Typed -> m (Type Typed, Subst Typed)
flatten assum i = runWriterT . go 0 where
  go :: Int -> Type Typed -> WriterT (Subst Typed) m (Type Typed)
  go l tau@(TyApps (TyCon v) as@(_:_))
    | l > 0, Just (Right _) <- i ^. at v = do
      ~v@(TyVar key) <- freshTV
      tell (Map.singleton key tau)
      pure v
    | otherwise = TyApps (TyCon v) <$> traverse (go (l + 1)) as

  go l (TyApp f x) = TyApp <$> go (l + 1) f <*> go (l + 1) x
  go l (TySkol v)
    | Just x <- assum ^. at (v ^. skolIdent) = go (l + 1) x
    | otherwise = freshTV
  go _ x@TyCon{} = pure x
  go _ x@TyVar{} = pure x
  go _ x@TyPromotedCon{} = pure x
  go _ x@TyLit{} = pure x
  go _ TyWithConstraints{} = error "flatten TyWithConstraints"
  go _ TyType = pure TyType

  go l (TyPi c t) = TyPi <$> go_b c <*> go (l + 1) t where
    go_b (Anon c) = Anon <$> go (l + 1) c
    go_b (Implicit c) = Implicit <$> go (l + 1) c
    go_b (Invisible v k r) = Invisible v <$> traverse (go (l + 1)) k <*> pure r

  go l (TyTuple a b) = TyTuple <$> go (l + 1) a <*> go (l + 1) b
  go l (TyRows t rs) = TyRows <$> go (l + 1) t <*> traverse (_2 %%~ go (l + 1)) rs
  go l (TyExactRows rs) = TyExactRows <$> traverse (_2 %%~ go (l + 1)) rs
  go x (TyOperator l o r) = TyOperator <$> go (x + 1) l <*> pure o <*> go (x + 1) r
  go l (TyWildcard v) = TyWildcard <$> traverse (go (l + 1)) v
  go l (TyParens t) = TyParens <$> go (l + 1) t

occurs :: Var Typed -> Type Typed -> Bool
occurs _ (TyVar _) = False
occurs _ (TyWildcard (Just (TyVar _))) = False
occurs v (TyWildcard (Just t)) = occurs v t
occurs x e = x `Set.member` ftv e

overlap :: [(Text, Type p)] -> [(Text, Type p)] -> [(Text, Type p, Type p)]
overlap xs ys
  | inter <- filter ((/=) 1 . length) $ L.groupBy ((==) `on` fst) (L.sortOn fst (xs ++ ys))
  = map get inter
  where get [(t, a), (_, b)] = (t, a, b)
        get _ = undefined

data UsedEq = Var | Proj1 | Proj2 deriving (Eq, Show, Ord)

newtype InField = InField Text
  deriving Show

instance Pretty InField where
  pretty (InField t) = string "When checking the field" <+> skeyword (text t)

isRec :: String
isRec = "A record type's hole can only be instanced to another record"
