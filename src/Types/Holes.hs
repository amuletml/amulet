{-# LANGUAGE TemplateHaskell, ViewPatterns, FlexibleContexts, TupleSections, ConstraintKinds, OverloadedStrings #-}
module Types.Holes (findHoleCandidate) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T

import Control.Monad.Reader
import Control.Monad.Namey
import Control.Monad.Logic
import Control.Monad.Infer
import Control.Monad.Fail (MonadFail)

import Control.Applicative

import Control.Lens hiding (Lazy)

import Syntax.Builtin
import Syntax.Subst
import Syntax.Types
import Syntax.Var
import Syntax

import Data.Traversable
import Data.Maybe
import Data.Span

import Types.Unify

import Types.Infer.Pattern

data PsScope =
  PsScope { _psVars :: Map.Map (Type Typed) [Expr Typed]
          , _psEnv :: Env
          , _psAnn :: Span
          , _psInScope :: Set.Set (Var Typed)
          }

type MonadPs m = (MonadNamey m, MonadReader PsScope m, MonadLogic m, MonadFail m)

makeLenses ''PsScope

findHoleCandidate :: MonadNamey m => Subst Typed -> Span -> Env -> Type Typed -> m [Expr Typed]
findHoleCandidate _ ann env t = observeManyT 10 $ runReaderT (fill t) (PsScope mempty env ann nms) where
  nms :: Set.Set (Var Typed)
  nms = env ^. names . to namesInScope . to Set.fromAscList

-- | Compute an expression that has the given type.
fill :: MonadPs m => Type Typed -> m (Expr Typed)
-- Let's get the impossible cases over with first:
fill TyLit{} = fail "findHoleCandidate: Kind error (TyLit)"
fill TyPromotedCon{} = fail "findHoleCandidate: Kind error (TyPromotedCon)"
fill TyType{} = fail "findHoleCandidate: Kind error (TyType)"
-- Type variables are complicated, so we only deal with skolemised types:
fill TyVar{} = fail "findHoleCandidate: don't know how to deal with type variables"

-- Skolems are only possible if we have a variable in scope for them:
fill t@TySkol{} = pick t

-- Introduce a new variable for the domain type and fill the function
-- body
fill ty@(domain :-> codomain) = fake [ty] $ \[a] ->
  assume domain $ \pat ->
    Fun (PatParam pat) <$> fill codomain <*> pure a

-- Special cases for concrete types we know about:
fill ty@(appsView -> [con, xs]) | con == tyList =
    pickLocal ty -- Try a known list
  `interleave`
    fake [ty] (pure . ListExp [] . head) -- Try nil
  `interleave`
    do -- Try consing
      head <- fill xs
      tail <- fill ty
      fake [ty, TyTuple xs ty, cONSTy] $ \[list, tuple, cons] ->
        pure (App (VarRef cONSName cons) (Tuple [head, tail] tuple) list)

-- Try to thunk a value corresponding to the type inside the `lazy`
fill ty@(appsView -> [con, xs]) | con == tyLazy =
  fake [ty] $ \[lazy] -> Lazy <$> fill xs <*> pure lazy

-- Let us not choose arbitrary literals, but pick them from scope
-- instead.
fill t | t == tyInt = pick t
fill t | t == tyString = pick t
fill t | t == tyFloat = pick t
fill t | t == tyFloat = pick t
fill t | t == tyBool = pick t

-- This one is easy, though.
fill t | t == tyUnit = fake [t] $ pure . Literal LiUnit . head

-- Sum types: we need to try each constructor.
fill ty@(TyApps (TyCon con) _) = once (pickLocal ty) <|> do
  -- Search all constructors for the type..
  con <- explore =<< view (psEnv . types . at con . non mempty . to Set.toList)

  (_, cty, inst_ty) <- instantiate Strong Expression =<<
    view (psEnv . names . at con . non (error "no type for bound constructor?"))

  let (cons, con_t) = splitConstrainedType inst_ty
  -- We need to see if all of the constraints are satisfiable, so feed
  -- them to the solver.
  case con_t of
    dom :-> cod -> do
      Just sub <- pure $ unifyPure_v ((ty, cod):cons)
      -- Alright, let's apply it: Fill the domain recursively.
      once $ fake [cty, ty] $ \[cann, app] ->
        App (VarRef con cann) <$> fill (apply sub dom) <*> pure app

    cod -> do
      Just _ <- pure $ unifyPure_v ((ty, cod):cons)
      -- We're done, since this is a unit constructor.
      once $ fake [ty] $ pure . VarRef con . head

-- Applications of type operators can be filled just like other
-- applications
fill (TyOperator l v r) = fill (TyApps (TyCon v) [l, r])

-- Filling pairs? Fill the left-hand side and the right-hand side in
-- order.
fill ty@(TyTuple a b) = do
  x <- fill a
  y <- fill b
  fake [ty] $ pure . Tuple [x, y] . head

-- To fill a record, we just fill each of the named fields.
fill ty@(TyRows _ xs) = fake [ty] $ \[ann] -> do
  rs <- for xs $ \(label, ty) ->
    fake [ty] $ \[field] ->
      Field label <$> fill ty <*> pure field

  pure (Record rs ann)

fill ty@(TyExactRows xs) = fake [ty] $ \[ann] -> do
  rs <- for xs $ \(label, ty) ->
    fake [ty] $ \[field] ->
      Field label <$> fill ty <*> pure field

  pure (Record rs ann)

fill _ = fail ""

-- | Compute all the possible variables with a matching type /that the
-- TC knows about/. This is a potentially huge number of variables if we
-- go by the typical @t ~ t'@ relationship, so we use the 'Eq' instance
-- instead.
tcVars :: MonadReader PsScope m => Type Typed -> m [Expr Typed]
tcVars ty = fake [ty] $ \[a] -> do
  all <- view (psEnv . names . to scopeToList)
  letBound <- view (psEnv . letBound)
  pure [ VarRef x a | (x, ty') <- all, x `Set.notMember` letBound, ty == ty' ]

knownImplication :: MonadPs m => Type Typed -> m (Expr Typed)
knownImplication ty = fake [ty] $ \[app] -> do
  all <- Map.toList <$> view psVars

  (fun, args, sub) <- explore
    [ (ex, mapMaybe (fmap (apply sub) . isArg) args, sub)
    | (TyArrs args ret, (ex:_)) <- all
    , Just sub <- [unifyPure ret ty] ]

  local (psVars %~ Map.mapKeys (apply sub)) $
    foldl (mkApp app) fun <$> traverse fill args

tcKnownImplication :: MonadPs m => Type Typed -> m (Expr Typed)
tcKnownImplication ty = fake [ty] $ \[app] -> do
  an <- view psAnn
  all <- view (psEnv . names . to scopeToList)

  (fun, tc_ty) <- explore $ [ (VarRef x (an, tc_ty), tc_ty) | (x, tc_ty@TyPi{}) <- all ]
  (_, _, TyArrs (mapMaybe isArg -> args) cod) <- instantiate Strong Expression tc_ty

  Just _ <- pure $ unifyPure cod ty

  foldl (mkApp app) fun <$> traverse fill args

pick, pickLocal :: MonadPs m => Type Typed -> m (Expr Typed)
pick t = pickLocal t `interleave` (explore =<< tcVars t) `interleave` tcKnownImplication t

pickLocal t = knownImplication t

-- | Exhaustively explore a list of choices.
explore :: MonadLogic m => [a] -> m a
explore [] = mzero
explore (x:xs) = pure x `interleave` explore xs

fake :: MonadReader PsScope m => [Type Typed] -> ([Ann Typed] -> m a) -> m a
fake t k = k . (flip zip t . repeat) =<< view psAnn

-- | Generate a fake variable and capture, and add it to the proof
-- search scope.
variable :: (MonadReader PsScope m, MonadNamey m) => Type Typed -> (Expr Typed -> Pattern Typed -> m a) -> m a
variable domain k = fake [domain] $ \[a] -> do
  vars <- view psInScope

  x <- genNameWithHint vars domain

  let ref = VarRef x a

  local (psVars . at domain %~ insert ref) . local (psInScope %~ Set.insert x) $
    k ref (Capture x a)

-- | A generalised version of 'variable' that will decompose product
-- types.
assume :: (MonadReader PsScope m, MonadNamey m) => Type Typed -> (Pattern Typed -> m a) -> m a
assume ty@TyTuple{} k = go [] (untup ty) k where
  untup (TyTuple a b) = a:untup b
  untup t = [t]

  go pats (x:xs) k = variable x $ \_ pat -> go (pat:pats) xs k
  go pats [] k = fake [ty] $ \[tuple] -> k (PTuple (reverse pats) tuple)

assume ty@(TyRows _ xs) k = go [] xs k where
  go pats ((label, t):xs) k = variable t $ \_ pat -> go ((label, pat):pats) xs k
  go pats [] k = fake [ty] $ \[record] -> k (PRecord (reverse pats) record)

assume ty@(TyExactRows xs) k = go [] xs k where
  go pats ((label, t):xs) k = variable t $ \_ pat -> go ((label, pat):pats) xs k
  go pats [] k = fake [ty] $ \[record] -> k (PRecord (reverse pats) record)

assume ty@(TyApps (TyCon c) xs) k = fake [ty] $ \[pat] -> do
  cs <- view (psEnv . types . at c . non mempty . to Set.toList)
  case cs of
    [con] -> do
      ~(dom :-> TyApps _ tyvars, _) <- skolGadt con =<<
        instantiate Strong Expression =<<
          view (psEnv . names . at con . non undefined)

      let x `u` y = fromMaybe mempty (unifyPure x y)
          sub = foldr1 compose (zipWith u tyvars xs)

      assume (apply sub dom) $ \inner ->
        k (Destructure con (Just inner) pat)
    _ -> variable ty (const k)

assume t k = variable t (const k)

insert :: a -> Maybe [a] -> Maybe [a]
insert x Nothing = Just [x]
insert x (Just xs) = Just (x:xs)

splitConstrainedType :: Type Typed -> ([(Type Typed, Type Typed)], Type Typed)
splitConstrainedType (TyWithConstraints ts t) = (ts, t)
splitConstrainedType t = ([], t)

isArg :: TyBinder p -> Maybe (Type p)
isArg (Anon t) = Just t
isArg _ = Nothing

mkApp :: Ann Typed -> Expr Typed -> Expr Typed -> Expr Typed
mkApp ann f x = App f x ann

genNameWithHint :: MonadNamey m => Set.Set (Var Typed) -> Type Typed -> m (Var Typed)
genNameWithHint vars ty =
  do
    ~(TgName _ id) <- genName
    let ourname = TgName (hint !! (id `mod` length hint)) id

    pure $ if ourname `Set.member` vars
       then discriminate ourname
       else ourname
  where
    hints ty = case ty of
      TyPi{} -> ["f", "g", "h"]
      TyApps t [x] | t == tyList -> map (`T.snoc` 's') $ hints x
      _ -> ["x", "y", "z"]
    hint = hints ty
    discriminate (TgName x i) = TgName (x <> T.pack (show i)) i
    discriminate _ = undefined
