{-# LANGUAGE TemplateHaskell, ViewPatterns, FlexibleContexts
           , TupleSections, ConstraintKinds, OverloadedStrings
           , ScopedTypeVariables, CPP #-}
module Types.Holes (findHoleCandidate) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T

import Control.Monad.Reader
import Control.Monad.Namey
import Control.Monad.Logic
import Control.Monad.Infer

import Control.Applicative

import Control.Lens hiding (Lazy)

import Syntax.Builtin
import Syntax.Subst
import Syntax.Types
import Syntax.Var
import Syntax

import Data.Text (Text)
import Data.Traversable
import Data.Spanned
import Data.Maybe
import Data.Span

import Types.Unify

import Types.Infer.Pattern

#ifdef TRACE_TC
import Debug.Trace
import Text.Pretty.Semantic hiding (fill)
#endif

data PsScope =
  PsScope { _psVars :: Map.Map (Type Typed) [Expr Typed]
          , _psEnv :: Env
          , _psAnn :: !Span
          , _psInScope :: Set.Set Text
          , _psDepth :: !Int
          }

type MonadPs m = (MonadNamey m, MonadReader PsScope m, MonadLogic m, MonadFail m, Alternative m)

makeLenses ''PsScope

findHoleCandidate :: MonadNamey m => Subst Typed -> Span -> Env -> Type Typed -> m [Expr Typed]
findHoleCandidate _ ann env t = observeManyT 10 $ runReaderT (fill t) (PsScope mempty env ann nms 0) where
  nms = env ^. names . to namesInScope . to (map getName) . to Set.fromAscList
  getName (TgName x _) = x
  getName (TgInternal x) = x

-- | Compute an expression that has the given type.
fill :: forall m. MonadPs m => Type Typed -> m (Expr Typed)
#ifdef TRACE_TC
fill t | trace (displayS (keyword "fill" <+> pretty t)) False = undefined
#endif

fill t@TyPi{} | isSkolemisable t = fake [t] $ \[a] -> do
  (_, t, _, _) <- skolemise (ByAscription (annotation a) t) t
  fill t

-- Let's get the impossible cases over with first:
fill TyLit{} = fail "findHoleCandidate: Kind error (TyLit)"
-- fill TyPromotedCon{} = fail "findHoleCandidate: Kind error (TyPromotedCon)"
fill TyType{} = fail "findHoleCandidate: Kind error (TyType)"
-- Type variables are complicated, so we only deal with skolemised types:
fill TyVar{} = fail "findHoleCandidate: don't know how to deal with type variables"

-- Skolems are only possible if we have a variable in scope for them:
fill t@TySkol{} = pick t

-- Introduce a new variable for the domain type and fill the function
-- body
fill ty@(domain :-> codomain) = fake [ty] $ \[a] -> do
  is_sum <- isSum domain
  let makeFun :: m (Expr Typed)
      makeFun = assume domain $ \pat -> Fun (PatParam pat) <$> fill codomain <*> pure a
  if is_sum
     then makeFunction domain codomain <|> makeFun
     else makeFun

-- Special cases for concrete types we know about:
fill ty@(TyApps con [_]) | con == tyList =
    knownImplication ty -- Try a known list
  `interleave`
    fake [ty] (pure . ListExp [] . head) -- Try nil

-- Try to thunk a value corresponding to the type inside the `lazy`
fill ty@(TyApps con [xs]) | con == tyLazy =
  fake [ty] $ \[lazy] -> Lazy <$> fill xs <*> pure lazy

-- Make a reference of the inner type or try existing
fill ty@(TyApps con [xs]) | con == tyRef =
    pick ty
  `interleave`
    do fake [xs] $ \[an] ->
         App (VarRef refName an) <$> fill xs <*> pure an

-- Let us not choose arbitrary literals, but pick them from scope
-- instead.
fill t | t == tyInt = knownImplication t `interleave` (explore =<< tcVars t)
fill t | t == tyString = knownImplication t `interleave` (explore =<< tcVars t)
fill t | t == tyFloat = knownImplication t `interleave` (explore =<< tcVars t)

-- Booleans are finite so we can enumerate them.
fill t | t == tyBool = fake [t] $ \[a] ->
  explore [ Literal (LiBool True) a
          , Literal (LiBool False) a
          ]

-- This one is easy, though.
fill t | t == tyUnit = fake [t] $ pure . Literal LiUnit . head

-- Sum types: we need to try each constructor.
fill ty@(TyApps (TyCon ty_v) _) = once (knownImplication ty) <|> do
  -- Search all constructors for the type..
  con <- explore =<< view (psEnv . types . at ty_v . non mempty . to Set.toList)

  (_, cty, inst_ty) <- instantiate Strong Expression =<<
    view (psEnv . names . at con . non (error "no type for bound constructor?"))

  let (cons, con_t) = splitConstrainedType inst_ty
  -- We need to see if all of the constraints are satisfiable, so feed
  -- them to the solver.
  case con_t of
    dom :-> cod -> do
      guard (nonRec ty_v dom)
      Just sub <- pure $ unifyPure_v ((ty, cod):cons)
      -- Alright, let's apply it: Fill the domain recursively.
#ifdef TRACE_TC
      traceM (displayS (keyword "introduce" <+> pretty con))
#endif
      once $ fake [cty, ty] $ \[cann, app] ->
        App (VarRef con cann) <$> fill (apply sub dom) <*> pure app

    cod -> do
      Just _ <- pure $ unifyPure_v ((ty, cod):cons)
#ifdef TRACE_TC
      traceM (displayS (keyword "use constructor" <+> pretty con))
#endif
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
fill ty@(TyExactRows xs) = fake [ty] $ \[ann] -> do
  rs <- for xs $ \(label, ty) ->
    fake [ty] $ \[field] ->
      Field label <$> fill ty <*> pure field

  pure (Record rs ann)

fill _ = fail ""

makeFunction :: MonadPs m => Type Typed -> Type Typed -> m (Expr Typed)
makeFunction domain@(TyApps (TyCon t) _) body_t = fake [domain] $ \[ann] -> do
  cons <- view (psEnv . types . at t . non undefined . to Set.toList)

  arms <- for cons $ \con -> do
    (ty, _) <- skolGadt con =<<
       instantiate Strong Expression =<<
         view (psEnv . names . at con . non undefined)
    ([], ty) <- pure $ splitConstrainedType ty

    case ty of
      dom :-> cod -> do
        guard (nonRec t dom)
        Just sub <- pure $ unifyPure cod domain
        assume (apply sub dom) $ \pat ->
          Arm (Destructure con (Just pat) ann) Nothing <$> fill body_t

      _ -> Arm (Destructure con Nothing ann) Nothing <$> fill body_t

  pure $ Function arms ann

makeFunction _ _ = undefined

-- | Is this type constructor a sum type constructor?
isSum :: MonadReader PsScope m => Type Typed -> m Bool
isSum (TyOperator l o r) = isSum (TyApps (TyCon o) [l, r])
isSum (TyApps (TyCon t) _) = do
  x <- view (psEnv . types . at t)
  pure $ case x of
    Just t -> Set.size t /= 1
    Nothing -> False
isSum _ = pure False

-- | Compute all the possible variables with a matching type /that the
-- TC knows about/. This is a potentially huge number of variables if we
-- go by the typical @t ~ t'@ relationship, so we use the 'Eq' instance
-- instead.
tcVars :: MonadReader PsScope m => Type Typed -> m [Expr Typed]
tcVars ty = fake [ty] $ \[a] -> do
  all <- view (psEnv . names . to scopeToList)
  letBound <- view (psEnv . letBound)
  pure [ VarRef x a | (x, ty') <- all, x `Set.notMember` letBound, ty == ty' ]

pick, knownImplication :: MonadPs m => Type Typed -> m (Expr Typed)
pick t = knownImplication t `interleave` (explore =<< tcVars t)

knownImplication ty = fake [ty] $ \[app] -> do
  guard . (< 20) =<< view psDepth
  all <- Map.toList <$> view psVars

  (fun, args, sub) <- explore
    [ (ex, mapMaybe (fmap (apply sub) . isArg) args, sub)
    | (TyArrs args ret, ex:_) <- all
    , Just sub <- [unifyPure ret ty]
    ]

-- If we need the type we want to fill to be able to call this function
-- then it's not very useful
  guard (ty `Set.notMember` Set.fromList args)

#ifdef TRACE_TC
  traceM (displayS (keyword "considering" <+> string "function" <+> pretty fun <+> colon <+> pretty ty))
#endif

  local (psVars %~ Map.mapKeys (apply sub)) . local (psDepth %~ succ) $
    foldl (mkApp app) fun <$> traverse fill args

-- | Exhaustively explore a list of choices.
explore :: MonadLogic m => [a] -> m a
explore = foldr (interleave . pure) mzero

fake :: MonadReader PsScope m => [Type Typed] -> ([Ann Typed] -> m a) -> m a
fake t k = k . (flip zip t . repeat) =<< view psAnn

-- | Generate a fake variable and capture, and add it to the proof
-- search scope.
variable :: (MonadReader PsScope m, MonadNamey m) => Type Typed -> (Expr Typed -> Pattern Typed -> m a) -> m a
variable domain k = fake [domain] $ \[a] -> do
  vars <- view psInScope

  (x, name) <- genNameWithHint vars domain

  let ref = VarRef x a

#ifdef TRACE_TC
  traceM (displayS (keyword "assume" <+> pretty x <+> colon <+> pretty domain))
#endif

  local (psVars . at domain %~ insert ref) . local (psInScope %~ Set.insert name) $
    k ref (Capture x a)

-- | A generalised version of 'variable' that will decompose product
-- types.
assume :: MonadPs m => Type Typed -> (Pattern Typed -> m a) -> m a
assume ty@TyTuple{} k = go [] (untup ty) k where
  untup (TyTuple a b) = a:untup b
  untup t = [t]

  go pats (x:xs) k = assume x $ \pat -> go (pat:pats) xs k
  go pats [] k = fake [ty] $ \[tuple] -> k (PTuple (reverse pats) tuple)

assume ty@(TyRows _ xs) k = go [] xs k where
  go pats ((label, t):xs) k = assume t $ \pat -> go ((label, pat):pats) xs k
  go pats [] k = fake [ty] $ \[record] -> k (PRecord (reverse pats) record)

assume ty@(TyExactRows xs) k = go [] xs k where
  go pats ((label, t):xs) k = assume t $ \pat -> go ((label, pat):pats) xs k
  go pats [] k = fake [ty] $ \[record] -> k (PRecord (reverse pats) record)

assume ty@(TyApps (TyCon c) xs) k = fake [ty] $ \[pat] -> do
  cs <- view (psEnv . types . at c . non mempty . to Set.toList)
  case cs of
    [con] -> do
      ~(ty, _) <- skolGadt con =<<
        instantiate Strong Expression =<<
          view (psEnv . names . at con . non undefined)

      case ty of
        dom :-> TyApps _ tyvars -> do
          guard (not (isSkolemisable dom))
          guard (nonRec c dom)

          let x `u` y = fromMaybe mempty (unifyPure x y)
              sub = foldr1 compose (zipWith u tyvars xs)

          assume (apply sub dom) $ \inner ->
            k (Destructure con (Just inner) pat)
        _ -> k (Destructure con Nothing pat)
    _ -> variable ty (const k)

assume t@TyPi{} k = do
  (_, _, t) <- instantiate Strong Expression t
  variable t (const k)

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

genNameWithHint :: MonadNamey m => Set.Set Text -> Type Typed -> m (Var Typed, Text)
genNameWithHint vars ty =
  do
    ~(TgName _ id) <- genName
    let ourname = TgName (hint !! (id `mod` length hint)) id
        name = hint !! (id `mod` length hint)
        hint = hints ty

    pure $ if name `Set.member` vars
              then (discriminate ourname, name)
              else (ourname, name)
  where
    hints ty = case ty of
      TyPi{} -> ["f", "g", "h"]
      TyApps t [x] | t == tyList -> map (`T.snoc` 's') $ hints x
      _ -> ["x", "y", "z", "a", "b", "c"]

    discriminate (TgName x i) = TgName (x <> T.pack (show i)) i
    discriminate _ = undefined

nonRec :: Var Typed -> Type Typed -> Bool
nonRec v (TyApps (TyCon x) _) = x /= v
nonRec v (TyTuple a b) = nonRec v a && nonRec v b
nonRec v (TyPi _ b) = nonRec v b
nonRec _ TyVar{} = True
nonRec _ TySkol{} = True
nonRec _ TyPromotedCon{} = True
nonRec _ TyLit{} = True
nonRec _ TyType{} = True
nonRec v (TyRows t xs) = nonRec v t && all (nonRec v . snd) xs
nonRec v (TyExactRows xs) = all (nonRec v . snd) xs
nonRec v (TyParens p) = nonRec v p
nonRec v (TyOperator l v' r) = v /= v' && nonRec v l && nonRec v r
nonRec _ _ = error "nonRec: that's a weird type you have there."
