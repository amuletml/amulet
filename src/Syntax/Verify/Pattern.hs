{-# LANGUAGE
  DeriveFunctor
, FlexibleContexts
, FlexibleInstances
, GADTs
, OverloadedStrings
, ScopedTypeVariables
, StandaloneDeriving
, TupleSections
, UndecidableInstances #-}

{- | Helpers for pattern totality checking

This is based on the "GADTs meet their match" paper[1], with some minor
modifications:

 - Amulet is non-lazy, and so pattern evaluation order is not
   defined. Thus we need not consider the "diverging" case in the
   paper. We also merge computation of the covered and uncovered sets
   into one computation.

 - We introduce tuples and record patterns, which are effectively treated
   as single-constructor types (they themselves are always matched, and
   decompose to a single thing).

 - Value abstractions also contain a "literal" and "not literal" case,
   which match against a single literal, and a set of "all literals but
   these".

There are a couple of improvements we could in the future, if needs
arise:

 - We currently do not implement variable+pattern constraints, as we do
   not currently have view patterns.

 - Our handling of guards is rather naive (just discard the uncovered set
   in this case). It is worth considering how we can improve this, while
   still taking into account the lack of purity guarantees.

 - We could optimise the handling of non-GADT destructure patterns so we
   do not need to generate as many free type variables, and can rely less
   on the unifier. The paper[1] discusses some ways of doing this.

 - Improve out handling of uninhabited types. Our checking of
   inhabitedness is rather naive, and just ducks out on recursive types,
   even if they are trivially uninhabited (say, type a = A of a). We
   should also check whether types are inhabited on non-empty arms.

[1]: https://www.microsoft.com/en-us/research/wp-content/uploads/2016/08/gadtpm-acm.pdf

-}
module Syntax.Verify.Pattern
  ( Covering, covered, uncovered
  , ValueAbs(..)
  , AbsState
  , emptyAbsState, emptyAlt
  , covering
  , inhabited
  ) where

import Control.Monad.State.Strict
import Control.Monad.Chronicles
import Control.Monad.Namey
import Control.Monad.Infer
import Control.Applicative
import Control.Lens

import qualified Data.Map.Lazy as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Bifunctor
import Data.Foldable
import Data.Reason
import Data.These
import Data.Maybe

import Syntax.Types hiding (constructors)
import Syntax

import Types.Infer.Builtin (expandTypeWith)
import Types.Infer.Pattern (skolGadt)
import Types.Unify

import Text.Pretty.Semantic hiding (empty)

-- | Represents both the covered and uncovered set of a computation.
--
-- This could be thought of as a unification of the list and maybe monad.
data Covering a
  = Covering { covered'   :: Seq.Seq a
             , uncovered' :: Seq.Seq a
             }
  -- | Represents a "pure" version of the covered set. We use this to
  -- represent values constructed via "pure" (and so both the covered and
  -- uncovered sets are identical).
  | PureCovering { pureCover :: (Seq.Seq a) }
  deriving (Show, Functor)

covered, uncovered :: Covering a -> Seq.Seq a
covered (Covering c _) = c
covered (PureCovering c) = c

uncovered (Covering _ u) = u
uncovered (PureCovering u) = u

instance Applicative Covering where
  pure x = PureCovering (pure x)

  (PureCovering f) <*> (PureCovering x) = PureCovering (f <*> x)
  (Covering fc fu) <*> (Covering xc xu) = Covering (fc <*> xc) (fu <*> xu)
  (Covering fc fu) <*> (PureCovering x) = Covering (fc <*> x) (fu <*> x)
  (PureCovering f) <*> (Covering xc xu) = Covering (f <*> xc) (f <*> xu)

instance Alternative Covering where
  empty = PureCovering empty

  (PureCovering l) <|> (PureCovering r) = PureCovering (l <|> r)
  (Covering lc lu) <|> (Covering rc ru) = Covering (lc <|> rc) (lu <|> ru)
  (Covering lc lu) <|> (PureCovering r) = Covering (lc <|> r) (lu <|> r)
  (PureCovering l) <|> (Covering rc ru) = Covering (l <|> rc) (l <|> ru)

instance Monad Covering where
  (Covering c u) >>= f = Covering (c >>= covered . f) (u >>= uncovered . f)
  (PureCovering x) >>= f = asum (f <$> x)

instance MonadPlus Covering where

class Monad m => MonadCover m where
  -- | Return a computation representing just a covered value.
  cover :: a -> m a
  -- | Return a computation representing just an uncovered value.
  uncover :: a -> m a

instance MonadCover Covering where
  cover x = Covering (pure x) empty
  uncover x = Covering empty (pure x)

instance (Monad (t m), MonadCover m, MonadTrans t) => MonadCover (t m) where
  cover = lift . cover
  uncover = lift . uncover

-- | An abstract representation of a value.
--
-- With the exception of 'VNotLiteral', this is a subset of both
-- expressions and patterns, and allows representing what values are
-- covered and uncovered by a given pattern.
data ValueAbs p
  = VVariable (Var p) (Type p)
  | VDestructure (Var p) (Maybe (ValueAbs p))
  | VRecord (Map.Map T.Text (ValueAbs p))
  | VTuple (ValueAbs p) (ValueAbs p)

  | VLiteral Lit
  | VNotLiteral (Var p) (Set.Set Lit)

deriving instance (Show (Var p), Show (Ann p)) => Show (ValueAbs p)

instance Pretty (Var p) => Pretty (ValueAbs p) where
  pretty VVariable{} = skeyword "_"
  pretty (VDestructure k Nothing) = stypeCon (pretty k)
  pretty (VDestructure k (Just p)) = stypeCon (pretty k) <+>
    case p of
      VDestructure _ Just{} -> parens (prettyIn p)
      _ -> prettyIn p
  pretty (VRecord fs)
    -- Should we filter out wildcard fields here?
    = enclose (lbrace <> space) (space <> rbrace)
    . hsep . punctuate comma
    . map (\(n, v) -> text n <+> equals <+> prettyIn v)
    . Map.toList $ fs
  pretty t@VTuple{} = hsep . punctuate comma . extract $ t where
    extract (VTuple l r) = prettyIn l : extract r
    extract p = [prettyIn p]

  pretty (VLiteral l) = pretty l
  pretty VNotLiteral{} = skeyword "_"

-- | Pretty-print an internal value
prettyIn :: Pretty (Var p) => ValueAbs p -> Doc
prettyIn v@VTuple{} = parens (pretty v)
prettyIn v = pretty v

-- | Wraps the state of one pattern alternative, capturing constraints,
-- substitution sets and the latest fresh variable.
data AbsState
  = AbsState
  { altSub :: SolveState
  , altTyc :: Seq.Seq (Constraint Typed)
  , altFresh :: VarResolved
  }
  deriving (Show)

-- | Construct an empty pattern abstraction state from a fresh variable,
emptyAbsState :: Var Typed -> AbsState
emptyAbsState = AbsState emptyState mempty

-- | Construct an empty value abstraction from a abstraction state and
-- the type of the test variable.
emptyAlt :: AbsState -> Type Typed -> (AbsState, ValueAbs Typed)
emptyAlt alt ty =
  let v@(TgName _ i) = altFresh alt
  in (alt { altFresh = TgName "a" (i + 1) }, VVariable v ty)

-- | A type-safe list of constraints between patterns and value
-- abstractions.
--
-- We use this to ensure that each iteration of the solver consumes and
-- produces exactly the same number of terms.
data PatPair a where
  Nil :: PatPair ()
  (:*:) :: (Pattern Typed, ValueAbs Typed) -> PatPair b -> PatPair (ValueAbs Typed, b)

infixr 9 :*:

-- | Extract the pattern part of a 'PatPair', suitable for building an
-- uncovered set.
patPair :: PatPair a -> a
patPair Nil = ()
patPair ((_, x) :*: xs) = (x, patPair xs)

type CoverState = (SolveState, Seq.Seq (Constraint Typed))

-- | The type of the covering monad
type CoveringM = StateT CoverState (NameyT Covering)

-- | Run the value abstraction monad.
covering :: Env -> Pattern Typed
         -> Seq.Seq (AbsState, ValueAbs Typed)
         -> Covering (AbsState, ValueAbs Typed)
covering env p xs = msum . flip fmap xs $ \(AbsState st cs i, v) -> do
  (((v', ()), (st', cs')), i') <- flip runNameyT i
                                . flip runStateT (st, cs)
                                $ covering' env ((p, v) :*: Nil)
  pure (AbsState st' cs' i', v')

covering' :: Env -> PatPair a -> CoveringM a
covering' env = go where
  go :: PatPair a -> CoveringM a

  -- Nil
  go Nil = cover ()

  -- ConCon for constructors: If the constructors are the same, just
  -- recurse. Otherwise, that constructor is uncovered.
  go ((Destructure k (Just p) _, v@(VDestructure k' (Just u))) :*: xs)
    | k == k' = first (VDestructure k' . Just) <$> go ((p, u):*:xs)
    | otherwise = uncover (v, patPair xs)
  go ((Destructure k Nothing _, v@(VDestructure k' Nothing)) :*: xs)
    | k == k' = (VDestructure k' Nothing,) <$> go xs
    | otherwise = uncover (v, patPair xs)
  go ((Destructure{}, v@VDestructure{}) :*: xs) = uncover (v, patPair xs)

  go ((PGadtCon k _ _ (Just p) _, v@(VDestructure k' (Just u))) :*: xs)
    | k == k' = first (VDestructure k' . Just) <$> go ((p, u):*:xs)
    | otherwise = uncover (v, patPair xs)
  go ((PGadtCon k _ _ Nothing _, v@(VDestructure k' Nothing)) :*: xs)
    | k == k' = (VDestructure k' Nothing,) <$> go xs
    | otherwise = uncover (v, patPair xs)
  go ((PGadtCon{}, v@VDestructure{}) :*: xs) = uncover (v, patPair xs)

  -- ConVar for constructors: We always perform the UConVar implementation here -
  -- namely we find every possible case.
  go ((p@(Destructure _ _ (_, ty)), VVariable v vTy) :*: xs) = do
    (k, arg) <- constructors env ty vTy
    maybe (pure ()) guardInhabited arg
    guardAll xs
    case arg of
      Nothing -> onVar p v (VDestructure k Nothing) xs
      Just arg -> do
        v' <- genName
        onVar p v (VDestructure k (Just (VVariable v' arg))) xs

  go ((p@(PGadtCon _ _ _ _ (_, ty)), VVariable v vTy) :*: xs) = do
    (k, arg) <- constructors env ty vTy
    maybe (pure ()) guardInhabited arg
    guardAll xs
    case arg of
      Nothing -> onVar p v (VDestructure k Nothing) xs
      Just arg -> do
        v' <- genName
        onVar p v (VDestructure k (Just (VVariable v' arg))) xs

  go ((Destructure{}, _) :*: _) = error "Mismatch on Destructure"
  go ((PGadtCon{}, _) :*: _) = error "Mismatch on PGadtCon"

  -- ConCon for literals
  go ((PLiteral li _, v@(VLiteral li')) :*: xs)
    | li == li' = (v,) <$> go xs
    | otherwise = uncover (v, patPair xs)

  -- ConCon for bounded literals: Like normal constructors, we can just enumerate each value.
  go ((p@(PLiteral LiUnit{} _), VVariable v _) :*: xs) = onVar p v (VLiteral LiUnit) xs
  go ((p@(PLiteral LiBool{} _), VVariable v _) :*: xs)
    =   onVar p v (VLiteral (LiBool True)) xs
    <|> onVar p v (VLiteral (LiBool False)) xs

  -- For other literals, we have an infinite number of values, so we
  -- consider this specific literal, and the set of all other literals.
  go ((p@(PLiteral li _), VVariable v _) :*: xs)
    =   onVar p v (VLiteral li) xs
    <|> onVar p v (VNotLiteral v (Set.singleton li)) xs
  -- This case is a little messy, as it's both ConCon and ConVar. Namely, if
  -- we're not in the excluded set, treat it as a variable. If we are, treat it
  -- as a constructor (and so it is uncovered).
  go ((p@(PLiteral li _), n@(VNotLiteral v lis)) :*: xs)
    | li `Set.notMember` lis
      =   onVar p v (VLiteral li) xs
      <|> onVar p v (VNotLiteral v (Set.insert li lis)) xs
    | otherwise = uncover (n, patPair xs)

  go ((PLiteral{}, _) :*: _) = error "Mismatch on PLiteral"

  -- As tuple patterns can be of differing sizes, we can't just zip them
  -- up. Thus we represent them as pairs, and hope it all works out.
  go ((PTuple [p] _, u) :*: xs) = go ((p, u) :*: xs)
  -- ConCon for tuples: This always matches, so we just visit the children.
  go ((PTuple (p1:p2) a, VTuple u1 u2) :*: xs) = do
    (u1', (u2', xs')) <- go ((p1, u1) :*: (mkTuple p2 a, u2) :*: xs)
    pure (VTuple u1' u2', xs')
  -- ConVar for tuples: This always matches, so we just build up child value
  -- abstractions and then visit as normal.
  go ((p@(PTuple _ _), VVariable v vTy) :*: xs) = do
    (ty1, ty2) <- tupleTy vTy
    u1 <- flip VVariable ty1 <$> genName
    u2 <- flip VVariable ty2 <$> genName
    onVar p v (VTuple u1 u2) xs

  go ((PTuple{}, _) :*: _) = error "Mismatch on PTuple"

  -- ConCon for records: This always matches, so we just visit each field.
  go ((PRecord ps _, VRecord us') :*: xs) = first VRecord <$> foldRecord ps us' xs
  -- ConVar for records: This always matches, so we build up a child value for
  -- each field and visit as normal.
  go ((p@(PRecord _ (_, ty)), VVariable v vTy) :*: xs) = do
    constrain [mkUni vTy ty]
    us <- traverse (\ty -> flip VVariable ty <$> genName) (rowTy ty)
    onVar p v (VRecord us) xs
  go ((PRecord{}, _) :*: _) = error "Mismatch on PTuple"

  -- Var: Match the remaining patterns, extend the environment, and unify the
  -- bound variable with the abstraction.
  go ((w@(Wildcard _), u) :*: xs) = do
    guardInhabited (getType w)
    (u,) <$> go xs
  go ((w@(Capture v _), u) :*: xs) = do
    constrainVal v u
    guardInhabited (getType w)
    (u,) <$> go xs
  go ((PAs p v _, u) :*: xs) = onVar p v u xs

  -- Boring wrappers
  go ((PType p _ _, u) :*: xs) = go ((p, u) :*: xs)
  go ((PList{}, _) :*: _) = error "PList is handled by desugar"

  -- | Add a unification constraint between a variable and value
  -- abstraction, then continue matching
  onVar :: Pattern Typed -> Var Typed -> ValueAbs Typed -> PatPair a -> CoveringM (ValueAbs Typed, a)
  onVar p v u xs = constrainVal v u >> go ((p, u) :*: xs)

  mkTuple [] _ = error "Empty tuple"
  mkTuple [x] _ = x
  mkTuple xs a = PTuple xs a

  tupleTy (TyTuple t1 t2) = pure (t1, t2)
  tupleTy ty = do
    v1 <- freshTV; v2 <- freshTV
    constrain [mkUni ty (TyTuple v1 v2)]
    pure (v1, v2)

  rowTy (TyVar _) = mempty
  rowTy (TyRows f fs) = rowTy f <> Map.fromList fs
  rowTy (TyExactRows fs) = Map.fromList fs
  rowTy (TyParens t) = rowTy t
  rowTy (TyWithConstraints _ t) = rowTy t
  rowTy t = error $ "Cannot get row from "++ show t

  -- | Abort if this type is uninhabited.
  guardInhabited :: Type Typed -> CoveringM ()
  guardInhabited ty = do
    (solve, cs) <- get
    name <- genName
    unless (inhabited env (AbsState solve cs name) ty) mzero

  -- | Abort if the remaining pairs are uninhabited.
  guardAll :: PatPair a -> CoveringM ()
  guardAll Nil = pure ()
  guardAll ((x, _) :*: xs) = do
    guardInhabited (getType x)
    guardAll xs

  -- | Visit over each field in a record
  foldRecord :: [(T.Text, Pattern Typed)] -> Map.Map T.Text (ValueAbs Typed)
             -> PatPair a -> CoveringM (Map.Map T.Text (ValueAbs Typed), a)
  foldRecord [] vs xs = (vs,) <$> go xs
  foldRecord ((f, p):fs) us xs = do
    let u = fromMaybe (error $ "Cannot find field " ++ show f ++ " in record") (Map.lookup f us)
    (us', (u', xs')) <- foldRecord fs us ((p, u) :*: xs)
    pure (Map.insert f u' us', xs')

-- | Return all constructors matching this constraint
constructors :: (MonadPlus m, MonadState CoverState m, MonadNamey m)
             => Env -> Type Typed -> Type Typed
             -> m (Var Typed, Maybe (Type Typed))
constructors env kty vty = do
  k <- asum . map pure . toList . ctors $ kty
  (pty, _) <- skolGadt k =<< instantiate Strong Expression (fromJust (env ^. (names . at k)))
  let (cs, ty) = case pty of
                   TyWithConstraints cs ty -> (cs, ty)
                   _ -> ([], pty)
      (arg, res) = unwrapCtor ty

  -- Unify our result with the child, including any additional GADT constraints.
  constrain (mkUni res (expandTypeWith (env ^. tySyms) vty):map (uncurry mkUni) cs)
  pure (k, arg)

  where
    -- | Get the constructors for a given type
    ctors :: Type Typed -> Set.Set (Var Typed)
    ctors (TyCon v) = fromMaybe (error $ "Cannot find constructors for " ++ show v) (env ^. (types . at v))
    ctors (TyApp f _) = ctors f
    ctors t = error $ "Cannot get type name from " ++ show (pretty t)

    -- | Unwrap a constructor, returning the argument and result type
    unwrapCtor :: Type p -> (Maybe (Type p), Type p)
    unwrapCtor (TyPi bind res) = case bind of
      Anon arg -> (Just arg, res)
      Implicit _ -> unwrapCtor res
      Invisible _ _ _ -> unwrapCtor res
    unwrapCtor (TyWithConstraints _ t) = unwrapCtor t
    unwrapCtor (TyParens t) = unwrapCtor t
    unwrapCtor t = (Nothing, t)

-- | Determines if a given type is inhabited
inhabited :: Env -> AbsState -> Type Typed -> Bool
inhabited env (AbsState st cs i)
  = not . Seq.null
  . flip evalNameyT i
  . flip evalStateT (st, cs)
  . go mempty
  where

  inhb :: Monad m => m ()
  inhb = pure ()

  go :: (MonadPlus m, MonadNamey m, MonadState CoverState m)
     => Set.Set (Var Typed) -> Type Typed -> m ()
  go c (TyVar v) = do
    let c' = Set.insert v c
    t' <- gets (typeWithin v . fst)
    case t' of
      -- If there's no substitution, or a loop in the substitution, assume it's inhabited.
      Nothing -> inhb
      Just (TyVar v') | Set.member v' c' -> inhb
      Just t' -> go c' t'

  go c t@TyApp{} = ctorCheck c t
  go c t@TyCon{} = ctorCheck c t
  -- We don't really have the concept of uninhabited types, so we assume
  -- type names are.
  go _ TyPromotedCon{} = inhb
  -- For now, we'll just assume all Pi types are inhabited. Thankfully,
  -- they all are.
  go _ TyPi{} = inhb
  -- All the boring types: just determine if the children are inhabited
  go c (TyRows f fs) = go c f >> traverse_ (go c . snd) fs
  go c (TyExactRows fs) = traverse_ (go c . snd) fs
  go c (TyTuple l r) = go c l >> go c r
  go c (TyOperator l v r) = go c (TyApp (TyApp (TyCon v) l) r)
  go c (TyWildcard t) = maybe inhb (go c) t
  go c (TyParens t) = go c t

  go _ TySkol{} = inhb
  go c (TyWithConstraints _ t) = go c t
  go _ TyType = inhb
  go _ TyLit{} = inhb
  go _ TyTupleL{} = inhb

  -- | Returns the type name if this type is concrete (defined as has 0 or more
  -- constructors, rather than being abstract).
  --
  -- We pass in a set of previously visited type names to avoid getting into
  -- loops.
  concreteTy c (TyCon v)
    | v `Set.notMember` c
    , isJust (env ^. (types . at v))
    = Just v
    | otherwise = Nothing
  concreteTy c (TyApp f _) = concreteTy c f
  concreteTy _ _ = Nothing

  -- | A naive check to determine if a constructor is inhabited.
  ctorCheck :: (MonadPlus m, MonadNamey m, MonadState CoverState m)
            => Set.Set (Var Typed) -> Type Typed -> m ()
  ctorCheck c t
    | Just v <- concreteTy c t = do
        let c' = Set.insert v c
        (_, arg) <- constructors env t t
        maybe inhb (go c') arg
    | otherwise = inhb

-- | Make a unification constraint
mkUni :: Type Typed -> Type Typed -> Constraint Typed
mkUni = ConUnify (It'sThis (BecauseInternal "pmcheck")) mempty undefined

-- | Add one or more type constraints into the current environment,
-- failing if an error occurs.
constrain :: (MonadPlus m, MonadNamey m, MonadState CoverState m) => [Constraint Typed] -> m ()
constrain css = maybe empty put =<< doConstrain css where
  doConstrain css = do
    (sub, cs) <- get
    x <- runChronicleT . solveImplies sub mempty . (cs<>) $ Seq.fromList css
    pure $ case x of
      These Seq.Empty (sub', cs') -> Just (sub', Seq.fromList cs')
      That (sub', cs') ->  Just (sub', Seq.fromList cs')
      These _ _ -> Nothing
      This _ -> Nothing

-- | Require a variable to match a given pattern
--
-- Note this is not currently used, but may do something useful in the
-- future when we add view patterns.
constrainVal :: Var Typed -> ValueAbs Typed -> CoveringM ()
constrainVal _ _ = pure ()
