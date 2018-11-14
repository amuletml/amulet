{-# LANGUAGE
  DeriveFunctor
, FlexibleContexts
, FlexibleInstances
, GADTs
, OverloadedStrings
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

[1]: https://www.microsoft.com/en-us/research/wp-content/uploads/2016/08/gadtpm-acm.pdf

-}
module Syntax.Verify.Pattern
  ( Covering(..)
  , ValueAbs, altAbs
  , ValueAlt
  , emptyAlt
  , covering
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
import Data.These
import Data.Maybe

import Syntax.Pretty
import Syntax.Subst
import Syntax.Types

import Types.Unify
import Types.Infer.Pattern (skolGadt)
import Types.Infer.Builtin (tyUnit)

import Text.Pretty.Semantic hiding (empty)

-- | Represents both the covered and uncovered set of a computation.
--
-- This could be thought of as a unification of the list and maybe monad.
data Covering a =
  Covering { covered :: Maybe a
           , uncovered :: Seq.Seq a
           }
  deriving (Show, Functor)

instance Applicative Covering where
  pure x = Covering (pure x) (pure x)
  (Covering fc fu) <*> (Covering xc xu) = Covering (fc <*> xc) (fu <*> xu)

instance Alternative Covering where
  empty = Covering empty empty
  (Covering lc lu) <|> (Covering rc ru) = Covering (lc <|> rc) (lu <|> ru)
  some (Covering c u) = Covering (some c) (some u)
  many (Covering c u) = Covering (many c) (many u)

instance Monad Covering where
  (Covering c u) >>= f = Covering (c >>= covered . f) (u >>= uncovered . f)

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
  | VDestructure (Var p) (Seq.Seq (Constraint Typed)) (Maybe (ValueAbs p))
  | VRecord (Map.Map T.Text (ValueAbs p))
  | VTuple (ValueAbs p) (ValueAbs p)

  | VLiteral Lit
  | VNotLiteral (Var p) (Set.Set Lit)

deriving instance (Show (Var p), Show (Ann p)) => Show (ValueAbs p)

instance Pretty (Var p) => Pretty (ValueAbs p) where
  pretty VVariable{} = skeyword "_"
  pretty (VDestructure k _ Nothing) = stypeCon (pretty k)
  pretty (VDestructure k _ (Just p)) = stypeCon (pretty k) <+>
    case p of
      VDestructure _ _ Just{} -> parens (prettyIn p)
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
data ValueAlt p
  = ValueAlt
  { altAbs :: ValueAbs p
  , altTyc :: Seq.Seq (Constraint p)
  , altSub :: Subst p
  , altFresh :: Int
  }

deriving instance (Show (Var p), Show (Ann p)) => Show (ValueAlt p)

-- | Construct an empty pattern alternative state from a fresh variable,
-- and the type of the test variable.
emptyAlt :: Var p ~ Var Resolved => Var p -> Type p -> ValueAlt p
emptyAlt v@(TgName _ i) ty = ValueAlt (VVariable v ty) mempty mempty (i + 1)
emptyAlt _ _ = error "Require TgName for emptyAlt"

-- | A type-safe list of constraints between patterns and value
-- abstractions.
--
-- We use this to ensure that each iteration of the solver consumes and
-- produces exactly the same number of terms.
data PatPair a where
  Nil :: PatPair ()
  (:*:) :: (Pattern Typed, ValueAbs Typed) -> PatPair b -> PatPair (ValueAbs Typed, b)

infixr 9 :*:

instance Pretty (PatPair a) where
  pretty Nil = "()"
  pretty ((a, b) :*: xs) = pretty a <+> "<->" <+> pretty b <+> ":*:" <+> pretty xs

-- | Extract the pattern part of a 'PatPair', suitable for building an
-- uncovered set.
patPair :: PatPair a -> a
patPair Nil = ()
patPair ((_, x) :*: xs) = (x, patPair xs)

-- | The type of the covering monad
type CoveringM = StateT (Subst Typed, Seq.Seq (Constraint Typed)) (NameyT Covering)

-- | Run the value abstraction monad.
covering :: Env -> Pattern Typed
         -> Seq.Seq (ValueAlt Typed)
         -> Covering (ValueAlt Typed)
covering env p xs = msum . flip fmap xs $ \(ValueAlt v tc sub i) -> do
  ~(((v', ()), (sub', tc')), TgName _ i') <- flip runNameyT (TgName "a" i)
                                           . flip runStateT (sub, tc)
                                           $ covering' env ((p, v) :*: Nil)
  pure $ ValueAlt v' tc' sub' i'

covering' :: Env -> PatPair (a, ()) -> CoveringM (a, ())
covering' env x = do (cs, r) <- go x
                     constrain cs
                     pure r
  where
  go :: PatPair a -> CoveringM (Seq.Seq (Constraint Typed), a)

  -- Nil
  go Nil = cover (mempty, ())

  -- ConCon for constructors: If the constructors are the same, just
  -- recurse. Otherwise, that constructor is uncovered.
  go ((Destructure k (Just p) _, v@(VDestructure k' cs (Just u))) :*: xs)
    | k == k' = bimap (pure . ConImplies undefined tyUnit cs) (first (VDestructure k' cs . Just))
                <$> go ((p, u):*:xs)
    | otherwise = uncover (empty, (v, patPair xs))
  go ((Destructure k Nothing _, v@(VDestructure k' cs Nothing)) :*: xs)
    | k == k' = bimap (pure . ConImplies undefined tyUnit cs) (VDestructure k' cs Nothing,)
                <$> go xs
    | otherwise = uncover (empty, (v, patPair xs))
  go ((Destructure{}, v@VDestructure{}) :*: xs) = uncover (empty, (v, patPair xs))

  -- ConVar for constructors: We always perform the UConVar implementation here -
  -- namely we find every possible case.
  go ((p@(Destructure k _ _), VVariable v vTy) :*: xs) = msum . flip map (toList ctors) $ \k -> do
    (pty, _) <- skolGadt k =<< instantiate Expression (fromJust (env ^. (names . at k)))
    let (pcs, ty) = case pty of
                     TyWithConstraints cs ty -> (cs, ty)
                     _ -> ([], pty)
        (arg, res) = unwrapCtor ty
        cs = Seq.fromList $ mkUni res vTy:map (uncurry mkUni) pcs

    -- Perform an initial constraint on this type, to exclude any impossible cases.
    constrain . pure $ ConImplies undefined tyUnit cs empty

    u <- case arg of
      Nothing -> pure $ VDestructure k cs Nothing
      Just arg -> do
        v' <- genName
        pure $ VDestructure k cs (Just (VVariable v' arg))

    -- And wrap any constraints inside this one
    first (pure . ConImplies undefined tyUnit cs) <$> onVar p v u xs

    where
      ctors = let ty = maybe (error $ "Cannot find type of " ++ show k) typeName (env ^. (names . at k))
                  cs = fromMaybe (error $ "Cannot find constructors for " ++ show ty) (env ^. (types . at ty))
              in cs

  go ((Destructure{}, _) :*: _) = error "Mismatch on Destructure"

  -- ConCon for literals
  go ((PLiteral li _, v@(VLiteral li')) :*: xs)
    | li == li' = fmap (v,) <$> go xs
    | otherwise = uncover (empty, (v, patPair xs))

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
    | otherwise = uncover (empty, (n, patPair xs))

  go ((PLiteral{}, _) :*: _) = error "Mismatch on PLiteral"

  -- As tuple patterns can be of differing sizes, we can't just zip them
  -- up. Thus we represent them as pairs, and hope it all works out.
  go ((PTuple [p] _, u) :*: xs) = go ((p, u) :*: xs)
  -- ConCon for tuples: This always matches, so we just visit the children.
  go ((PTuple (p1:p2) (a, ty), VTuple u1 u2) :*: xs) = do
    (_, ty2) <- tupleTy ty
    (c1, (u1', (u2', xs'))) <- go ((p1, u1) :*: (mkTuple p2 (a, ty2), u2) :*: xs)
    pure (c1, (VTuple u1' u2', xs'))
  -- ConVar for tuples: This always matches, so we just build up child value
  -- abstractions and then visit as normal.
  go ((p@(PTuple _ (_, ty)), VVariable v vTy) :*: xs) = do
    (ty1, ty2) <- tupleTy ty
    constrain . pure $ mkUni vTy ty

    u1 <- flip VVariable ty1 <$> genName
    u2 <- flip VVariable ty2 <$> genName
    onVar p v (VTuple u1 u2) xs

  go ((PTuple{}, _) :*: _) = error "Mismatch on PTuple"

  -- ConCon for records: This always matches, so we just visit each field.
  go ((PRecord ps _, VRecord us') :*: xs) = fmap (first VRecord) <$> foldRecord ps us' xs
  -- ConVar for records: This always matches, so we build up a child value for
  -- each field and visit as normal.
  go ((p@(PRecord _ (_, ty)), VVariable v vTy) :*: xs) = do
    constrain . pure $ mkUni vTy ty
    us <- traverse (\ty -> flip VVariable ty <$> genName) (rowTy ty)
    onVar p v (VRecord us) xs
  go ((PRecord{}, _) :*: _) = error "Mismatch on PTuple"

  -- Var: Match the remaining patterns, extend the environment, and unify the
  -- bound variable with the abstraction.
  go ((Wildcard _, u) :*: xs) = fmap (u,) <$> go xs
  go ((Capture v _, u) :*: xs) = do
    constrainVal v u
    fmap (u,) <$> go xs
  go ((PAs p v _, u) :*: xs) = onVar p v u xs

  -- Boring wrappers
  go ((PType p _ _, u) :*: xs) = go ((p, u) :*: xs)
  go ((PSkolem p _ _, u) :*: xs) = go ((p, u) :*: xs)
  go ((PWrapper _ p _, u) :*: xs) = go ((p, u) :*: xs)

  -- | Add a unification constraint between a variable and value
  -- abstraction, then continue matching
  onVar :: Pattern Typed -> Var Typed -> ValueAbs Typed -> PatPair a
        -> CoveringM (Seq.Seq (Constraint Typed), (ValueAbs Typed, a))
  onVar p v u xs = constrainVal v u >> go ((p, u) :*: xs)

  mkTuple [] _ = error "Empty tuple"
  mkTuple [x] _ = x
  mkTuple xs a = PTuple xs a

  tupleTy (TyTuple t1 t2) = pure (t1, t2)
  tupleTy ty = do
    v1 <- freshTV; v2 <- freshTV
    constrain . pure $ mkUni ty (TyTuple v1 v2)
    pure (v1, v2)

  rowTy (TyVar _) = mempty
  rowTy (TyRows f fs) = rowTy f <> Map.fromList fs
  rowTy (TyExactRows fs) = Map.fromList fs
  rowTy (TyParens t) = rowTy t
  rowTy (TyWithConstraints _ t) = rowTy t
  rowTy t = error $ "Cannot get row from "++ show t

  -- | Visit over each field in a record
  foldRecord :: [(T.Text, Pattern Typed)] -> Map.Map T.Text (ValueAbs Typed)
             -> PatPair a -> CoveringM (Seq.Seq (Constraint Typed), (Map.Map T.Text (ValueAbs Typed), a))
  foldRecord [] vs xs = fmap (vs,) <$> go xs
  foldRecord ((f, p):fs) us xs = do
    let u = fromMaybe (error $ "Cannot find field " ++ show f ++ " in record") (Map.lookup f us)
    (cs, (us', (u', xs'))) <- foldRecord fs us ((p, u) :*: xs)
    pure (cs, (Map.insert f u' us', xs'))

-- | Unwrap a constructor, returning the argument and result type
unwrapCtor :: Type p -> (Maybe (Type p), Type p)
unwrapCtor (TyPi bind res) = case bind of
  Anon arg -> (Just arg, res)
  Implicit _ -> unwrapCtor res
  Invisible _ _ -> unwrapCtor res
unwrapCtor (TyWithConstraints _ t) = unwrapCtor t
unwrapCtor (TyParens t) = unwrapCtor t
unwrapCtor t = (Nothing, t)

-- | Get the name of this type constructor
typeName :: Show (Type p) => Type p -> Var p
typeName (TyPi _ res) = typeName res
typeName (TyCon t) = t
typeName (TyApp f _) = typeName f
typeName (TyWithConstraints _ t) = typeName t
typeName (TyParens t) = typeName t
typeName (TyOperator _ t _) = t
typeName t = error ("Unknown type " ++ show t)

-- | Make a unification constraint
mkUni :: Type Typed -> Type Typed -> Constraint Typed
mkUni = ConUnify undefined undefined

-- | Add one or more type constraints into the current environment,
-- failing if an error occurs.
constrain :: (Seq.Seq (Constraint Typed)) -> CoveringM ()
constrain css = do
  (sub, cs) <- get
  x <- runChronicleT . solveWith sub $ cs <> css
  case x of
    These Seq.Empty (sub', _, cs') -> put (sub', Seq.fromList cs')
    That (sub', _, cs') -> put (sub', Seq.fromList cs')
    These _ _ -> empty
    This _ -> empty

-- | Require a variable to match a given pattern
--
-- Note this is not currently used, but may do something useful in the
-- future when we add view patterns.
constrainVal :: Var Typed -> ValueAbs Typed -> CoveringM ()
constrainVal _ _ = pure ()
