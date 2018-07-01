{-# LANGUAGE StandaloneDeriving, FlexibleContexts, FlexibleInstances,
   UndecidableInstances, ScopedTypeVariables, TemplateHaskell #-}
module Syntax.Implicits where

import qualified Data.Map.Merge.Strict as Map
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Semigroup
import Data.Foldable

import Control.Lens

import Syntax.Pretty hiding ((:>))

-- | An obligation the solver needs to resolve if it chose this
-- implicit parameter.
data Obligation p
  -- | A 'TyForall' (or similar that was skipped over to get to the head
  = Quantifier (TyBinder p)
  -- | An implicit parameter to an implicit value
  | Implication (Type p)

deriving instance (Show (Var p), Show (Ann p)) => Show (Obligation p)
deriving instance Ord (Var p) => Ord (Obligation p)
deriving instance Eq (Var p) => Eq (Obligation p)

-- | A concrete representation of /one/ implicit parameter, stored in an
-- implicit parameter scope (a trie, indexed by the 'head').
data Implicit p
  -- | The choice for this implicit parameter
  = ImplChoice { _implHead :: Type p -- ^ The 'head' of the implicit parameter, i.e. the return type
               , _implType :: Type p -- ^ The /entire/ type of the implicit parameter, with all quantifiers and such
               , _implPre :: [Obligation p] -- ^ The list of 'pre'conditions this choice implies
               , _implVar :: Var p -- ^ The actual implicit
               }

deriving instance (Show (Var p), Show (Ann p)) => Show (Implicit p)
deriving instance Ord (Var p) => Ord (Implicit p)
deriving instance Eq (Var p) => Eq (Implicit p)

makeLenses ''Implicit

-- | A node in the trie of implicits
data Node p
  -- | We're done
  = One (Implicit p)
  -- | There are multiple choices to consider
  | Some [Implicit p]
  -- | There are more to go
  | Many (ImplicitScope p)

deriving instance (Show (Var p), Show (Ann p)) => Show (Node p)
deriving instance Ord (Var p) => Ord (Node p)
deriving instance Eq (Var p) => Eq (Node p)

-- | A trie of implicit choices.
newtype ImplicitScope p = Trie (Map.Map (Type p) (Node p))

deriving instance (Show (Var p), Show (Ann p)) => Show (ImplicitScope p)
deriving instance Ord (Var p) => Ord (ImplicitScope p)
deriving instance Eq (Var p) => Eq (ImplicitScope p)

-- | Insert a choice for an implicit parameter (the variable @v@) of
-- type @tau@ at the given trie.
insert :: forall p. Ord (Var p) => Var p -> Type p -> ImplicitScope p -> ImplicitScope p
insert v ty = go ts implicit where
  (head, obligations) = getHead ty

  implicit = ImplChoice head ty (toList obligations) v
  ts = spine head

  go [] _ _ = error "empty spine (*very* malformed type?)"
  go [x] i (Trie m) = Trie (Map.alter (\node -> join node i) x m)
      where join (Just (One x)) i = Just (Some [i, x])
            join (Just (Some xs)) i = Just (Some (i:xs))
            join Nothing i = Just (One i)
            join _ _ = Nothing

  go (x:xs) i (Trie l) = Trie (Map.alter (\node -> insert' xs i node) x l) where
    insert' :: [Type p] -> Implicit p -> Maybe (Node p) -> Maybe (Node p)
    insert' xs i (Just (Many t)) = Just (Many (go xs i t))
    insert' xs i Nothing = Just (Many (go xs i (Trie Map.empty)))
    insert' _ _ (Just _) = error "badly-kinded type (end with spine remaining)"

-- | Find a type in a trie *exactly* (i.e., without considering type
-- variables)
lookup :: forall p. Ord (Var p) => Type p -> ImplicitScope p -> [Implicit p]
lookup ty = go ts where
  ts = spine ty
  go :: [Type p] -> ImplicitScope p -> [Implicit p]
  go [x] (Trie m) = case find x m of
    Just (One x) -> [x]
    Just (Some xs) -> xs
    Nothing -> []
    _ -> error "badly-kinded type (trie with no spine left)"
  go (x:xs) (Trie m) = case Map.lookup x m of
    Just (Many m) -> go xs m
    Nothing -> []
    Just _ -> error "badly-kinded type (end with spine remaining)"
  go [] Trie{} = error "badly-kinded type (empty spine?)"

  find :: Type p -> Map.Map (Type p) (Node p) -> Maybe (Node p)
  find w = fixup . toList . Map.filterWithKey (\k _ -> k `matches` w) where
    fixup []  = Nothing
    fixup [x] = Just x
    fixup (x:xs) = Just (sconcat (x :| xs))

instance Ord (Var p) => Monoid (ImplicitScope p) where
  mempty = Trie mempty

instance Ord (Var p) => Semigroup (ImplicitScope p) where
  Trie m <> Trie m' = Trie (merge m m') where
    merge = Map.merge Map.preserveMissing Map.preserveMissing (Map.zipWithMatched (const (<>)))

instance Ord (Var p) => Semigroup (Node p) where
  (<>) (One x) (One y) = Some [x, y]
  (<>) (One x) (Some xs) = Some (x:xs)
  (<>) One{} _ = error "malformed trie (Many and One at same level)"
  (<>) (Some x) (One y) = Some (x `snoc` y)
  (<>) (Some x) (Some y) = Some (x ++ y)
  (<>) Some{} _ = error "malformed trie (Many and Some at same level)"
  (<>) (Many x) (Many y) = Many (x <> y)
  (<>) Many{} _ = error "malformed trie (Many and Some/One at same level)"

-- | Compute the set of keys in a scope. Note that this operation takes
-- time proportional to the number of elements in the trie!
keys :: forall p. Ord (Var p) => ImplicitScope p -> Set.Set (Type p)
keys = go where
  go (Trie m) = foldMap goNode m

  goNode (One x) = x ^. implHead & Set.singleton
  goNode (Some xs) = foldMapOf (each . implHead) Set.singleton xs
  goNode (Many t) = go t

-- | Map a function over the elements of a trie. Note that, because of
-- the way they are stored, the function will be applied many times to
-- distinct parts of possibly the same type.
mapTypes :: forall p. Ord (Var p) => (Type p -> Type p) -> ImplicitScope p -> ImplicitScope p
mapTypes fn = go where
  go (Trie m) = Trie (Map.foldMapWithKey (\k x -> Map.singleton (fn k) (goNode x)) m)

  goNode (One x) = One (goI x)
  goNode (Some xs) = Some (map goI xs)
  goNode (Many t) = Many (go t)

  goI (ImplChoice h t o v) = ImplChoice (fn h) (fn t) (map goO o) v

  goO (Quantifier (Invisible v k)) = Quantifier (Invisible v (fn <$> k))
  goO (Quantifier _) = error "impossible quantifier"
  goO (Implication ts) = Implication (fn ts)

-- | Find the 'Many' node located at the end of the provided spine
-- section in the scope.
subTrie :: forall p. Ord (Var p) => [Type p] -> ImplicitScope p -> Maybe (ImplicitScope p)
subTrie = go where
  go (x:xs) (Trie m) = goNode xs (Map.lookup x m)
  go [] t = Just t

  goNode xs (Just (Many t)) = go xs t
  goNode _ _ = Nothing

singleton :: Ord (Var p) => Var p -> Type p -> ImplicitScope p
singleton v t = insert v t mempty

spine :: Type p -> [Type p]
spine (TyApp f x) = spine f `snoc` x
spine t = [t]

getHead :: Type p -> (Type p, Seq.Seq (Obligation p))
getHead t@TyVar{} = (t, Seq.empty)
getHead t@TyCon{} = (t, Seq.empty)
getHead t@TyPromotedCon{} = (t, Seq.empty)
getHead t@TyApp{} = (t, Seq.empty)
getHead (TyPi b t) = case b of
  Anon v -> (TyArr v t, Seq.empty) -- regular function, do nothing
  Explicit v k -> (TyPi (Explicit v k) t, Seq.empty) -- explicitly-quantified forall, do nothing
  Invisible{} ->
    let (hd, cs) = getHead t
     in (hd, Quantifier b `cons` cs)
  Implicit k ->
    let (hd, cs) = getHead t
     in (hd, Implication k `cons` cs)
getHead t@TyRows{} = (t, Seq.empty)
getHead t@TyExactRows{} = (t, Seq.empty)
getHead t@TyTuple{} = (t, Seq.empty)
getHead t@TySkol{} = (t, Seq.empty)
getHead t@TyWithConstraints{} = (t, Seq.empty)
getHead t@TyType = (t, Seq.empty)

-- | Does there exist a substitution that can make a the same as b?
-- (Conservative check.)
matches :: Ord (Var p) => Type p -> Type p -> Bool
matches TyVar{} _ = True
matches _ TyVar{} = True

matches (TyCon t) (TyCon t') = t == t'
matches TyCon{} _ = False

matches (TyPromotedCon t) (TyPromotedCon t') = t == t'
matches TyPromotedCon{} _ = False

matches (TyApp f x) (TyApp f' x') = f `matches` f' && x `matches` x'
matches TyApp{} _ = False

matches (TyPi b t) (TyPi b' t') = t `matches` t' && b `matchesBinder` b' where
  matchesBinder (Anon t) (Anon t') = t `matches` t'
  matchesBinder (Implicit t) (Implicit t') = t `matches` t'
  matchesBinder (Invisible _ k) (Invisible _ k') = maybe False id (matches <$> k <*> k')
  matchesBinder (Explicit _ k) (Explicit _ k') = k `matches` k'
  matchesBinder _ _ = False

matches TyPi{} _ = False

matches TyRows{} _ = False -- TODO
matches TyExactRows{} _ = False -- TODO

matches (TyTuple a b) (TyTuple a' b') = matches a a' && matches b b'
matches TyTuple{} _ = False

matches (TySkol v) (TySkol v') = v == v'
matches TySkol{} _ = False

matches (TyWithConstraints _ _) _ = error "matches TyWithConstraints"

matches TyType t = t == TyType
