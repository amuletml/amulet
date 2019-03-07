{-# LANGUAGE StandaloneDeriving, FlexibleContexts, FlexibleInstances,
   UndecidableInstances, ScopedTypeVariables, TemplateHaskell #-}
module Syntax.Implicits
  ( ImplicitScope
  , Obligation(..), Sort(..)
  , Implicit(..), implHead, implPre, implVar, implType, implSort, implSpan
  , lookup, keys, mapTypes, subTrie
  , insert, singleton
  , spine, splitImplVarType
  , matches, overlap
  )
  where

import qualified Data.Map.Merge.Strict as Map
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Text (Text)
import Data.Semigroup
import Data.Function
import Data.Foldable
import Data.Maybe
import Data.List hiding (insert, lookup)

import Control.Lens

import Syntax hiding ((:>))
import Prelude hiding (lookup)

-- | An obligation the solver needs to resolve if it chose this
-- implicit parameter.
data Obligation p
  -- | A 'TyForall' (or similar) that was skipped over to get to the head of this type.
  = Quantifier (TyBinder p)
  -- | An implicit parameter to an implicit value.
  | Implication (Type p)

data Sort = InstSort | Superclass | LocalAssum
  deriving (Eq, Show, Ord)

deriving instance (Show (Var p), Show (Ann p)) => Show (Obligation p)
deriving instance Ord (Var p) => Ord (Obligation p)
deriving instance Eq (Var p) => Eq (Obligation p)

-- | A concrete representation of /one/ implicit parameter, stored in an
-- implicit parameter scope (a trie, indexed by the 'head').
data Implicit p
  -- | The choice for this implicit parameter
  = ImplChoice { _implHead :: Type p -- ^ The 'head' of the implicit parameter, i.e. the return type
               , _implType :: Type p
                  -- ^ The /entire/ type of the implicit parameter, with all quantifiers and such
               , _implPre :: [Obligation p] -- ^ The list of 'pre'conditions this choice implies
               , _implVar :: Var p -- ^ The actual implicit
               , _implSort :: Sort -- ^ What kind of implicit is this? (instance, superclass axiom)
               , _implSpan :: Ann Resolved -- ^ Where was this implicit defined?
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
  -- | There are choices to consider, but there are also more to go
  | ManyMore [Implicit p] (ImplicitScope p)

deriving instance (Show (Var p), Show (Ann p)) => Show (Node p)
deriving instance Ord (Var p) => Ord (Node p)
deriving instance Eq (Var p) => Eq (Node p)

-- | A trie of implicit choices.
newtype ImplicitScope p = Trie (Map.Map (Type p) (Node p))

deriving instance (Show (Var p), Show (Ann p)) => Show (ImplicitScope p)
deriving instance Ord (Var p) => Ord (ImplicitScope p)
deriving instance Eq (Var p) => Eq (ImplicitScope p)

-- | Insert a choice for a *fully-known* (@Solved@) implicit parameter
-- (the variable @v@) of type @tau@ at the given trie.
insert :: forall p. Ord (Var p) => Ann Resolved -> Sort -> Var p -> Type p -> ImplicitScope p -> ImplicitScope p
insert annot sort v ty = go ts implicit where
  (head, obligations) = getHead ty

  implicit = ImplChoice head ty (toList obligations) v sort annot
  ts = spine head

  go [] _ _ = error "empty spine (*very* malformed type?)"
  go [x] i (Trie m) = Trie (Map.alter (`join` i) x m)
      where join (Just (One x)) i = Just (Some [i, x])
            join (Just (Some xs)) i = Just (Some (i:xs))
            join Nothing i = Just (One i)
            join _ _ = Nothing

  go (x:xs) i (Trie l) = Trie (Map.alter (insert' xs i) x l) where
    insert' :: [Type p] -> Implicit p -> Maybe (Node p) -> Maybe (Node p)
    insert' xs i (Just (Many t)) = Just (Many (go xs i t))
    insert' xs i (Just (One t)) = Just (ManyMore [t] (go xs i mempty))
    insert' xs i (Just (Some ts)) = Just (ManyMore ts (go xs i mempty))
    insert' xs i (Just (ManyMore ts t)) = Just (ManyMore ts (go xs i t))
    insert' xs i Nothing = Just (Many (go xs i (Trie Map.empty)))

-- | Find a type in a trie by conservative fuzzy search.
lookup :: forall p. Ord (Var p) => Type p -> ImplicitScope p -> [Implicit p]
lookup ty = go ts where
  ts = spine ty
  go :: [Type p] -> ImplicitScope p -> [Implicit p]
  go [x] (Trie m) = case find x m of
    Just (One x) -> [x]
    Just (Some xs) -> xs
    Just (ManyMore xs _) -> xs
    _ -> []
  go (x:xs) (Trie m) = case find x m of
    Just (Many m) -> go xs m
    Just (ManyMore ss m) -> ss ++ go xs m
    Just (One x) -> [x] -- discard xs
    Just (Some xs) -> xs -- discard xs
    _ -> []
  go [] Trie{} = error "badly-kinded type (empty spine?)"

  find :: Type p -> Map.Map (Type p) (Node p) -> Maybe (Node p)
  find w = fixup . toList . Map.filterWithKey (\k _ -> w `matches` k) where
    fixup []  = Nothing
    fixup [x] = Just x
    fixup (x:xs) = Just (sconcat (x :| xs))

instance Ord (Var p) => Monoid (ImplicitScope p) where
  mempty = Trie mempty

instance Ord (Var p) => Semigroup (ImplicitScope p) where
  Trie m <> Trie m' = Trie (merge m m')

instance Ord (Var p) => Semigroup (Node p) where
  One x <> One y = Some [x, y]
  One x <> Some xs = Some (x:xs)
  One x <> ManyMore xs t = ManyMore (x:xs) t
  One x <> Many t = ManyMore [x] t

  Some x <> One y = Some (x `snoc` y)
  Some x <> Some y = Some (x ++ y)
  Some x <> ManyMore xs t = ManyMore (x ++ xs) t
  Some x <> Many t = ManyMore x t

  Many x <> Many y = Many (x <> y)
  Many t <> One x = ManyMore [x] t
  Many t <> Some xs = ManyMore xs t
  Many t <> ManyMore xs t' = ManyMore xs (t <> t')

  ManyMore xs ts <> One x = ManyMore (xs `snoc` x) ts
  ManyMore xs ts <> Some ys = ManyMore (xs ++ ys) ts
  ManyMore xs ts <> ManyMore ys ts' = ManyMore (xs ++ ys) (ts <> ts')
  ManyMore xs ts <> Many ts' = ManyMore xs (ts <> ts')

-- | Compute the set of keys in a scope. Note that this operation takes
-- time proportional to the number of elements in the trie!
keys :: forall p. Ord (Var p) => ImplicitScope p -> Set.Set (Type p)
keys = go where
  go (Trie m) = foldMap goNode m

  goNode (One x) = x ^. implHead & Set.singleton
  goNode (Some xs) = foldMapOf (each . implHead) Set.singleton xs
  goNode (Many t) = go t
  goNode (ManyMore xs t) = foldMapOf (each . implHead) Set.singleton xs <> go t

-- | Map a function over the elements of a trie. Note that, because of
-- the way they are stored, the function will be applied many times to
-- distinct parts of possibly the same type.
mapTypes :: forall p. (Ord (Var p)) => (Type p -> Type p) -> ImplicitScope p -> ImplicitScope p
mapTypes fn = go where
  go (Trie m) = Trie (Map.foldrWithKey (\k x r -> makeTrie (change fn k) (goNode x) `merge` r) mempty m)

  change fn k =
    let sp = spine k
        k' = fn k
        sp' = spine k'
     in if sp == sp'
           then [k']
           else sp'

  makeTrie :: [Type p] -> Node p -> Map.Map (Type p) (Node p)
  makeTrie [x] n = Map.singleton x n
  makeTrie (x:xs) n = Map.singleton x (Many (Trie (makeTrie xs n)))
  makeTrie [] _ = error "a node was left dangling while balancing trie"

  goNode (One x) = One (goI x)
  goNode (Some xs) = Some (map goI xs)
  goNode (Many t) = Many (go t)
  goNode (ManyMore xs t) = ManyMore (map goI xs) (go t)

  goI (ImplChoice h t o v s a) = ImplChoice (fn h) (fn t) (map goO o) v s a

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

-- | Make a trie consisting of the only the one given implicit.
singleton :: Ord (Var p) => Ann Resolved -> Sort -> Var p -> Type p -> ImplicitScope p
singleton a s v t = insert a s v t mempty

-- | Decompose a type into its main "spine" of left-nested applications.
-- @
--  spine (f x y (a b)) = [ f, x, y, a b ]
-- @
spine :: Type p -> [Type p]
spine (TyApp f x) = spine f `snoc` x
spine t = [t]

getHead, splitImplVarType :: Type p -> (Type p, Seq.Seq (Obligation p))
getHead t@TyVar{} = (t, Seq.empty)
getHead t@TyCon{} = (t, Seq.empty)
getHead t@TyPromotedCon{} = (t, Seq.empty)
getHead t@TyApp{} = (t, Seq.empty)
getHead (TyPi b t) = case b of
  Anon v -> (TyArr v t, Seq.empty) -- regular function, do nothing
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
getHead t@TyWildcard{} = (t, Seq.empty)
getHead (TyParens t) = getHead t
getHead t@TyOperator{} = (t, Seq.empty)

-- | Split the type of an implicit variable into its head and a set of
-- obligations.
splitImplVarType = getHead

merge :: Ord (Var p) => Map.Map (Type p) (Node p) -> Map.Map (Type p) (Node p) -> Map.Map (Type p) (Node p)
merge = Map.merge Map.preserveMissing Map.preserveMissing (Map.zipWithMatched (const (<>)))

-- | Does there exist a substitution that can make a the same as b?
-- (Conservative check.)
matches :: Ord (Var p) => Type p -> Type p -> Bool
matches (TyParens x) x' = matches x x'
matches x (TyParens x') = matches x x'

matches TyVar{} _ = True
matches _ TyVar{} = True

matches TyWildcard{} _ = True
matches _ TyWildcard{} = True

matches (TyCon t) (TyCon t') = t == t'
matches TyCon{} _ = False

matches (TyPromotedCon t) (TyPromotedCon t') = t == t'
matches TyPromotedCon{} _ = False

matches (TyApp f x) (TyApp f' x') = f `matches` f' && x `matches` x'
matches TyApp{} _ = False

matches (TyOperator l o r) x' = matches ((TyCon o `TyApp` l) `TyApp` r) x'
matches x (TyOperator l o r) = matches x ((TyCon o `TyApp` l) `TyApp` r)

matches (TyPi b t) (TyPi b' t') = t `matches` t' && b `matchesBinder` b' where
  matchesBinder (Anon t) (Anon t') = t `matches` t'
  matchesBinder (Implicit t) (Implicit t') = t `matches` t'
  matchesBinder (Invisible _ k) (Invisible _ k') = fromMaybe False (matches <$> k <*> k')
  matchesBinder _ _ = False

matches TyPi{} _ = False

matches (TyRows _ vs) (TyExactRows vs') = length vs' >= length vs && all m (overlap vs vs') where
  m (_, a, b) = a `matches` b
matches (TyRows rho xs) (TyRows sigma ys)
  | over <- overlap xs ys
  = rho `matches` sigma && length ys >= length xs && all m over
    where m (_, a, b) = a `matches` b
matches TyRows{} _ = False

matches (TyExactRows vs) (TyExactRows vs') = length vs' == length vs && all m (overlap vs vs') where
  m (_, a, b) = a `matches` b

matches (TyExactRows vs') (TyRows _ vs) = length vs' >= length vs && all m (overlap vs vs') where
  m (_, a, b) = a `matches` b

matches TyExactRows{} _ = False

matches (TyTuple a b) (TyTuple a' b') = matches a a' && matches b b'
matches TyTuple{} _ = False

matches (TySkol v) (TySkol v') = v == v'
matches TySkol{} _ = False

matches (TyWithConstraints _ _) _ = error "matches TyWithConstraints"

matches TyType t = t == TyType

overlap :: [(Text, Type p)] -> [(Text, Type p)] -> [(Text, Type p, Type p)]
overlap xs ys
  | inter <- filter ((/=) 1 . length) $ groupBy ((==) `on` fst) (sortOn fst (xs ++ ys))
  = map get inter
  where get [(t, a), (_, b)] = (t, a, b)
        get _ = undefined
