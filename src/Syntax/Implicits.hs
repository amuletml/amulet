{-# LANGUAGE StandaloneDeriving, FlexibleContexts, FlexibleInstances,
   UndecidableInstances, ScopedTypeVariables, TemplateHaskell, MultiParamTypeClasses, GADTs #-}
module Syntax.Implicits
  ( ImplicitScope
  , Obligation(..), Sort(..)
  , Implicit(..), implHead, implPre, implVar, implType, implSort, implSpan, implClass
  , lookup, keys, subTrie
  , insert, singleton
  , splitImplVarType
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

import Syntax.Subst

import Core.Builtin
import Core.Var

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
data Implicit info p
  -- | The choice for this implicit parameter
  = ImplChoice { _implHead :: Type p -- ^ The 'head' of the implicit parameter, i.e. the return type
               , _implType :: Type p
                  -- ^ The /entire/ type of the implicit parameter, with all quantifiers and such
               , _implPre :: [Obligation p] -- ^ The list of 'pre'conditions this choice implies
               , _implVar :: Var p -- ^ The actual implicit
               , _implSort :: Sort -- ^ What kind of implicit is this? (instance, superclass axiom)
               , _implSpan :: Ann Resolved -- ^ Where was this implicit defined?
               , _implClass :: info
                 -- ^ Class information for the implicit
                 -- Invariant: 'implClass' is only defined if 'implSort' == 'InstSort'
               }


deriving instance (Show info, Show (Var p), Show (Ann p)) => Show (Implicit info p)
deriving instance (Ord info, Ord (Var p)) => Ord (Implicit info p)
deriving instance (Eq info, Eq (Var p)) => Eq (Implicit info p)

makeLenses ''Implicit

-- | A node in the trie of implicits
data Node info p
  -- | We're done
  = One (Implicit info p)
  -- | There are multiple choices to consider
  | Some [Implicit info p]
  -- | There are more to go
  | Many (ImplicitScope info p)
  -- | There are choices to consider, but there are also more to go
  | ManyMore [Implicit info p] (ImplicitScope info p)

deriving instance (Show info, Show (Var p), Show (Ann p)) => Show (Node info p)
deriving instance (Ord info, Ord (Var p)) => Ord (Node info p)
deriving instance (Eq info, Eq (Var p)) => Eq (Node info p)

-- | A trie of implicit choices.
newtype ImplicitScope info p = Trie (Map.Map (Type p) (Node info p))

deriving instance (Show info, Show (Var p), Show (Ann p)) => Show (ImplicitScope info p)
deriving instance (Ord info, Ord (Var p)) => Ord (ImplicitScope info p)
deriving instance (Eq info, Eq (Var p)) => Eq (ImplicitScope info p)

instance Ord (Var p) => Substitutable p (ImplicitScope i p) where
  ftv = foldMap ftv . keys
  apply m (Trie trie) = Trie . rebalance $ fmap (apply m) trie where
    rebalance :: Map.Map (Type p) (Node i p) -> Map.Map (Type p) (Node i p)
    rebalance = Map.fromListWith (<>) . go . Map.toList

    go :: [(Type p, Node i p)] -> [(Type p, Node i p)]
    go ((k, v):rest) =
      if length (appsView new) == length (appsView k)
         then (new, v):go rest
         else
           case appsView new of
             -- [x] -> (x, v):go rest
             (x:xs) -> (x, makeTrie xs v):go rest
             [] -> error "empty spine"
      where new = apply m' k
            m' = m `Map.withoutKeys` don'tSubstitute v
    go [] = []

    makeTrie :: [Type p] -> Node i p -> Node i p
    makeTrie [] n = n
    makeTrie (x:xs) n = Many (Trie (Map.singleton x (makeTrie xs n)))

instance Ord (Var p) => Substitutable p (Node i p) where
  ftv (One i) = ftv i
  ftv (Some is) = ftv is
  ftv (Many t) = ftv t
  ftv (ManyMore is m) = ftv is <> ftv m

  apply m (One impl) = One (apply m impl)
  apply m (Some is) = Some (apply m is)
  apply m (Many t) = Many (apply m t)
  apply m (ManyMore is t) = ManyMore (apply m is) (apply m t)

instance Ord (Var p) => Substitutable p (Implicit i p) where
  ftv i = ftv (i ^. implType) Set.\\ boundByImpl i
  apply m i = i & implHead %~ apply m' & implType %~ apply m' where
    m' = m `Map.withoutKeys` boundByImpl i

-- | Insert a choice for a *fully-known* (@Solved@) implicit parameter
-- (the variable @v@) of type @tau@ at the given trie.
insert :: forall i p. Ord (Var p) => Ann Resolved -> Sort -> Var p -> Type p -> i -> ImplicitScope i p -> ImplicitScope i p
insert annot sort v ty info = go ts implicit where
  (head, obligations) = getHead ty

  implicit = ImplChoice head ty (toList obligations) v sort annot info
  ts = appsView head

  go [] _ _ = error "empty spine (*very* malformed type?)"
  go [x] i (Trie m) = Trie (Map.alter (`join` i) x m)
      where join (Just (One x)) i = Just (Some [i, x])
            join (Just (Some xs)) i = Just (Some (i:xs))
            join Nothing i = Just (One i)
            join _ _ = Nothing

  go (x:xs) i (Trie l) = Trie (Map.alter (insert' xs i) x l) where
    insert' :: [Type p] -> Implicit i p -> Maybe (Node i p) -> Maybe (Node i p)
    insert' xs i (Just (Many t)) = Just (Many (go xs i t))
    insert' xs i (Just (One t)) = Just (ManyMore [t] (go xs i mempty))
    insert' xs i (Just (Some ts)) = Just (ManyMore ts (go xs i mempty))
    insert' xs i (Just (ManyMore ts t)) = Just (ManyMore ts (go xs i t))
    insert' xs i Nothing = Just (Many (go xs i (Trie Map.empty)))

-- | Find a type in a trie by conservative fuzzy search.
lookup :: forall i p. Var p ~ Var Typed => Type p -> ImplicitScope i p -> [Implicit i p]
lookup ty = go ts  where
  ts = appsView ty
  go :: [Type p] -> ImplicitScope i p -> [Implicit i p]
  go [x] (Trie m) = case find x m of
    Just (One x) -> [x]
    Just (Some xs) -> xs
    Just (ManyMore xs _) -> xs
    _ -> []
  go (x:xs) (Trie m) = case find x m of
    Just (Many m) -> go xs m
    Just (ManyMore ss m) -> ss ++ go xs m
    Just (One x) -> [x]
    Just (Some xs) -> xs
    _ -> []
  go [] Trie{} = error "badly-kinded type (empty spine?)"

  find :: Type p -> Map.Map (Type p) (Node i p) -> Maybe (Node i p)
  find w = fixup . toList . Map.filterWithKey (\k _ -> w `matches` k) where
    fixup []  = Nothing
    fixup [x] = Just x
    fixup (x:xs) = Just (sconcat (x :| xs))

instance Ord (Var p) => Monoid (ImplicitScope i p) where
  mempty = Trie mempty

instance Ord (Var p) => Semigroup (ImplicitScope i p) where
  Trie m <> Trie m' = Trie (merge m m')

instance Ord (Var p) => Semigroup (Node i p) where
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
keys :: forall i p. Ord (Var p) => ImplicitScope i p -> Map.Map (Type p) [Implicit i p]
keys = go where
  go (Trie m) = foldMap goNode m

  goNode (One x) = Map.singleton (x ^. implHead) [x]
  goNode (Some xs) = foldMapOf each (\i -> Map.singleton (i ^. implHead) [i]) xs
  goNode (Many t) = go t
  goNode (ManyMore xs t) = foldMapOf each (\i -> Map.singleton (i ^. implHead) [i]) xs <> go t


-- | Find the 'Many' node located at the end of the provided spine
-- section in the scope.
subTrie :: forall i p. Ord (Var p) => [Type p] -> ImplicitScope i p -> Maybe (ImplicitScope i p)
subTrie = go where
  go (x:xs) (Trie m) = goNode xs (Map.lookup x m)
  go [] t = Just t

  goNode xs (Just (Many t)) = go xs t
  goNode _ _ = Nothing

-- | Make a trie consisting of the only the one given implicit.
singleton :: Ord (Var p) => Ann Resolved -> Sort -> Var p -> Type p -> i -> ImplicitScope i p
singleton a s v t i = insert a s v t i mempty


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
getHead t@TyLit{} = (t, Seq.empty)
getHead t@TyWildcard{} = (t, Seq.empty)
getHead (TyParens t) = getHead t
getHead t@TyOperator{} = (t, Seq.empty)

-- | Split the type of an implicit variable into its head and a set of
-- obligations.
splitImplVarType = getHead

merge :: Ord (Var p) => Map.Map (Type p) (Node i p) -> Map.Map (Type p) (Node i p) -> Map.Map (Type p) (Node i p)
merge = Map.merge Map.preserveMissing Map.preserveMissing (Map.zipWithMatched (const (<>)))

-- | Does there exist a substitution that can make a the same as b?
-- (Conservative check.)
matches :: Var p ~ Var Typed => Type p -> Type p -> Bool
matches (TyParens x) x' = matches x x'
matches x (TyParens x') = matches x x'

matches TyVar{} _ = True
matches _ TyVar{} = True

matches TyWildcard{} _ = True
matches _ TyWildcard{} = True

matches (TyCon t) (TyCon t') = t == t'
matches TyCon{} _ = False

matches (TyLit t) (TyLit t') = t == t'
matches TyLit{} _ = False

matches (TyPromotedCon t) (TyPromotedCon t') = t == t'
matches TyPromotedCon{} _ = False

matches (TyApp f x) (TyApp f' x') = f `matches` f' && x `matches` x'

matches (TyApp f x) (a :-> b) =
  matches f (TyApp (TyCon tyArr_n) a) && matches x b

matches (TyApp f x) (TyTuple a b) =
  matches f (TyApp (TyCon tyProd_n) a) && matches x b

matches TyApp{} _ = False

matches (TyOperator l o r) x' = matches ((TyCon o `TyApp` l) `TyApp` r) x'
matches x (TyOperator l o r) = matches x ((TyCon o `TyApp` l) `TyApp` r)

matches (TyPi b t) (TyPi b' t') = t `matches` t' && b `matchesBinder` b' where
  matchesBinder (Anon t) (Anon t') = t `matches` t'
  matchesBinder (Implicit t) (Implicit t') = t `matches` t'
  matchesBinder (Invisible _ k x) (Invisible _ k' x') = x == x' && fromMaybe False (matches <$> k <*> k')
  matchesBinder _ _ = False

matches (a :-> b) (TyApp f x) =
  matches f (TyApp (TyCon tyArr_n) a) && matches x b

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

matches (TyTuple a b) (TyApp f x) =
  matches f (TyApp (TyCon tyProd_n) a) && matches x b

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

don'tSubstitute :: Ord (Var p) => Node i p -> Set.Set (Var p)
don'tSubstitute (One i) = boundByImpl i
don'tSubstitute _ = mempty

boundByImpl :: Ord (Var p) => Implicit i p -> Set.Set (Var p)
boundByImpl = foldMap fv . view implPre where
  fv (Quantifier v) = Set.singleton (v ^?! tyBinderVar)
  fv _ = mempty

tyArr_n :: Var Typed
tyArr_n = TgName nm id where
  CoVar id (Just nm) _ = vArrow

tyProd_n :: Var Typed
tyProd_n = TgName nm id where
  CoVar id (Just nm) _ = vProduct
