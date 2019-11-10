{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances
  , StandaloneDeriving, GeneralizedNewtypeDeriving, DerivingStrategies
  , TypeFamilies, TemplateHaskell, FunctionalDependencies, RankNTypes #-}
module Syntax.Types
  ( Telescope, one, foldTele, foldTeleM, teleFromList, mapTele, traverseTele, teleToList
  , Scope(..), namesInScope, inScope, scopeToList, mapScope
  , Env, freeInEnv, difference, envOf, scopeFromList, toMap
  , names, typeVars, constructors, types, letBound, classes, modules
  , classDecs, tySyms, declaredHere
  , ClassInfo(..), ciName, ciMethods, ciContext, ciConstructorName, ciAssocTs
  , TySymInfo(..), tsName, tsArgs, tsExpansion, tsKind, TySyms, tsEquations, tsConstraint
  , ciConstructorTy, ciHead, ciClassSpan, ciDefaults, ciMinimal, ciFundep, ciDerive
  , Origin(..)

  , focus
  , DerivingStrat(..)
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)

import Control.Monad.Reader.Class
import Control.Monad.Namey

import Control.Lens

import Syntax.Implicits
import Syntax.Toplevel
import Syntax.Boolean
import Syntax.Subst
import Syntax.Expr
import Syntax.Type
import Syntax.Var

-- A bag of bindings, returned from pattern match checking
newtype Telescope p =
  Telescope { getTele :: Map.Map (Var Resolved) (Type p) }
  deriving newtype (Semigroup, Monoid)

deriving instance (Show (Ann p), Show (Var p)) => Show (Telescope p)
deriving instance Ord (Var p) => Ord (Telescope p)
deriving instance Ord (Var p) => Eq (Telescope p)

type instance Index (Telescope p) = Var Resolved
type instance IxValue (Telescope p) = Type p
instance Ixed (Telescope p) where
  ix k f (Telescope m) = Telescope <$> ix k f m

instance At (Telescope p) where
  at k f (Telescope m) = Telescope <$> at k f m

newtype Scope p f =
  Scope { getScope :: Map.Map (Var p) f }

deriving instance (Show (Var p), Show f) => Show (Scope p f)
deriving instance (Ord (Var p), Ord f) => Ord (Scope p f)
deriving instance (Ord (Var p), Ord f) => Eq (Scope p f)
deriving instance Ord (Var p) => Semigroup (Scope p f)
deriving instance Ord (Var p) => Monoid (Scope p f)

instance (Ord (Var p), Ord (Ann p)) => Traversable (Scope p) where
  traverse f (Scope m) = Scope <$> traverse f m

instance Ord (Var p) => Foldable (Scope p) where
  foldMap f (Scope m) = foldMap f m

instance (Ord (Var p), Ord (Ann p)) => Functor (Scope p) where
  fmap f (Scope m) = Scope (fmap f m)

type instance Index (Scope p f) = Var p
type instance IxValue (Scope p f) = f
instance Ord (Var p) => Ixed (Scope p f) where
  ix k f (Scope m) = Scope <$> ix k f m

instance Ord (Var p) => At (Scope p f) where
  at k f (Scope m) = Scope <$> at k f m

data Env
  = Env { _names        :: Scope Resolved (Type Typed)
        , _classes      :: ImplicitScope ClassInfo Typed
        , _typeVars     :: Set.Set (Var Typed)
        , _constructors :: Set.Set (Var Typed)
        , _letBound     :: Set.Set (Var Typed)
        , _declaredHere :: Set.Set (Var Typed) -- Only types
        , _types        :: Map.Map (Var Typed) (Set.Set (Var Typed))
        , _modules      :: Map.Map (Var Typed) (ImplicitScope ClassInfo Typed, TySyms)
        , _classDecs    :: Map.Map (Var Typed) ClassInfo
        , _tySyms       :: TySyms
        }
  deriving (Eq, Show, Ord)

type TySyms = Map.Map (Var Typed) TySymInfo


data TySymInfo
  = TySymInfo
      { _tsName      :: Var Typed
        -- ^ The name of the type synonym
      , _tsExpansion :: Type Typed
        -- ^ The expansion of the type synonym, with free variables
      , _tsArgs      :: [Var Typed]
        -- ^ The arguments to the type synonym in order
      , _tsKind      :: Type Typed
        -- ^ The kind of the type synoynm
      }
  | TyFamInfo
      { _tsName       :: Var Typed
        -- ^ The name of this type family
      , _tsEquations  :: [([Type Typed], Type Typed, Var Typed)]
        -- ^ The equations to this type family
      , _tsArgs       :: [Var Typed]
        -- ^ The arguments to this type family, in order
      , _tsKind       :: Type Typed
        -- ^ The return kind of this type family
      , _tsConstraint :: Maybe (Type Typed)
      }
  deriving (Eq, Show, Ord)


data ClassInfo
  = ClassInfo
      { _ciName :: Var Typed
        -- ^ The name of this class
      , _ciHead :: Type Typed
        -- ^ The head of this class (name applied to parameters)
      , _ciMethods :: Map.Map (Var Typed) (Type Typed)
        -- ^ A map of methods to their signatures
      , _ciAssocTs :: Map.Map (Var Typed) (Int, Type Typed, Type Typed)
        -- ^ A map of associated types to their arities + (declared, actual) kinds
      , _ciContext :: Map.Map Text (Type Typed)
        -- ^ The superclasses of this class,
      , _ciConstructorName :: Var Typed
        -- ^ The name of the constructor for this class
      , _ciConstructorTy :: Type Typed
        -- ^ The type of the constructor for this class
      , _ciClassSpan :: Ann Desugared
        -- ^ The annotation of the class
      , _ciDefaults :: Map.Map Text (Expr Desugared)
        -- ^ Default methods
      , _ciMinimal :: Formula Text
        -- ^ Minimal definition
      , _ciFundep :: [([Int], [Int], Ann Resolved)]
        -- ^ Functional dependencies
      , _ciDerive :: Maybe DerivingStrat
        -- ^ Deriving strategy for this class
      }
  | MagicInfo
      { _ciFundep :: [([Int], [Int], Ann Resolved)]
      , _ciDerive :: Maybe DerivingStrat
      }
  deriving (Eq, Show, Ord)

newtype DerivingStrat =
  DerivingStrat { runDerive :: forall m. (MonadReader Env m, MonadNamey m)
                            => Type Typed
                            -> Ann Resolved
                            -> m (Maybe (Toplevel Desugared)) }

instance Eq DerivingStrat where
  _ == _ = False

instance Show DerivingStrat where
  show _ = "<deriving handler>"

instance Ord DerivingStrat where
  _ `compare` _ = GT

makeLenses ''Env
makeLenses ''ClassInfo
makeLenses ''TySymInfo

(\\) :: Ord (Var p) => Scope p f -> Scope p f -> Scope p f
Scope x \\ Scope y = Scope (x Map.\\ y)

instance Monoid Env where
  mappend = (<>)
  mempty = Env mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty

instance Semigroup Env where
  Env s i c t d l m n o p <> Env s' i' c' t' d' l' m' n' o' p' =
    Env (s <> s') (i <> i') (c <> c') (t <> t') (d <> d') (l <> l') (m <> m') (n <> n') (o <> o') (p <> p')

difference :: Env -> Env -> Env
difference (Env a b c d e f g h i j) (Env a' _ c' d' e' f' g' h' i' j') =
  Env (a \\ a') b (c Set.\\ c') (d Set.\\ d') (e Set.\\ e') (f Set.\\ f') (g Map.\\ g') (h Map.\\ h') (i Map.\\ i') (j Map.\\ j')

freeInEnv :: Env -> Set.Set (Var Typed)
freeInEnv = foldMap ftv . view names

envOf :: Scope Resolved (Type Typed) -> Env
envOf a = Env a mempty mempty mempty mempty mempty mempty mempty mempty mempty

scopeFromList :: Ord (Var p) => [(Var p, f)] -> Scope p f
scopeFromList = Scope . Map.fromList

namesInScope :: Scope p f -> [Var p]
namesInScope (Scope m) = Map.keys m

scopeToList :: Scope p f -> [(Var p, f)]
scopeToList (Scope m) = Map.toList m

inScope :: Ord (Var p) => Var p -> Scope p f -> Bool
inScope v (Scope m) = v `Map.member` m

mapScope :: (Var p -> Var p) -> (f -> f) -> Scope p f -> Scope p f
mapScope k k' (Scope m) = Scope (k' <$> Map.mapKeysMonotonic k m)

focus :: Telescope t -> Scope Resolved (Type t) -> Scope Resolved (Type t)
focus m s = Scope (getScope s <> getTele m)

one :: Var Resolved -> Type p -> Telescope p
one k t = Telescope (Map.singleton k t)

foldTeleM :: (Monad m, Monoid x) => (Var Resolved -> Type p -> m x) -> Telescope p -> m x
foldTeleM f = Map.foldrWithKey (\key t rest -> mappend <$> f key t <*> rest) (pure mempty) . getTele

foldTele :: Monoid m => (Type p -> m) -> Telescope p -> m
foldTele f x = foldMap f (getTele x)

mapTele :: (Type p -> Type p) -> Telescope p -> Telescope p
mapTele f (Telescope x) = Telescope (fmap f x)

traverseTele :: Applicative f
             => (Var Resolved -> Type p -> f (Type p))
             -> Telescope p -> f (Telescope p)
traverseTele f (Telescope x) = Telescope <$> Map.traverseWithKey f x

teleFromList :: [(Var Resolved, Type p)] -> Telescope p
teleFromList = Telescope . Map.fromList

teleToList :: Telescope p -> [(Var Resolved, Type p)]
teleToList = Map.toList . getTele

toMap :: Scope p f -> Map.Map (Var p) f
toMap = getScope

data Origin
  = Supplied -- the programmer supplied this type
  | Guessed -- the compiler invented this type
  | Deduced -- the programmer almost supplied this type
  deriving (Show, Eq)

instance Substitutable Typed Env where
  ftv = view typeVars

  apply sub env = env & names %~ go
                      & classes %~ apply sub
    where go (Scope m) = Scope (fmap (apply sub) m)

