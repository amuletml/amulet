{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies, DataKinds #-}
{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
module Syntax where

import qualified Data.Text as T
import Data.Text (Text)
import Data.Spanned
import Data.Span

import Data.List.NonEmpty(NonEmpty ((:|)))
import Data.Semigroup (sconcat, Semigroup(..))
import Data.Typeable
import Data.Triple
import Data.Maybe
import Data.Data

import Control.Lens

newtype Parsed = Parsed Parsed deriving Data
newtype Resolved = Resolved Resolved deriving Data
newtype Typed = Typed Typed deriving Data

data family Var a

data instance Var Parsed
  = Name Text
  | InModule Text (Var Parsed)
  deriving (Eq, Show, Ord, Data)

instance Semigroup (Var Parsed) where
  (Name t) <> v = InModule t v
  (InModule m n) <> v = InModule m (n <> v)

data instance Var Resolved
  = TgName Text {-# UNPACK #-} !Int
  | TgInternal Text
  deriving (Show, Data)

instance Semigroup (Var Resolved) where
  _ <> x@(TgInternal _) = x
  (TgInternal _) <> _ = error "Nonsensical module"
  (TgName x _) <> (TgName y i) = TgName (T.concat [x, T.pack ".", y]) i

instance Eq (Var Resolved) where
  (TgName _ a) == (TgName _ b) = a == b
  (TgInternal a) == (TgInternal b) = a == b
  _ == _ = False

instance Ord (Var Resolved) where
  (TgName _ a) `compare` (TgName _ b) = a `compare` b
  (TgInternal a) `compare` (TgInternal b) = a `compare` b

  (TgName _ _) `compare` (TgInternal _) = GT
  (TgInternal _) `compare` (TgName _ _) = LT

data instance Var Typed
  = TvName (Var Resolved)
  deriving (Show, Data, Eq, Ord)

instance Semigroup (Var Typed) where
  (TvName x) <> (TvName y) = TvName (x <> y)

type family Ann a :: * where
  Ann Parsed = Span
  Ann Resolved = Span
  Ann Typed = (Span, Type Typed)

data Expr p
  = VarRef (Var p) (Ann p)
  | Let [(Var p, Expr p, Ann p)] (Expr p) (Ann p)
  | If (Expr p) (Expr p) (Expr p) (Ann p)
  | App (Expr p) (Expr p) (Ann p)
  | Fun (Pattern p) (Expr p) (Ann p)
  | Begin [Expr p] (Ann p)
  | Literal Lit (Ann p)
  | Match (Expr p) [(Pattern p, Expr p)] (Ann p)
  | Function [(Pattern p, Expr p)] (Ann p)
  | BinOp (Expr p) (Expr p) (Expr p) (Ann p)
  | Hole (Var p) (Ann p)
  | Ascription (Expr p) (Type p) (Ann p)

  -- Records
  | Record [(Text, Expr p)] (Ann p) -- { foo = bar, baz = quux }
  | RecordExt (Expr p) [(Text, Expr p)] (Ann p) -- { foo with baz = quux }
  | Access (Expr p) Text (Ann p) -- foo.bar

  -- Sections
  | LeftSection (Expr p) (Expr p) (Ann p) -- (+ foo)
  | RightSection (Expr p) (Expr p) (Ann p) -- (foo +)
  | BothSection (Expr p) (Ann p) -- (+)
  | AccessSection Text (Ann p)
  | Parens (Expr p) (Ann p) -- (xyz), just useful for resetting precedence

  -- Tuple (see note [1])
  | Tuple [Expr p] (Ann p)
  | TupleSection [Maybe (Expr p)] (Ann p)

  -- Explicit type application
  | TypeApp (Expr p) (Type p) (Ann p)
  | Cast (Expr p) (Coercion p) (Ann p)

deriving instance (Eq (Var p), Eq (Ann p)) => Eq (Expr p)
deriving instance (Show (Var p), Show (Ann p)) => Show (Expr p)
deriving instance (Ord (Var p), Ord (Ann p)) => Ord (Expr p)
deriving instance (Data p, Typeable p, Data (Var p), Data (Ann p)) => Data (Expr p)
instance (Data (Var p), Data (Ann p), Data p) => Spanned (Expr p)

data Pattern p
  = Wildcard (Ann p)
  | Capture (Var p) (Ann p)
  | Destructure (Var p) (Maybe (Pattern p)) (Ann p)
  | PType (Pattern p) (Type p) (Ann p)
  | PRecord [(Text, Pattern p)] (Ann p)
  | PTuple [Pattern p] (Ann p)
  | PLiteral Lit (Ann p)

deriving instance (Eq (Var p), Eq (Ann p)) => Eq (Pattern p)
deriving instance (Show (Var p), Show (Ann p)) => Show (Pattern p)
deriving instance (Ord (Var p), Ord (Ann p)) => Ord (Pattern p)
deriving instance (Data p, Typeable p, Data (Var p), Data (Ann p)) => Data (Pattern p)
instance (Data (Var p), Data (Ann p), Data p) => Spanned (Pattern p)

data Lit
  = LiFloat Double
  | LiInt Integer
  | LiStr Text
  | LiBool Bool
  | LiUnit
  deriving (Eq, Show, Ord, Data, Typeable)

data Type p
  = TyCon (Var p)
  | TyVar (Var p)
  | TyPromotedCon (Var p)
  | TyApp (Type p) (Type p)
  | TyPi (TyBinder p) (Type p) -- arrow, pi or forall
  | TyRows (Type p) [(Text, Type p)]  -- { α | foo : int, bar : string }
  | TyExactRows [(Text, Type p)] -- { foo : int, bar : string }
  | TyTuple (Type p) (Type p) -- (see note [1])

  -- Used internally:
  | TySkol (Skolem p)
  | TyWithConstraints [(Type p, Type p)] (Type p)

  -- Dependent type stuff
  | TyType -- yeah, type : type, fight me

data TyBinder p
  = Anon { _tyBinderType :: Type p } -- a function type
  | Implicit { _tyBinderVar :: Var p, _tyBinderArg :: Maybe (Type p) } -- a forall type

instance Spanned (Type p) where
  annotation _ = internal

deriving instance (Data p, Typeable p, Data (Var p)) => Data (TyBinder p)
deriving instance Show (Var p) => Show (TyBinder p)
deriving instance Ord (Var p) => Ord (TyBinder p)
deriving instance Eq (Var p) => Eq (TyBinder p)


data Skolem p
  = Skolem { _skolIdent :: Var p -- the constant itself
           , _skolVar :: Var p -- what variable this skolemises
           , _skolScope :: Type p -- the type this was generated for
           , _skolMotive :: SkolemMotive p
           }

deriving instance Show (Var p) => Show (Skolem p)
deriving instance (Data p, Typeable p, Data (Var p)) => Data (Skolem p)

data SkolemMotive p
  = ByAscription (Type p)
  | BySubsumption (Type p) (Type p)
  | ByExistential (Var p) (Type p)

deriving instance Show (Var p) => Show (SkolemMotive p)
deriving instance (Data p, Typeable p, Data (Var p)) => Data (SkolemMotive p)

instance Eq (Var p) => Eq (Skolem p) where
  Skolem v _ _ _ == Skolem v' _ _ _ = v == v'

instance Ord (Var p) => Ord (Skolem p) where
  Skolem v _ _ _ `compare` Skolem v' _ _ _ = v `compare` v'

deriving instance Eq (Var p) => Eq (Type p)
deriving instance Show (Var p) => Show (Type p)
deriving instance Ord (Var p) => Ord (Type p)
deriving instance (Data p, Typeable p, Data (Var p)) => Data (Type p)

data Coercion p
  = VarCo (Var p)
  | ReflCo (Type p) -- <T> : T ~ T
  | SymCo (Coercion p) -- sym (X : T ~ S) : S ~ T
  | AppCo (Coercion p) (Coercion p) -- (f : B ~ D) (x : A ~ C) : B A ~ D C
  | ArrCo (Coercion p) (Coercion p) -- (x : S ~ T) -> (y : S' ~ T') : (S -> S') ~ (T -> T')
  | ProdCo (Coercion p) (Coercion p) -- (x : S ~ T, y : S' ~ T') : (S, S') ~ (T, T')
  | ExactRowsCo [(Text, Coercion p)] -- { x : A ~ B } : { x : A } ~ { x : B }
  | RowsCo (Coercion p) [(Text, Coercion p)] -- { x : A ~ B | f : S ~ T } : { A | f : S } ~ { B | f : T }
  | AssumedCo (Type p) (Type p) -- <A, B> : A ~ B
  | ForallCo (Var p) (Coercion p) -- (forall v. phi : a ~ b) : forall v. a ~ forall v. b

deriving instance Eq (Var p) => Eq (Coercion p)
deriving instance Show (Var p) => Show (Coercion p)
deriving instance Ord (Var p) => Ord (Coercion p)
deriving instance (Data p, Typeable p, Data (Var p)) => Data (Coercion p)

data Toplevel p
  = LetStmt [(Var p, Expr p, Ann p)]
  | ForeignVal (Var p) Text (Type p) (Ann p)
  | TypeDecl (Var p) [Var p] [Constructor p]
  | Module (Var p) [Toplevel p]
  | Open { openName :: Var p
         , openAs :: Maybe T.Text }

instance (Spanned (Constructor p), Ann p ~ Span) => Spanned (Toplevel p) where
  annotation (LetStmt ((_, _, x):vs)) = sconcat (x :| map thd3 vs)
  annotation (TypeDecl _ _ (x:xs)) = sconcat (annotation x :| map annotation xs)
  annotation (ForeignVal _ _ _ x) = x
  annotation _ = internal

deriving instance (Eq (Var p), Eq (Ann p)) => Eq (Toplevel p)
deriving instance (Show (Var p), Show (Ann p)) => Show (Toplevel p)
deriving instance (Ord (Var p), Ord (Ann p)) => Ord (Toplevel p)
deriving instance (Data p, Typeable p, Data (Var p), Data (Ann p)) => Data (Toplevel p)

data Constructor p
  = UnitCon (Var p) (Ann p)
  -- In ArgCon, the Type p is the type of the (sole) argument
  | ArgCon (Var p) (Type p) (Ann p)
  -- In GeneralisedCon, the Type p is the type of the overall thing
  | GeneralisedCon (Var p) (Type p) (Ann p)

deriving instance (Eq (Var p), Eq (Ann p)) => Eq (Constructor p)
deriving instance (Show (Var p), Show (Ann p)) => Show (Constructor p)
deriving instance (Ord (Var p), Ord (Ann p)) => Ord (Constructor p)
deriving instance (Data p, Typeable p, Data (Var p), Data (Ann p)) => Data (Constructor p)
instance (Data (Var p), Data (Ann p), Data p) => Spanned (Constructor p)

pattern TyArr :: Type p -> Type p -> Type p
pattern TyArr t t' <- TyPi (Anon t) t' where
  TyArr t ty = TyPi (Anon t) ty

pattern TyForall :: Var p -> Maybe (Type p) -> Type p -> Type p
pattern TyForall v k t' <- TyPi (Implicit v k) t' where
  TyForall v k ty = TyPi (Implicit v k) ty


unTvName :: Var Typed -> Var Resolved
unTvName (TvName x) = x

getType :: Data (f Typed) => f Typed -> Type Typed
getType = snd . head . catMaybes . gmapQ get where
  get d = fmap (`asTypeOf` (undefined :: (Span, Type Typed))) (cast d)
  -- FIXME: Point-freeing this definition makes type inference broken.
  -- Thanks, GHC.

makePrisms ''Expr
makePrisms ''Type
makePrisms ''Pattern
makePrisms ''Toplevel
makePrisms ''Constructor
makePrisms ''Lit
makeLenses ''Skolem
makeLenses ''TyBinder

_TyArr :: Prism' (Type p) (Type p, Type p)
_TyArr = prism (uncurry (TyPi . Anon)) go where
  go (TyArr a b) = Right (a, b)
  go x = Left x


{- Note [1]: Tuple types vs tuple patterns/values

    Tuple types only describe *pairs*, but patterns/values can have any
    number of elements. We do this like Idris, in which (a, b, c) = (a,
    (b, c)) -}

--- vim: fdm=marker
