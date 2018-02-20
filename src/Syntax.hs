{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies, DataKinds #-}
{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
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

  -- Tuple (see note [1])
  | Tuple [Expr p] (Ann p)

  -- Explicit type application
  | TypeApp (Expr p) (Type p) (Ann p)


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

deriving instance (Eq (Var p), Eq (Ann p)) => Eq (Pattern p)
deriving instance (Show (Var p), Show (Ann p)) => Show (Pattern p)
deriving instance (Ord (Var p), Ord (Ann p)) => Ord (Pattern p)
deriving instance (Data p, Typeable p, Data (Var p), Data (Ann p)) => Data (Pattern p)
instance (Data (Var p), Data (Ann p), Data p) => Spanned (Pattern p)

data Lit
  = LiInt Integer
  | LiStr Text
  | LiBool Bool
  | LiUnit
  deriving (Eq, Show, Ord, Data, Typeable)

data Type p
  = TyCon (Var p)
  | TyVar (Var p)
  | TyForall [Var p] (Type p)
  | TyArr (Type p) (Type p)
  | TyApp (Type p) (Type p)
  | TyRows (Type p) [(Text, Type p)]  -- { α | foo : int, bar : string }
  | TyExactRows [(Text, Type p)] -- { foo : int, bar : string }
  | TyTuple (Type p) (Type p) -- (see note [1])

  | TySkol (Skolem p)

data Skolem p
  = Skolem (Var p) -- the constant itself
           (Var p) -- what variable this skolemises
           (Type p) -- the type this was generated for

deriving instance (Show (Var p), Show (Ann p)) => Show (Skolem p)
deriving instance (Data p, Typeable p, Data (Var p), Data (Ann p)) => Data (Skolem p)

instance Eq (Var p) => Eq (Skolem p) where
  Skolem v _ _  == Skolem v' _ _ = v == v'

instance Ord (Var p) => Ord (Skolem p) where
  Skolem v _ _ `compare` Skolem v' _ _ = v `compare` v'

deriving instance (Eq (Var p), Eq (Ann p)) => Eq (Type p)
deriving instance (Show (Var p), Show (Ann p)) => Show (Type p)
deriving instance (Ord (Var p), Ord (Ann p)) => Ord (Type p)
deriving instance (Data p, Typeable p, Data (Var p), Data (Ann p)) => Data (Type p)

data Kind p
  = KiStar
  | KiArr (Kind p) (Kind p)
  | KiVar (Var p)
  | KiForall [Var p] (Kind p)

deriving instance (Eq (Var p), Eq (Ann p)) => Eq (Kind p)
deriving instance (Show (Var p), Show (Ann p)) => Show (Kind p)
deriving instance (Ord (Var p), Ord (Ann p)) => Ord (Kind p)
deriving instance (Data p, Typeable p, Data (Var p), Data (Ann p)) => Data (Kind p)

data Toplevel p
  = LetStmt [(Var p, Expr p, Ann p)]
  | ForeignVal (Var p) Text (Type p) (Ann p)
  | TypeDecl (Var p) [Var p] [Constructor p]
  | Module (Var p) [Toplevel p]
  | Open { openName :: Var p
         , openAs :: Maybe (Var p) }

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
  | ArgCon (Var p) (Type p) (Ann p)

deriving instance (Eq (Var p), Eq (Ann p)) => Eq (Constructor p)
deriving instance (Show (Var p), Show (Ann p)) => Show (Constructor p)
deriving instance (Ord (Var p), Ord (Ann p)) => Ord (Constructor p)
deriving instance (Data p, Typeable p, Data (Var p), Data (Ann p)) => Data (Constructor p)
instance (Data (Var p), Data (Ann p), Data p) => Spanned (Constructor p)


-- }}}

unTvName :: Var Typed -> Var Resolved
unTvName (TvName x) = x

getType :: Data (f Typed) => f Typed -> Type Typed
getType = snd . head . catMaybes . gmapQ get where
  get d = fmap (`asTypeOf` (undefined :: (Span, Type Typed))) (cast d)
  -- FIXME: Point-freeing this definition makes type inference broken.
  -- Thanks, GHC.

makePrisms ''Expr
makePrisms ''Type


{- Note [1]: Tuple types vs tuple patterns/values

    Tuple types only describe *pairs*, but patterns/values can have any
    number of elements. We do this like Idris, in which (a, b, c) = (a,
    (b, c)) -}

--- vim: fdm=marker
