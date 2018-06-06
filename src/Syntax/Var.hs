{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Syntax.Var where

import qualified Data.Text as T
import Data.Text (Text)
import Data.Data

import Text.Pretty.Semantic

newtype Parsed = Parsed Parsed deriving Data
newtype Resolved = Resolved Resolved deriving Data
newtype Typed = Typed Typed deriving Data

type Name = Var Resolved

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

newtype instance Var Typed
  = TvName (Var Resolved)
  deriving (Show, Data, Eq, Ord)

instance Semigroup (Var Typed) where
  (TvName x) <> (TvName y) = TvName (x <> y)

instance Pretty (Var Parsed) where
  pretty (Name v) = text v
  pretty (InModule t v) = text t <> dot <> pretty v

instance Pretty (Var Resolved) where
  pretty (TgName v i) = text v <> scomment (string "#" <> string (show i))
  pretty (TgInternal v) = text v

instance Pretty (Var Typed) where
  pretty (TvName v) = pretty v
