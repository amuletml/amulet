{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}

{- | The various variables that "Syntax" uses.

   We make liberal use of trees that grow within the Amulet compiler,
   meaning we define a series of \"phases\" ('Parsed', 'Resolved',
   etc...) and use type families to change how we annotate syntax as
   progress through the compiler progresses.
-}
module Syntax.Var where

import qualified Data.Text as T
import Data.Text (Text)
import Data.Data

import Text.Pretty.Semantic

-- | The parsed phrase is used on the result of the parser, before
-- resolution occurs.
--
-- Variables are only aware of their name and we only annotate values
-- with their position in the original code.
newtype Parsed = Parsed Parsed deriving Data

-- | The parsed phrase is used after we have resolved variables, and
-- during desugaring.
--
-- We do not provide any additional annotations, but do uniquify
-- variables, effectively eliminating the concept of modules.
newtype Resolved = Resolved Resolved deriving Data

-- | This is used after (and during) type checking, before we need to
-- lower to core.
--
-- We do not provide any additional information to variables. However,
-- all terms are now annotated with their type, which can be extracted if
-- needed.
newtype Typed = Typed Typed deriving Data

-- | An alias for resolved variables, this is considered the "default"
-- name, though should not be confused with the parsed variable
-- constructor.
type Name = Var Resolved

-- | A variable at a given phase of compilation.
data family Var a

-- | Parsed variables are little more than identifiers: they do not have
-- a concept of scope.
data instance Var Parsed
  = Name Text -- ^ An unqualified identifier
  | InModule Text (Var Parsed) -- ^ An identifier in a module
  deriving (Eq, Show, Ord, Data)

instance Semigroup (Var Parsed) where
  (Name t) <> v = InModule t v
  (InModule m n) <> v = InModule m (n <> v)

-- | A resolved variable. These no longer keep track of which module they
-- belong to, and thus simply hold a 'T.Text' value.
data instance Var Resolved
  = TgName Text {-# UNPACK #-} !Int -- ^ A user-defined name
  | TgInternal Text -- ^ A variable provided by the compiler
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

-- | A typed variable.
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
