{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances, DeriveDataTypeable #-}

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

-- | The resolved phrase is used after we have resolved variables, and
-- during desugaring.
--
-- We do not provide any additional annotations, but do uniquify
-- variables, effectively eliminating the concept of modules.
newtype Resolved = Resolved Resolved deriving Data

-- | The desugared phrase is used after we have resolved variables and
-- desugared terms.
newtype Desugared = Desugared Desugared deriving Data

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
type family Var p :: * where
  Var Parsed = VarParsed
  Var Resolved = VarResolved
  Var Desugared = VarResolved
  Var Typed = VarResolved

-- | Parsed variables are little more than identifiers: they do not have
-- a concept of scope.
data VarParsed
  = Name Text -- ^ An unqualified identifier
  | InModule Text VarParsed -- ^ An identifier in a module
  deriving (Eq, Show, Ord, Data)

instance Semigroup VarParsed where
  (Name t) <> v = InModule t v
  (InModule m n) <> v = InModule m (n <> v)

-- | A resolved variable. These no longer keep track of which module they
-- belong to, and thus simply hold a 'T.Text' value.
data VarResolved
  = TgName Text {-# UNPACK #-} !Int -- ^ A user-defined name
  | TgInternal Text -- ^ A variable provided by the compiler
  deriving (Show, Data)

instance Semigroup VarResolved where
  _ <> x@(TgInternal _) = x
  (TgInternal x) <> (TgName y i) = TgName (T.concat [x, T.pack ".", y]) i
  (TgName x _) <> (TgName y i) = TgName (T.concat [x, T.pack ".", y]) i

instance Eq VarResolved where
  (TgName _ a) == (TgName _ b) = a == b
  (TgInternal a) == (TgInternal b) = a == b
  _ == _ = False

instance Ord VarResolved where
  (TgName _ a) `compare` (TgName _ b) = a `compare` b
  (TgInternal a) `compare` (TgInternal b) = a `compare` b

  (TgName _ _) `compare` (TgInternal _) = GT
  (TgInternal _) `compare` (TgName _ _) = LT

instance Pretty VarParsed where
  pretty (Name v) = text v
  pretty (InModule t v) = text t <> dot <> pretty v

instance Pretty VarResolved where
  pretty (TgName v i) = text v <> scomment (string "#" <> string (show i))
  pretty (TgInternal v) = text v

-- | A literal value
data Lit
  = LiFloat Double
  | LiInt Integer
  | LiStr Text
  | LiBool Bool
  | LiUnit
  deriving (Eq, Show, Ord, Data, Typeable)

instance Pretty Lit where
  pretty (LiStr s) = sstring (dquotes (text s))
  pretty (LiInt s) = sliteral (integer s)
  pretty (LiFloat s) = sliteral (double s)
  pretty (LiBool True) = sliteral (string "true")
  pretty (LiBool False) = sliteral (string "false")
  pretty LiUnit = sliteral (parens empty)

