{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, TemplateHaskell,
   DeriveAnyClass, OverloadedStrings #-}

-- | Core uses one variable type 'CoVar' across all types (terms, types,
-- coercions). These are identified by a unique number, which allows for
-- optimised lookups and comparisons.
module Core.Var where

import qualified Data.Text as T
import Data.Hashable

import Control.Lens
import Control.Monad.Namey

import Text.Pretty.Semantic
import GHC.Generics
import Data.Data

-- | The core variable type
data CoVar =
  CoVar { _covarId :: {-# UNPACK #-} !Int -- ^ The unique identifier for this variable.
        , _covarName :: Maybe T.Text -- ^ The name of this variable.
        , _covarInfo :: VarInfo -- ^ Additional information about this variable.
        }
  deriving (Show, Generic, Data)

instance Eq CoVar where
  (CoVar a _ _) == (CoVar b _ _) = a == b

instance Ord CoVar where
  (CoVar a _ _) `compare` (CoVar b _ _) = a `compare` b

instance Hashable CoVar where
  hashWithSalt s (CoVar a _ _) = hashWithSalt s a

-- | 'VarInfo' is used to store additional information about a variable,
-- which is used to determine its kind.
data VarInfo
  = ValueVar
  | DataConVar
  | TypeConVar
  | TypeVar
  | CastVar
  deriving (Eq, Show, Ord, Generic, Data, Hashable)

makeLenses ''CoVar
makePrisms ''VarInfo

covarDisplayName :: CoVar -> T.Text
covarDisplayName (CoVar i Nothing _) = genAlnum i
covarDisplayName (CoVar _ (Just n) _) = n

instance Pretty CoVar where
  pretty v@(CoVar i _ k) = text (covarDisplayName v) <> scomment (string "#" <> pretty k <> shown i)

instance Pretty VarInfo where
  pretty ValueVar = text "v"
  pretty DataConVar = text "D"
  pretty TypeConVar = text "t"
  pretty TypeVar = text "'t"
  pretty CastVar = text "c"

-- | Either a 'CoVar' or some alternative representation of it. This is
-- used to allow functions which may operate on 'CoVar's or annotated
-- alternatives.
class (Hashable a, Eq a, Ord a, Pretty a, Show a) => IsVar a where
  -- | Convert this variable into a 'CoVar'
  toVar :: a -> CoVar
  -- | Build this from a 'CoVar'
  fromVar :: CoVar -> a

  -- | This variable's type
  varInfo :: a -> VarInfo
  varInfo = view covarInfo . toVar

instance IsVar CoVar where
  toVar = id
  fromVar = id

-- | Is this variable usable inside a term or atom
isValueInfo :: VarInfo -> Bool
isValueInfo ValueVar = True
isValueInfo DataConVar = True
isValueInfo _ = False

-- | Is this variable usable within a type
isTypeInfo :: VarInfo -> Bool
isTypeInfo TypeConVar = True
isTypeInfo DataConVar = True
isTypeInfo _ = False
