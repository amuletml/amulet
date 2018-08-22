{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, TemplateHaskell,
   DeriveAnyClass #-}

-- | Core uses one variable type 'CoVar' across all types (terms, types,
-- coercions). These are identified by a unique number, which allows for
-- optimised lookups and comparisons.
module Core.Var where

import qualified Data.Text as T
import Data.Hashable

import Control.Lens
import GHC.Generics
import Text.Pretty.Semantic
import Data.Data

-- | The core variable type
data CoVar =
  CoVar { _covarId :: {-# UNPACK #-} !Int -- ^ The unique identifier for this variable.
        , _covarName :: T.Text -- ^ The name of this variable.
        , _covarInfo :: VarInfo -- ^ Additional information about this variable.
        }
  deriving (Show, Generic, Data, Hashable)

instance Eq CoVar where
  (CoVar a _ _) == (CoVar b _ _) = a == b

instance Ord CoVar where
  (CoVar a _ _) `compare` (CoVar b _ _) = a `compare` b

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

instance Pretty CoVar where
  pretty (CoVar i v _) = text v <> scomment (string "#" <> string (show i))

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
