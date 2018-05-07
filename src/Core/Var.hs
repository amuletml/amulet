{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, TemplateHaskell, OverloadedStrings #-}

module Core.Var where

import qualified Data.Text as T
import Control.Lens
import GHC.Generics
import Pretty
import Data.Data

data CoVar =
  CoVar { _covarId :: {-# UNPACK #-} !Int
        , _covarName :: T.Text
        , _covarInfo :: VarInfo
        }
  deriving (Show, Generic, Data)

instance Eq CoVar where
  (CoVar a _ _) == (CoVar b _ _) = a == b

instance Ord CoVar where
  (CoVar a _ _) `compare` (CoVar b _ _) = a `compare` b

data VarInfo
  = ValueVar
  | DataConVar
  | TypeConVar
  | TypeVar
  | CastVar
  | JoinVar {-# UNPACK #-} !Int
  deriving (Eq, Show, Ord, Generic, Data)

makeLenses ''CoVar
makePrisms ''VarInfo


instance Pretty CoVar where
  pretty (CoVar i v k) = text v <> scomment (string "#" <> pretty k <> string (show i))

instance Pretty VarInfo where
  pretty ValueVar = text "v"
  pretty DataConVar = text "D"
  pretty TypeConVar = text "t"
  pretty TypeVar = text "'t"
  pretty CastVar = text "c"
  pretty (JoinVar i) = text "j" <> brackets (string (show i))

class (Eq a, Ord a, Pretty a, Show a) => IsVar a where
  toVar :: a -> CoVar
  fromVar :: CoVar -> a

  varInfo :: a -> VarInfo
  varInfo = view covarInfo . toVar

instance IsVar CoVar where
  toVar = id
  fromVar = id

isValueInfo, isJoinInfo, isTypeInfo :: VarInfo -> Bool

isValueInfo ValueVar = True
isValueInfo DataConVar = True
isValueInfo _ = False

isJoinInfo (JoinVar _) = True
isJoinInfo _ = False

isTypeInfo TypeConVar = True
isTypeInfo DataConVar = True
isTypeInfo _ = False
