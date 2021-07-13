{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Core.Intrinsic
  ( Intrinsic(..)
  , intrinsicOf
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Hashable
import Data.Maybe
import Data.Tuple

import GHC.Generics

data Intrinsic
  = IntAdd
  | IntSub
  | IntMul
  | IntDiv
  | IntPow
  | IntMod
  | IntEq
  | IntLt
  | IntLe

  | FloatAdd
  | FloatSub
  | FloatMul
  | FloatDiv
  | FloatPow
  | FloatEq
  | FloatLt
  | FloatLe

  | StrConcat
  | StrEq
  | StrLt
  | StrLe

  | BoolEq
  deriving (Eq, Ord, Generic, Hashable)

names :: [(String, Intrinsic)]
names =
  [ ( "int.add", IntAdd )
  , ( "int.sub", IntSub )
  , ( "int.mul", IntMul )
  , ( "int.div", IntDiv )
  , ( "int.pow", IntPow )
  , ( "int.mod", IntMod )
  , ( "int.eq", IntEq )
  , ( "int.lt", IntLt )
  , ( "int.le", IntLe )

  , ( "float.add", FloatAdd )
  , ( "float.sub", FloatSub )
  , ( "float.mul", FloatMul )
  , ( "float.div", FloatDiv )
  , ( "float.pow", FloatPow )
  , ( "float.eq", FloatEq )
  , ( "float.lt", FloatLt )
  , ( "float.le", FloatLe )

  , ( "string.concat", StrConcat )
  , ( "string.eq", StrEq )
  , ( "string.lt", StrLt )
  , ( "string.le", StrLe )

  , ( "bool.eq", BoolEq )
  ]

instance Show Intrinsic where
  show = fromJust . flip Map.lookup (Map.fromList . map swap $ names)

intrinsicOf :: T.Text -> Maybe Intrinsic
intrinsicOf = go where
  go t
    | Just ('%', x) <- T.uncons t = Map.lookup (T.unpack x) lookup
    | otherwise = Nothing

  lookup = Map.fromList names
