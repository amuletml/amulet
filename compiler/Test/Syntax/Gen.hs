{-# LANGUAGE ScopedTypeVariables #-}
module Test.Syntax.Gen ( genType ) where

import Types.Infer.Builtin (tyUnit, tyBool, tyInt, tyString, tyLazy)
import Syntax.Var
import Syntax

import Data.Traversable
import Data.Function
import Data.Text (unpack, singleton)
import Data.Char
import Data.List

import qualified Hedgehog.Range as Range
import qualified Hedgehog.Gen as Gen
import Hedgehog hiding (Var) -- fuck you

genVar :: MonadGen m => m (Var Resolved)
genVar = do
  t <- Gen.text (Range.linear 1 25) Gen.lower
  let alphaIndex (c:cs) = alphaIndex cs + (ord c - 97)
      alphaIndex [] = 0
  pure (TgName t (alphaIndex (unpack t)))

genType :: MonadGen m => m (Type Typed)
genType =
  Gen.recursive Gen.choice
    [ Gen.element [ tyUnit, tyBool, tyInt, tyString ]
    ]
    [ TyArr <$> genType <*> genType
    , TyTuple <$> genType <*> genType
    , TyApp tyLazy <$> genType
    , TyApp <$> genType <*> genType
    , do
        v <- genVar
        TyForall v . Just <$> genType <*> genType
    , do
        n <- Gen.int (Range.linear 1 10)
        let alpha = map singleton $ cycle ['a'..'z']

        fmap (TyExactRows . nubBy ((==) `on` fst)) . for [1..n] . const $
          (,) (alpha !! n) <$> genType

    ]
