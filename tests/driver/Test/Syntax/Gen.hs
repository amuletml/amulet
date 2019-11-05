{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
module Test.Syntax.Gen ( genType, genLit ) where

import Syntax.Builtin
import Syntax.Var
import Syntax

import Data.Traversable
import Data.Function
import Data.Text (singleton)
import Data.List

import qualified Data.Text as T

import Control.Monad.Reader

import qualified Hedgehog.Range as Range
import qualified Hedgehog.Gen as Gen
import Hedgehog hiding (Var) -- fuck you

data InScopeSet =
  InScopeSet { freshNames   :: [Var Typed]
             , namesInScope :: [Var Typed]
             }

referToVar :: (MonadGen m, MonadReader InScopeSet m) => m (Var Resolved)
referToVar = do
  scope <- asks namesInScope
  Gen.element scope

gen_type :: (MonadGen m, MonadReader InScopeSet m) => m (Type Typed)
gen_type = do
  scope <- asks namesInScope

  let base_case =
          [ Gen.element [ tyUnit, tyBool, tyInt, tyString ]
          , Gen.element [ TyPromotedCon nILName, TyPromotedCon cONSName ]
          ]
        ++
          case scope of
            [] -> []
            _ -> [TyVar <$> referToVar]

  Gen.recursive Gen.choice
    base_case
    [ TyArr <$> gen_type <*> gen_type
    , TyTuple <$> gen_type <*> gen_type

    , TyApp tyLazy <$> gen_type
    , TyApp tyList <$> gen_type
    , TyApp <$> gen_type <*> gen_type
    , TyLit <$> genLit
    , TyTupleL <$> gen_type <*> gen_type

    , do
        k <- Gen.maybe gen_type
        withVar $ \v -> do
          vis <- Gen.element [ Spec, Req ]
          TyPi (Invisible v k vis) <$> gen_type
    , do
        n <- Gen.int (Range.linear 1 10)
        let alpha = map singleton $ cycle ['a'..'z']

        fmap (TyExactRows . nubBy ((==) `on` fst)) . for [1..n] . const $
          (,) (alpha !! n) <$> genType
    , do
        t <- gen_type
        n <- Gen.int (Range.linear 1 10)
        let alpha = map singleton $ cycle ['a'..'z']

        fmap (TyRows t . nubBy ((==) `on` fst)) . for [1..n] . const $
          (,) (alpha !! n) <$> genType
    ]

genType :: MonadGen m => m (Type Typed)
genType = runReaderT gen_type emptyScope

genLit :: MonadGen m => m Lit
genLit =
  Gen.choice
    [ LiInt . fromIntegral <$> Gen.int (Range.linear minBound maxBound)
    , LiStr <$> Gen.text (Range.linear 1 100) Gen.alphaNum
    , LiBool <$> Gen.bool
    , pure LiUnit
    ]

withVar :: MonadReader InScopeSet m => (Var Typed -> m a) -> m a
withVar k =
  do
    ~(new:_) <- asks freshNames
    local (bump new) (k new)
  where bump n (InScopeSet ~(_:nm) sc) = InScopeSet nm (n:sc)

emptyScope :: InScopeSet
emptyScope = InScopeSet alpha mempty where
  alpha = zipWith toName [1..] ([1..] >>= flip replicateM ['a'..'z'])
  toName id n = TgName (T.pack n) id

-- pah orphans
