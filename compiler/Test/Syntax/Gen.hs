{-# LANGUAGE PackageImports, ScopedTypeVariables #-}
module Test.Syntax.Gen ( genType, genCorrectExpr, genBadExpr ) where

import "amuletml" Control.Monad.Infer (Env, values, types)
import "amuletml" Types.Infer.Builtin (tyUnit, tyBool, tyInt, tyString)

import "amuletml" Syntax.Raise
import "amuletml" Syntax

import qualified Data.Map.Strict as Map
import Data.Semigroup
import Data.Maybe
import Data.Span

import Control.Monad.Reader
import Control.Applicative

import qualified Hedgehog.Range as Range
import qualified Hedgehog.Gen as Gen
import Hedgehog hiding (Var) -- fuck you

import Pretty (pretty)

genVar :: MonadGen m => m (Var Resolved)
genVar = do
  t <- Gen.text (Range.linear 1 25) Gen.lower
  c <- Gen.int (Range.linear 0 (10^6))
  pure (TgName t c)

genType :: MonadGen m => m (Type Typed)
genType =
  Gen.recursive Gen.choice
   [ Gen.element [ tyUnit, tyBool, tyInt, tyString ]
   ]
   [ TyArr <$> genType <*> genType
   , TyTuple <$> genType <*> genType
   ]

genCorrectExpr :: forall m. (MonadGen m, Alternative m) => Type Typed -> m (Expr Resolved) -- yes, resolved
genCorrectExpr = flip runReaderT mempty . go where

  go, expr, ref, app :: Type Typed -> ReaderT (Map.Map (Type Typed) [Expr Resolved]) m (Expr Resolved)
  go want = Gen.small $ Gen.recursive Gen.choice
    [ expr want ]
    [ app want <|> ref want
    , app want
    , Begin <$> Gen.list (Range.linear 1 5) (go want) <*> pure internal
    , do t <- genType
         Match <$> go t <*> genMatches t want <*> pure internal
    ]

  expr x
    | x == tyInt = Literal . LiInt <$> Gen.integral (Range.linear 0 (10^6)) <*> pure internal
    | x == tyString = Literal . LiStr <$> Gen.text (Range.linear 1 100) Gen.alpha <*> pure internal
    | x == tyUnit = pure (Literal LiUnit internal)
    | x == tyBool = Literal . LiBool <$> Gen.bool <*> pure internal

  expr (TyArr d c) = do
    var <- genVar
    Fun (Capture var internal) <$> local (insert var d) (go c) <*> pure internal

  expr (TyTuple a b) = do
    a' <- go a
    b' <- go b
    pure (Tuple [a', b'] internal)

  expr (TyVar t) = ref (TyVar t)

  insert :: Var Resolved -> Type Typed -> Map.Map (Type Typed) [Expr Resolved] -> Map.Map (Type Typed) [Expr Resolved]
  insert v t = Map.insertWith (<>) t [VarRef v internal] . fmap (filter (/= VarRef v internal))

  ref want = do
    paths <- ask
    case fromMaybe [] (Map.lookup want paths) of
      [] -> error ("not in scope: a value of type " ++ show (pretty want))
      es -> Gen.element es

  app want = do
    tg <- genProbablyKnownType
    eg <- go tg
    let tf = TyArr tg want
    ef <- go tf
    pure (App ef eg internal)

  genMatches pat want = Gen.list (Range.linear 1 10) one where
    one = (,) <$> genCorrectPattern pat <*> go want

genProbablyKnownType :: forall m. (Alternative m, MonadGen m) => ReaderT (Map.Map (Type Typed) [Expr Resolved]) m (Type Typed)
genProbablyKnownType = do
  known <- ask
  if Map.null known
     then genType
     else Gen.frequency [ (2, Gen.element $ Map.keys known)
                        , (1, genType) ]

genBadExpr :: (MonadGen m, Alternative m) => m (Expr Resolved)
genBadExpr = Gen.small $ do
  be <- Gen.choice [ genBadApp
                   , genBadMatch
                   ]
  Gen.recursive Gen.choice
    [ Gen.constant be ]
    [ do tg <- genType
         tf <- genType
         let ta = TyArr tg tf
         ea <- genCorrectExpr ta
         Gen.constant (App ea be internal)
    ]

genBadApp :: (MonadGen m, Alternative m) => m (Expr Resolved)
genBadApp = do
  let notArr TyArr{} = False
      notArr _ = True
  t <- Gen.filter notArr genType
  f <- genCorrectExpr t
  x <- genCorrectExpr =<< genType
  pure (App f x internal)

genBadMatch :: (MonadGen m, Alternative m) => m (Expr Resolved)
genBadMatch = do
  (a, b, c) <- (,,) <$> genType <*> genType <*> genType
  guard (a /= b)
  guard (b /= c)
  overall <- genType

  one <- (,) <$> genCorrectPattern a <*> genCorrectExpr overall
  two <- (,) <$> genCorrectPattern c <*> genCorrectExpr overall
  Match <$> genCorrectExpr b <*> pure [one, two] <*> pure internal

genCorrectPattern :: MonadGen m => Type Typed -> m (Pattern Resolved)
genCorrectPattern want = case want of
  TyTuple a b -> do
    (pa, pb) <- (,) <$> genCorrectPattern a <*> genCorrectPattern b
    pure (PTuple [pa, pb] internal)
  _ -> do
    p <- Gen.choice [ Capture <$> genVar <*> pure internal
                    , Gen.constant (Wildcard internal)
                    ]
    pure (PType p (raiseT unTvName want) internal)
