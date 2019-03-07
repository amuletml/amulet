{-# LANGUAGE TemplateHaskell #-}
module Test.Types.Unify where


import Text.Pretty.Semantic

import Test.Syntax.Gen
import Test.Types.Util

import Data.Function
import Data.Foldable
import Data.List

import Control.Monad

import Hedgehog

import Syntax

prop_unifyMakesGoodCoercion :: Property
prop_unifyMakesGoodCoercion = property $ do
  aty <- forAllWith (displayS . displayType) genType
  case unify aty aty of
    Left e -> (footnote . displayS . pretty . toList $ e) *> failure
    Right x | (ca, cb) <- provenCoercion x -> do
      footnote . displayS $
          keyword "Given type:" <+> displayType aty
          <#> keyword "Coercion proves:" <+> pretty ca <+> soperator (char '~') <+> pretty cb
      assert (aty `equivalent` ca && aty `equivalent` cb)

prop_disjointTypesDon'tUnify :: Property
prop_disjointTypesDon'tUnify = property $ do
  aty <- forAllWith (displayS . displayType) genType
  bty <- forAllWith (displayS . displayType) genType
  unless (aty `disjoint` bty) discard
  case unify aty bty of
    Left{} -> success
    Right x | (ca, cb) <- provenCoercion x -> do
      footnote . displayS $
        keyword "Given types:" <+> displayType aty <+> keyword "and" <+> displayType aty
          <#> keyword "Coercion proves:" <+> pretty ca <+> soperator (char '~') <+> pretty cb
      failure

provenCoercion :: Coercion Typed -> (Type Typed, Type Typed)
provenCoercion (ReflCo a) = (a, a)
provenCoercion VarCo{} = undefined
provenCoercion (SymCo c)
  | (a, b) <- provenCoercion c = (b, a)
provenCoercion (AppCo c c')
  | (f, f') <- provenCoercion c, (x, x') <- provenCoercion c'
  = (TyApp f x, TyApp f' x')
provenCoercion (ArrCo c c')
  | (f, f') <- provenCoercion c, (x, x') <- provenCoercion c'
  = (TyArr f x, TyArr f' x')
provenCoercion (ProdCo c c')
  | (f, f') <- provenCoercion c, (x, x') <- provenCoercion c'
  = (TyTuple f x, TyTuple f' x')
provenCoercion (ExactRowsCo rs) =
  let one (t, c) =
        case provenCoercion c of
          (a, b) -> ((t, a), (t, b))
      (rows, rows') = unzip (map one rs)
   in (TyExactRows rows, TyExactRows rows')
provenCoercion (RowsCo c rs) =
  let one (t, c) =
        case provenCoercion c of
          (a, b) -> ((t, a), (t, b))
      (rows, rows') = unzip (map one rs)
      (a, b) = provenCoercion c
   in (TyRows a rows, TyRows b rows')
provenCoercion (ProjCo rs rs') =
  let assumed (t, a) = ((t, a), (t, a))
      (as, bs) = unzip (map assumed rs)
      proven (t, c) =
        case provenCoercion c of
          (a, b) -> ((t, a), (t, b))
      (ss, ts) = unzip (map proven rs')
      first = unionBy ((==) `on` fst) as ss
   in (TyExactRows first, TyRows (TyExactRows bs) ts)
provenCoercion (ForallCo v k t) =
  let (ka, kb) = provenCoercion k
      (ta, tb) = provenCoercion t
   in (TyForall v (Just ka) ta, TyForall v (Just kb) tb)
provenCoercion (AssumedCo a b) = (a, b)

tests :: Group
tests = $$(discover)
