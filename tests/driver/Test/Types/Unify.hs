{-# LANGUAGE TemplateHaskell #-}
module Test.Types.Unify where

import Text.Pretty.Semantic

import Test.Syntax.Gen
import Test.Types.Util
import Test.Util

import qualified Data.Set as Set

import Data.Function
import Data.Foldable
import Data.List

import Hedgehog

import Syntax.Subst
import Syntax

prop_genTypeWellScoped :: Property
prop_genTypeWellScoped = withTests 1000 . property $ do
  aty <- forAllWith (displayS . displayType) genType
  footnote . displayS $
    vsep [ string "Type:" <+> displayTypeTyped aty
         , string "ftv:" <+> shown (ftv aty)
         ]
  assert (Set.null (ftv aty))

prop_unify_goodReflCo :: Property
prop_unify_goodReflCo = withTests 1000 . property $ do
  aty <- forAllWith (show . displayType) genType
  case unify aty aty of
    Left e -> (footnote . show . pretty . toList $ e) *> failure
    Right x | (ca, cb) <- provenCoercion x -> do
      footnote . displayS $
          keyword "Given type:" <+> displayType aty
          <#> keyword "Coercion proves:" <+> displayType ca <+> soperator (char '~') <+> displayType cb
      case unify aty ca of
        Left e -> do
          footnote $ "Left-hand type of coercion doesn't match unifier input"
          footnote . show . pretty . toList $ e
          failure
        Right{} -> pure ()

      case unify ca cb of
        Left e -> do
          footnote $ "Unifier did not make a reflexive coercion for equal input types"
          footnote . show . pretty . toList $ e
          failure
        Right{} -> pure ()

prop_unify_goodCoOrDisjoint :: Property
prop_unify_goodCoOrDisjoint = property $ do
  aty <- forAllWith (displayS . displayType) genType
  bty <- forAllWith (displayS . displayType) genType
  footnote . displayS $
    keyword "Given types:" <+> displayType aty <+> keyword "and" <+> displayType bty

  case unify aty bty of
    Left{} -> footnote "They're disjoint" *> success
    Right x | (ca, cb) <- provenCoercion x -> do
      footnote . displayS $
        keyword "Given types:" <+> displayType aty <+> keyword "and" <+> displayType aty
          <#> keyword "Coercion proves:" <+> displayType ca <+> soperator (char '~') <+> displayType cb
      if ca `disjoint` aty || cb `disjoint` bty
         then failure
         else success

provenCoercion :: Coercion Typed -> (Type Typed, Type Typed)
provenCoercion (ReflCo a) = (a, a)
provenCoercion VarCo{} = undefined
provenCoercion MvCo{} = undefined
provenCoercion P1{} = undefined
provenCoercion P2{} = undefined
provenCoercion InstCo{} = undefined
provenCoercion (SymCo c)
  | (a, b) <- provenCoercion c = (b, a)
provenCoercion (TransCo x y)
  | (a, _) <- provenCoercion x
  , (_, c) <- provenCoercion y
  = (a, c)
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
provenCoercion (ForallCo v vis k t) =
  let (ka, kb) = provenCoercion k
      (ta, tb) = provenCoercion t
   in (TyPi (Invisible v (Just ka) vis) ta, TyPi (Invisible v (Just kb) vis) tb)
provenCoercion (AssumedCo a b) = (a, b)

tests :: TestTree
tests = hedgehog $ $$(discover)
