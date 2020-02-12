{- HLINT ignore -}
{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Test.Types.Holes where

import Control.Monad.Namey
import Control.Monad.Infer (firstName)
import Control.Arrow hiding ((<+>))
import Control.Lens hiding (Lazy)

import Test.Syntax.Gen
import Test.Types.Util
import Test.Util

import Syntax.Transform
import Syntax.Builtin
import Syntax.Desugar
import Syntax.Pretty
import Syntax

import Types.Holes

import Data.Foldable
import Data.Span

import Text.Pretty.Semantic

import Unsafe.Coerce

import Hedgehog

prop_amuseMakesTermsOfType :: Property
prop_amuseMakesTermsOfType = withTests 500 $ property $ testAmuse genSimpleType

prop_amuseProvesFormulas :: Property
prop_amuseProvesFormulas = withTests 500 $ property $ testAmuse genFormula

testAmuse :: Gen (Type Typed) -> PropertyT IO ()
testAmuse gen = do
  aty <- forAllWith (displayS . displayType) gen
  let (terms, nm) =
        runNamey (findHoleCandidate mempty internal builtinEnv aty) firstName

  cover 5 "single solution" (length terms == 1)
  cover 20 "multiple solutions" (length terms == 1)

  for_ terms $ \term -> do
    let term' = forgetTypes term
        ([LetStmt _ _ [Binding _  term'' _ _]], _) =
          runNamey (desugarProgram
                      [LetStmt NonRecursive Private [Binding firstName term' False internal]])
                   nm


    case checkExpr term'' aty of
      Left e -> do
        footnote . displayS $
          keyword "Proof term:" <+> pretty term''
        footnote . displayS . pretty . toList $ e
        failure
      Right () -> success

tests :: TestTree
tests = hedgehog $$discover

forgetTypes :: Expr Typed -> Expr Resolved
forgetTypes (VarRef v (an, _)) = VarRef v an
forgetTypes (Let re bs bd (a, _)) = Let re (forgetBinds <$> bs) (forgetTypes bd) a where
  forgetBinds (Binding v e b (a, _)) = Binding v (forgetTypes e) b a
  forgetBinds (Matching p e (a, _)) = Matching (forgetPat p) (forgetTypes e) a
  forgetBinds (TypedMatching p e (a, _) _) = Matching (forgetPat p) (forgetTypes e) a
forgetTypes (If c t e (a, _)) = If (forgetTypes c) (forgetTypes t) (forgetTypes e) a
forgetTypes (App f x (a, _)) = App (forgetTypes f) (forgetTypes x) a
forgetTypes (Fun p bd (a, _)) = Fun (forgetP p) (forgetTypes bd) a where
  forgetP (PatParam p) = PatParam $ forgetPat p
  forgetP (EvParam p) = EvParam $ forgetPat p
forgetTypes (Begin es (a, _)) = Begin (forgetTypes <$> es) a
forgetTypes (Literal l (a, _)) = Literal l a
forgetTypes (Match e as p (a, _)) = Match (forgetTypes e) (forgetArm <$> as) p a where
  forgetArm (Arm p g e) = Arm (forgetPat p) (forgetTypes <$> g) (forgetTypes e)
forgetTypes (Function as p (a, _)) = Function (forgetArm <$> as) p a where
  forgetArm (Arm p g e) = Arm (forgetPat p) (forgetTypes <$> g) (forgetTypes e)
forgetTypes (BinOp l o r (a, _)) = BinOp (forgetTypes l) (forgetTypes o) (forgetTypes r) a
forgetTypes (Hole v (a, _)) = Hole v a
forgetTypes (Ascription e t (a, _)) = Ascription (forgetTypes e) (forgetKind t) a
forgetTypes (Record fs (a, _)) = Record (forgetField <$> fs) a
forgetTypes (RecordExt e fs (a, _)) = RecordExt (forgetTypes e) (forgetField <$> fs) a
forgetTypes (Access r t (a, _)) = Access (forgetTypes r) t a

forgetTypes LeftSection{} = undefined
forgetTypes RightSection{} = undefined
forgetTypes BothSection{} = undefined
forgetTypes AccessSection{} = undefined
forgetTypes Parens{} = undefined

forgetTypes (Tuple es (a, _)) = Tuple (forgetTypes <$> es) a
forgetTypes TupleSection{} = undefined
forgetTypes OpenIn{} = undefined

forgetTypes (Lazy e (a, _)) = Lazy (forgetTypes e) a
forgetTypes (Vta e t (a, _)) = Vta (forgetTypes e) (forgetKind t) a
forgetTypes (ListExp es (a, _)) = ListExp (forgetTypes <$> es) a

forgetTypes ListComp{} = undefined
forgetTypes DoExpr{} = undefined
forgetTypes Idiom{} = undefined
forgetTypes ListFrom{} = undefined
forgetTypes ListFromTo{} = undefined
forgetTypes ListFromThen{} = undefined
forgetTypes ListFromThenTo{} = undefined

forgetTypes (ExprWrapper _ e _) = forgetTypes e

forgetField :: Field Typed -> Field Resolved
forgetField (Field t e (a, _)) = Field t (forgetTypes e) a

forgetPat :: Pattern Typed -> Pattern Resolved
forgetPat (Wildcard (a, _)) = Wildcard a
forgetPat (Capture v (a, _)) = Capture v a
forgetPat (Destructure v p (a, _)) = Destructure v (forgetPat <$> p) a
forgetPat (PAs p v (a, _)) = PAs (forgetPat p) v a
forgetPat (PType p t (a, _)) = PType (forgetPat p) (forgetKind t) a
forgetPat (PRecord rs (a, _)) = PRecord (map (second forgetPat) rs) a
forgetPat (PTuple p (a, _)) = PTuple (forgetPat <$> p) a
forgetPat (PList p (a, _)) = PTuple (forgetPat <$> p) a
forgetPat (PLiteral l (a, _)) = PLiteral l a
forgetPat PGadtCon{} = undefined

forgetKind :: Type Typed -> Type Resolved
forgetKind = unsafeCoerce . transformType go where
  go (TySkol v) = TyVar (v ^. skolVar)
  go x = x
