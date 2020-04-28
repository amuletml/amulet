{-# LANGUAGE DuplicateRecordFields, FlexibleContexts, TypeFamilies #-}
module AmuletLsp.Features.Folding (getFolds) where

import Control.Monad

import Data.Sequence (Seq((:<|)), (<|))
import Data.Foldable
import Data.Position
import Data.Spanned
import Data.Span

import Language.Haskell.LSP.Types

import Syntax

getFolds :: [Toplevel Parsed] -> [FoldingRange]
getFolds = toList . foldMap getTop where

  getTop :: Toplevel Parsed -> Seq FoldingRange
  getTop t@ForeignVal{}  = mk t
  getTop t@TypeDecl{}    = mk t
  getTop t@TySymDecl{}   = mk t
  getTop t@TypeFunDecl{} = mk t

  getTop DeriveInstance{} = mempty
  getTop t@Class { classMethods = ms } = mk t <> foldMap getClass ms
  getTop t@Instance { instanceMethods = ms } = mk t <> foldMap getInstance ms

  getTop (Open m)       = getModule m
  getTop (Include m)    = getModule m
  getTop (Module _ _ m) = getModule m

  getTop (LetStmt _ _ bindings _) = foldMap getBinding bindings

  getModule :: ModuleTerm Parsed -> Seq FoldingRange
  getModule m@(ModStruct ts _) = mk m <> foldMap getTop ts
  getModule ModRef{} = mempty
  getModule ModImport{} = mempty
  getModule ModTargetImport{} = mempty

  getBinding :: Binding Parsed -> Seq FoldingRange
  getBinding b@(Binding _ _ e _ _) = b `mkTo` getExpr e
  getBinding b@(Matching _ e _) = b `mkTo` getExpr e
  getBinding TypedMatching{} = mempty

  getClass :: ClassItem Parsed -> Seq FoldingRange
  getClass MethodSig{} = mempty
  getClass AssocType{} = mempty
  getClass (DefaultMethod b _) = getBinding b

  getInstance :: InstanceItem Parsed -> Seq FoldingRange
  getInstance (MethodImpl b) = getBinding b
  getInstance TypeImpl{} = mempty

  getExpr :: Expr Parsed -> Seq FoldingRange

  -- Short circuit expressions which will never yield any folds right
  -- here.
  getExpr e
    | s <- annotation e
    , SourcePos _ sl _ <- spanStart s
    , SourcePos _ el _ <- spanEnd s
    , sl == el = mempty

  getExpr VarRef{} = mempty
  getExpr Literal{} = mempty
  getExpr Hole{} = mempty
  getExpr BothSection{} = mempty
  getExpr AccessSection{} = mempty

  getExpr (Let _ bs bod _) = foldMap getBinding bs <> getExpr bod
  getExpr (Fun _ bod _) = bod `mkTo` getExpr bod
  getExpr e@(Begin es _) = e `mkTo` foldMap getExpr es
  getExpr e@(Match t as _ _) = mk e <> getExpr t <> foldMap getArm as
  getExpr e@(Function as _ _) = mk e <> foldMap getArm as
  getExpr e@(MLet _ _ _ bod _) = mk e <> getExpr bod
  getExpr e@(Record fs _) = mk e <> foldMap getField fs
  getExpr e@(RecordExt r fs _) = mk e <> mk r <> foldMap getField fs

  getExpr e@(If c t f _) = mk e <> getExpr c <> getExpr t <> getExpr f
  getExpr (App f x _) = getExpr f <> getExpr x
  getExpr (BinOp l _ r _) = getExpr l <> getExpr r
  getExpr (Ascription e _ _) = getExpr e
  getExpr (Access e _ _) = getExpr e
  getExpr (LeftSection _ e _) = getExpr e
  getExpr (RightSection _ e _) = getExpr e
  getExpr (Parens e _) = getExpr e
  getExpr (Tuple es _) = foldMap getExpr es
  getExpr (TupleSection es _) = foldMap (foldMap getExpr) es
  getExpr (Lazy e _) = getExpr e
  getExpr (OpenIn _ e _) = getExpr e
  getExpr (Vta e _ _) = getExpr e
  getExpr (ListExp es _) = foldMap getExpr es
  getExpr (ListComp e es _) = getExpr e <> foldMap getComp es
  getExpr (ListFromTo _ f t _) = getExpr f <> getExpr t
  getExpr (ListFromThenTo _ f t s _) = getExpr f <> getExpr t <> getExpr s
  getExpr (ListFrom _ e _) = getExpr e
  getExpr (ListFromThen _ f s _) = getExpr f <> getExpr s
  getExpr (Idiom _ _ e _) = getExpr e
  getExpr (ExprWrapper _ e _) = getExpr e

  getArm a@Arm { armExp = e } = a `mkTo` getExpr e

  getComp (CompGuard e) = getExpr e
  getComp (CompLet bs _) = foldMap getBinding bs
  getComp (CompGen _ e _) = getExpr e

  getField (Field _ e _) = getExpr e

mk :: (Spanned a, MonadPlus m) => a -> m FoldingRange
mk node =
  let s = spanOf node
      (SourcePos _ sl sc) = spanStart s
      (SourcePos _ el ec) = spanEnd s
  in if sl >= el then mzero else
     pure FoldingRange { _startLine = sl - 1
                       , _startCharacter = Just sc
                       , _endLine = el - 1
                       , _endCharacter = Just (ec - 1)
                       , _kind = Nothing }

-- | Append a span to a sequence of other ones. If the head is similar,
-- then we replace it.
mkTo :: Spanned a => a -> Seq FoldingRange -> Seq FoldingRange
mkTo node rest =
  case mk node of
    Nothing -> rest
    Just pos
      | first :<| rest' <- rest
      , _startLine first == _startLine pos
      , _endLine first == _endLine pos
      -> pos <| rest'

      | otherwise -> pos <| rest
