{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, PackageImports #-}
module Syntax.Desugar (desugarProgram) where

import Control.Monad.Gen

import qualified Data.Text as T

import Syntax

import Data.Foldable
import Data.Triple

desugarProgram :: forall m. MonadGen Int m => [Toplevel Parsed] -> m [Toplevel Parsed]
desugarProgram = traverse statement where
  statement (LetStmt vs) = LetStmt <$> traverse (second3A expr) vs
  statement (Module v ss) = Module v <$> traverse statement ss
  statement x = pure x

  expr x@Literal{} = pure x
  expr x@VarRef{} = pure x
  expr x@Hole{} = pure x
  expr x@TypeApp{} = pure x
  expr (Let vs e a) = Let <$> traverse (second3A expr) vs <*> expr e <*> pure a
  expr (If c t e a) = If <$> expr c <*> expr t <*> expr e <*> pure a
  expr (App f x a) = App <$> expr f <*> expr x <*> pure a
  expr (Fun p b a) = Fun p <$> expr b <*> pure a
  expr (Begin es a) = Begin <$> traverse expr es <*> pure a
  expr (Match e bs a) = Match <$> expr e <*> traverse (secondA expr) bs <*> pure a
  expr (BinOp l o r a) = BinOp <$> expr l <*> expr o <*> expr r <*> pure a
  expr (Ascription e t a) = Ascription <$> expr e <*> pure t <*> pure a
  expr (Record rs a) = Record <$> traverse (secondA expr) rs <*> pure a
  expr (RecordExt e rs a) = RecordExt <$> expr e <*> traverse (secondA expr) rs <*> pure a
  expr (Access e k a) = Access <$> expr e <*> pure k <*> pure a

  expr (LeftSection op vl an) = do
    (cap, rhs) <- fresh an
    let go lhs = expr (Fun cap (BinOp rhs op lhs an) an)
    case vl of
      VarRef{} -> go vl
      Literal{} -> go vl
      _ -> do
        (Capture lv _, ref) <- fresh an
        Let [(lv, vl, an)] <$> go ref <*> pure an

  expr (RightSection vl op an) = expr (App op vl an)
  expr (BothSection o _) = pure o

  expr (AccessSection k a) = do
    (cap, ref) <- fresh a
    expr (Fun cap (Access ref k a) a)

  expr (Tuple es a) = Tuple <$> traverse expr es <*> pure a
  expr (TupleSection es a) = do
    es' <- traverse (traverse expr) es
    (body, tuple) <- foldrM (buildTuple a) (id, []) es'
    pure (body (Tuple tuple a))

  buildTuple :: MonadGen Int m
             => Ann Parsed
             -> (Maybe (Expr Parsed))
             -> (Expr Parsed -> Expr Parsed, [Expr Parsed])
             -> m (Expr Parsed -> Expr Parsed, [Expr Parsed])
  buildTuple a Nothing (wrapper, tuple) = do
    (p, v) <- fresh a
    pure (\x -> wrapper (Fun p x a), v:tuple)
  buildTuple _ (Just e@VarRef{}) (wrapper, tuple) = pure (wrapper, e:tuple)
  buildTuple _ (Just e@Literal{}) (wrapper, tuple) = pure (wrapper, e:tuple)
  buildTuple a (Just e) (wrapper, tuple) = do
    (Capture v _, ref) <- fresh a
    pure (\x -> Let [(v, e, a)] (wrapper x) a, ref:tuple)

fresh :: MonadGen Int m => Ann Parsed -> m (Pattern Parsed, Expr Parsed)
fresh an = do
  var <- Name . T.cons '_' . T.pack . show <$> gen
  pure (Capture var an, VarRef var an)
