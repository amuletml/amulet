{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
module Syntax.Desugar (desugarProgram) where

import Control.Monad.Gen

import qualified Data.Text as T

import Syntax

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

fresh :: MonadGen Int m => Ann Parsed -> m (Pattern Parsed, Expr Parsed)
fresh an = do
  var <- Name . T.cons '_' . T.pack . show <$> gen
  pure (Capture var an, VarRef var an)

