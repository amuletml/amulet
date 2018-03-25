{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
module Syntax.Desugar (desugarProgram) where

import Control.Monad.Gen
import Control.Monad

import qualified Data.Text as T
import Data.Foldable
import Data.Triple

import Syntax

desugarProgram :: forall m. MonadGen Int m => [Toplevel Resolved] -> m [Toplevel Resolved]
desugarProgram = traverse statement where
  statement (LetStmt vs) = LetStmt <$> traverse (second3A expr) vs
  statement (Module v ss) = Module v <$> traverse statement ss
  statement x = pure x

  expr x@Literal{} = pure x
  expr x@VarRef{} = pure x
  expr x@Hole{} = pure x
  expr x@TypeApp{} = pure x
  expr x@Cast{} = pure x
  expr (Let vs e a) = Let <$> traverse (second3A expr) vs <*> expr e <*> pure a
  expr (If c t e a) = If <$> expr c <*> expr t <*> expr e <*> pure a
  expr (App f x a) = App <$> expr f <*> expr x <*> pure a
  expr (Fun p b a) = Fun p <$> expr b <*> pure a
  expr (Begin es a) = Begin <$> traverse expr es <*> pure a
  expr (Match e bs a) = Match <$> expr e <*> traverse (secondA expr) bs <*> pure a
  expr (Function [(p, b)] a) = Fun p <$> expr b <*> pure a
  expr (Function bs a) = do
    (cap, rhs) <- fresh a
    Fun cap <$>
      (Match <$> expr rhs <*> traverse (secondA expr) bs <*> pure a)
      <*> pure a
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

  expr (Parens e _) = expr e

  expr (Tuple es a) = Tuple <$> traverse expr es <*> pure a
  expr (TupleSection es a) = do
    es' <- traverse (traverse expr) es
    (args, binds, tuple) <- foldrM (buildTuple a) ([], [], []) es'
    pure $ foldf (\(v, e) r -> Let [(v, e, a)] r a) binds
         $ foldf (\v e -> Fun v e a) args
         $ Tuple tuple a

  buildTuple :: MonadGen Int m
             => Ann Resolved
             -> Maybe (Expr Resolved)
             -> ([Pattern Resolved], [(Var Resolved, Expr Resolved)], [Expr Resolved])
             -> m ([Pattern Resolved], [(Var Resolved, Expr Resolved)], [Expr Resolved])
  buildTuple a Nothing (as, vs, tuple) = do
    (p, v) <- fresh a
    pure (p:as, vs, v:tuple)
  buildTuple _ (Just e@VarRef{}) (as, vs, tuple) = pure (as, vs, e:tuple)
  buildTuple _ (Just e@Literal{}) (as, vs, tuple) = pure (as, vs, e:tuple)
  buildTuple a (Just e) (as, vs, tuple) = do
    (Capture v _, ref) <- fresh a
    pure (as, (v, e):vs, ref:tuple)

  foldf f xs v = foldr f v xs

fresh :: MonadGen Int m => Ann Resolved -> m (Pattern Resolved, Expr Resolved)
fresh an = do
  x <- gen
  let var = TgName (alpha !! x) x
  pure (Capture var an, VarRef var an)

alpha :: [T.Text]
alpha = map T.pack $ [1..] >>= flip replicateM ['a'..'z']
