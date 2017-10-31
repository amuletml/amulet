{-# LANGUAGE FlexibleContexts #-}
module Syntax.Desugar (desugarProgram) where

import qualified Control.Monad.Infer as M
import Control.Monad.Gen
import Control.Monad 

import qualified Data.Text as T

import Syntax

desugar :: MonadGen Int m
        => (Expr Parsed -> m (Maybe (Expr Parsed)))
        -> Expr Parsed
        -> m (Expr Parsed)
desugar lower expr
  | perform <- \x -> maybeM (pure x) pure (lower x)
  , lo <- desugar lower
  = case expr of
      VarRef{} -> perform expr
      Literal{} -> perform expr
      Hole{} -> perform expr
      AccessSection{} -> perform expr
      Let bs e a -> perform =<< Let <$> desugarPair lower bs <*> lo e <*> pure a
      If c t e a -> perform =<< If <$> lo c <*> lo t <*> lo e <*> pure a
      App f x a -> perform =<< App <$> lo f <*> lo x <*> pure a
      Fun p e a -> perform =<< Fun p <$> lo e <*> pure a
      Begin es a -> perform =<< Begin <$> mapM lo es <*> pure a
      Match e as an -> perform =<< Match <$> lo e <*> desugarPair lower as <*> pure an
      BinOp l o r a -> perform =<< BinOp <$> lo l <*> lo o <*> lo r <*> pure a
      Record rows a -> perform =<< Record <$> desugarPair lower rows <*> pure a
      RecordExt b r a -> perform =<< RecordExt <$> lo b <*> desugarPair lower r <*> pure a
      Access ex k a -> perform =<< Access <$> lo ex <*> pure k <*> pure a
      LeftSection a b an -> perform =<< LeftSection <$> lo a <*> lo b <*> pure an
      RightSection a b an -> perform =<< LeftSection <$> lo a <*> lo b <*> pure an
      BothSection o an -> perform =<< BothSection <$> lo o <*> pure an
      Tuple es an -> perform =<< Tuple <$> mapM lo es <*> pure an

desugarPair :: MonadGen Int m
            => (Expr Parsed -> m (Maybe (Expr Parsed)))
            -> [(a, Expr Parsed)] -> m [(a, Expr Parsed)]
desugarPair lower bg
  = forM bg $ \(var, expr) -> (,) var <$> desugar lower expr

maybeM :: Monad m => m a -> (b -> m a) -> m (Maybe b) -> m a
maybeM noth just val = do
  val' <- val
  case val' of
    Nothing -> noth
    Just x -> just x

desugarProgram :: [Toplevel Parsed] -> [Toplevel Parsed]
desugarProgram = runGen . mapM desugarTop where
  desugarTop (LetStmt vs an) = LetStmt <$> desugarPair defaults vs <*> pure an
  desugarTop x = pure x

fresh :: MonadGen Int m => Ann Parsed -> m (Pattern Parsed, Expr Parsed)
fresh an = do
  var <- Name . (T.cons '_') <$> M.fresh
  pure (Capture var an, VarRef var an)

defaults :: MonadGen Int m => Expr Parsed -> m (Maybe (Expr Parsed))
defaults (LeftSection op vl an) = do
  (cap, ref) <- fresh an
  pure (Just (Fun cap (BinOp ref op vl an) an))
defaults (RightSection op vl an) = do
  (cap, ref) <- fresh an
  pure (Just (Fun cap (BinOp vl op ref an) an))
defaults (AccessSection key an) = do
  (cap, ref) <- fresh an
  pure (Just (Fun cap (Access ref key an) an))
defaults (BothSection op _) = pure (Just op)
defaults _ = pure Nothing
