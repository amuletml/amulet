{-# LANGUAGE FlexibleContexts #-}
module Syntax.Desugar (desugarProgram) where

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
  = perform =<< case expr of
      VarRef{} -> pure expr
      Literal{} -> pure expr
      Hole{} -> pure expr
      AccessSection{} -> pure expr
      Let bs e a -> Let <$> desugarPair lower bs <*> lo e <*> pure a
      If c t e a -> If <$> lo c <*> lo t <*> lo e <*> pure a
      App f x a -> App <$> lo f <*> lo x <*> pure a
      Fun p e a -> Fun p <$> lo e <*> pure a
      Begin es a -> Begin <$> mapM lo es <*> pure a
      Match e as an -> Match <$> lo e <*> desugarPair lower as <*> pure an
      BinOp l o r a -> BinOp <$> lo l <*> lo o <*> lo r <*> pure a
      Record rows a -> Record <$> desugarPair lower rows <*> pure a
      RecordExt b r a -> RecordExt <$> lo b <*> desugarPair lower r <*> pure a
      Access ex k a -> Access <$> lo ex <*> pure k <*> pure a
      LeftSection a b an -> LeftSection <$> lo a <*> lo b <*> pure an
      RightSection a b an -> LeftSection <$> lo a <*> lo b <*> pure an
      BothSection o an -> BothSection <$> lo o <*> pure an
      Tuple es an -> Tuple <$> mapM lo es <*> pure an
      EHasType e t a -> EHasType <$> lo e <*> pure t <*> pure a

desugarPair :: MonadGen Int m
            => (Expr Parsed -> m (Maybe (Expr Parsed)))
            -> [(a, Expr Parsed)] -> m [(a, Expr Parsed)]
desugarPair lower bg
  = forM bg $ \(var, expr) -> (,) var <$> desugar lower expr

maybeM :: Monad m => m a -> (b -> m a) -> m (Maybe b) -> m a
maybeM n j x = maybe n j =<< x

desugarProgram :: MonadGen Int m => [Toplevel Parsed] -> m [Toplevel Parsed]
desugarProgram = mapM desugarTop where
  desugarTop (LetStmt vs an) = LetStmt <$> desugarPair defaults vs <*> pure an
  desugarTop x = pure x

fresh :: MonadGen Int m => Ann Parsed -> m (Pattern Parsed, Expr Parsed)
fresh an = do
  var <- Name . T.cons '_' . T.pack . show <$> gen
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
