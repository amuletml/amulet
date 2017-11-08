{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
module Syntax.Desugar (desugarProgram) where

import Control.Monad.Gen

import qualified Data.Text as T

import Syntax

import Generics.SYB.Schemes
import Generics.SYB

desugarProgram :: forall m. MonadGen Int m => [Toplevel Parsed] -> m [Toplevel Parsed]
desugarProgram = everywhereM (mkM defaults) where
  defaults :: Expr Parsed -> m (Expr Parsed)
  defaults (BothSection op _) = pure op
  defaults (LeftSection op vl an) = do
    (cap, ref) <- fresh an
    pure (Fun cap (BinOp ref op vl an) an)
  defaults (RightSection op vl an) = do
    (cap, ref) <- fresh an
    pure (Fun cap (BinOp vl op ref an) an)
  defaults (AccessSection key an) = do
    (cap, ref) <- fresh an
    pure (Fun cap (Access ref key an) an)
  defaults x = pure x

fresh :: MonadGen Int m => Ann Parsed -> m (Pattern Parsed, Expr Parsed)
fresh an = do
  var <- Name . T.cons '_' . T.pack . show <$> gen
  pure (Capture var an, VarRef var an)

