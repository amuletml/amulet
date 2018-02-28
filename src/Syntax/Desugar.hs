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
  -- (+)
  defaults (BothSection op an) = do
    (ap, ar) <- fresh an
    (bp, br) <- fresh an
    pure (Fun ap (Fun bp (BinOp ar op br an) an) an)

  -- (+ foo)
  defaults (LeftSection op vl an) = do
    (cap, rhs) <- fresh an
    let go lhs = pure (Fun cap (BinOp rhs op lhs an) an)
    case vl of
      VarRef{} -> go vl
      Literal{} -> go vl
      _ -> do
        (Capture lv _, ref) <- fresh an
        Let [(lv, vl, an)] <$> go ref <*> pure an

  -- (foo +)
  defaults (RightSection vl op an) = pure (App op vl an)
  defaults (AccessSection key an) = do
    (cap, ref) <- fresh an
    pure (Fun cap (Access ref key an) an)

  defaults x = pure x

fresh :: MonadGen Int m => Ann Parsed -> m (Pattern Parsed, Expr Parsed)
fresh an = do
  var <- Name . T.cons '_' . T.pack . show <$> gen
  pure (Capture var an, VarRef var an)

