{-# LANGUAGE GADTs, ConstraintKinds, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Control.Monad.Infer.Error where

import Data.Functor.Const
import Data.Spanned
import Data.Span
import Data.Data

import Syntax.Pretty
import Text.Pretty.Semantic

data SomeReason where
  BecauseOf :: (Reasonable a p) => a p -> SomeReason

instance Pretty SomeReason where
  pretty (BecauseOf a) = pretty a

instance Spanned SomeReason where
  annotation (BecauseOf a) = annotation a

instance Show SomeReason where
  show (BecauseOf _) = "reason"

instance Eq SomeReason where
  BecauseOf _ == BecauseOf _ = False

class (Spanned (f p), Pretty (f p)) => Reasonable f p where
  blame :: f p -> Doc
  blame _ = empty

instance (Spanned (Pattern p), Pretty (Var p)) => Reasonable Pattern p where
  blame _ = string "the" <+> highlight "pattern"

instance (Spanned (Expr p), Pretty (Var p)) => Reasonable Expr p where
  blame _ = string "the" <+> highlight "expression"

instance (Data p, Data (Ann p), Data (Var p), Pretty (Var p)) => Reasonable Constructor p where
  blame _ = string "the" <+> highlight "constructor"

instance (Ann p ~ Span, Data p, Data (Ann p), Data (Var p), Pretty (Var p)) => Reasonable Toplevel p where
  blame _ = string "the" <+> highlight "declaration"

instance Reasonable (Const SomeReason) p where
  blame = blameOf . getConst

instance Spanned (Const SomeReason a) where
  annotation = annotation . getConst

instance Pretty (Const SomeReason a) where
  pretty = pretty . getConst

blameOf :: SomeReason -> Doc
blameOf (BecauseOf (x :: f p)) = blame x
