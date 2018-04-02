{-# LANGUAGE GADTs, ConstraintKinds, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Control.Monad.Infer.Error where

import Data.Spanned
import Data.Proxy
import Data.Span
import Data.Data

import Syntax.Pretty
import Pretty

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
  blame :: Proxy (f p) -> Doc
  blame _ = empty

instance (Spanned (Pattern p), Pretty (Var p)) => Reasonable Pattern p where
  blame _ = string "the pattern"

instance (Spanned (Expr p), Pretty (Var p)) => Reasonable Expr p where
  blame _ = string "the expression"

instance (Data p, Data (Ann p), Data (Var p), Pretty (Var p)) => Reasonable Constructor p where
  blame _ = string "the constructor"

instance (Data p, Data (Ann p), Data (Var p), Pretty (Var p)) => Reasonable Type p where
  blame _ = string "the type"

instance (Ann p ~ Span, Data p, Data (Ann p), Data (Var p), Pretty (Var p)) => Reasonable Toplevel p where
  blame _ = string "the declaration"

blameOf :: SomeReason -> Doc
blameOf (BecauseOf (_ :: f p)) = blame (Proxy :: Proxy (f p))
