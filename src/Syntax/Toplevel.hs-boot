{-# LANGUAGE RoleAnnotations, FlexibleContexts, UndecidableInstances #-}
module Syntax.Toplevel where

import Data.Data

import Text.Pretty.Semantic

import Syntax.Type
import Syntax.Var

type role ModuleTerm nominal
data ModuleTerm p

instance (Eq (Var p), Eq (Ann p)) => Eq (ModuleTerm p)
instance (Show (Var p), Show (Ann p)) => Show (ModuleTerm p)
instance (Ord (Var p), Ord (Ann p)) => Ord (ModuleTerm p)
instance (Data p, Typeable p, Data (Var p), Data (Ann p)) => Data (ModuleTerm p)
instance Pretty (Var p) => Pretty (ModuleTerm p)
