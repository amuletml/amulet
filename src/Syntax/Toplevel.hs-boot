{-# LANGUAGE RoleAnnotations, FlexibleContexts, UndecidableInstances #-}
module Syntax.Toplevel where

import Data.Data

import Text.Pretty.Semantic

import Syntax.Type
import Syntax.Var

type role ModuleTerm nominal
data ModuleTerm p

instance EqPhrase p => Eq (ModuleTerm p)
instance ShowPhrase p => Show (ModuleTerm p)
instance OrdPhrase p => Ord (ModuleTerm p)
instance DataPhrase p => Data (ModuleTerm p)
instance Pretty (Var p) => Pretty (ModuleTerm p)
