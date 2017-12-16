module Core.Types
  ( arity
  ) where

import Core.Core

arity :: CoType -> Int
arity (CotyArr _ t) = 1 + arity t
arity (CotyForall _ t) = arity t
arity _ = 0
