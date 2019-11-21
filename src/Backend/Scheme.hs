module Backend.Scheme (genScheme) where

import Core.Core
import Core.Var

import Text.Pretty.Semantic

genScheme :: [Stmt CoVar] -> Doc
genScheme [] = empty
