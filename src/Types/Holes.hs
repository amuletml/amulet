module Types.Holes where

-- Post-TC phase to identify and error on typed holes.

import Data.Triple

import Syntax

findHoles :: [Toplevel Typed] -> [Expr Typed]
findHoles (LetStmt vs:xs) = concatMap (findExprHoles . snd3) vs ++ findHoles xs
findHoles (_:xs) = findHoles xs
findHoles [] = []

findExprHoles :: Expr Typed -> [Expr Typed]
findExprHoles x@Hole{} = [x]
findExprHoles (Let vs e _)
  = concatMap (findExprHoles . snd3) vs ++ findExprHoles e
findExprHoles (If c t e _)
  = concat [ findExprHoles c, findExprHoles t, findExprHoles e ]
findExprHoles (BinOp c t e _)
  = concat [ findExprHoles c, findExprHoles t, findExprHoles e ]
findExprHoles (App x y _) = findExprHoles x ++ findExprHoles y
findExprHoles (Fun _ y _) = findExprHoles y
findExprHoles (Begin xs _) = concatMap findExprHoles xs
findExprHoles (Match m vs _) = findExprHoles m ++ concatMap (findExprHoles . snd) vs
findExprHoles (Ascription e _ _) = findExprHoles e
findExprHoles (Record rs _) = concatMap (findExprHoles . snd) rs
findExprHoles (RecordExt e rs _) = findExprHoles e ++ concatMap (findExprHoles . snd) rs
findExprHoles (Access e _ _) = findExprHoles e
findExprHoles (Tuple es _) = concatMap findExprHoles es
findExprHoles (TypeApp e _ _) = findExprHoles e

findExprHoles LeftSection{} = error "impossible (desugar)"
findExprHoles RightSection{} = error "impossible (desugar)"
findExprHoles BothSection{} = error "impossible (desugar)"
findExprHoles AccessSection{} = error "impossible (desugar)"
findExprHoles TupleSection{} = error "impossible (desugar)"
findExprHoles VarRef{} = []
findExprHoles Literal{} = []
