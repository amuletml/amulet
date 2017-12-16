module Types.Holes where

-- Post-TC phase to identify and error on typed holes.

import Data.Triple

import Syntax

findHoles :: [Toplevel Typed] -> [Expr Typed]
findHoles (LetStmt vs _:xs) = concatMap (findExprHoles . snd3) vs ++ findHoles xs
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
findExprHoles _ = []
