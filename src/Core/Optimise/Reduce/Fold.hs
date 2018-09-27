{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Core.Optimise.Reduce.Fold
  ( foldApply
  ) where

import qualified Data.Text as T

import Core.Optimise.Reduce.Base
import Core.Builtin

foldApply :: CoVar -> [Atom a] -> Maybe (Term a)
foldApply v xs =
  case xs of
    -- Primitive integer reductions
    [Lit (Int l), Lit (Int r)] | v == vOpAdd -> num (l + r)
    [Lit (Int l), Lit (Int r)] | v == vOpSub -> num (l - r)
    [Lit (Int l), Lit (Int r)] | v == vOpMul -> num (l * r)
    [Lit (Int l), Lit (Int r)] | v == vOpDiv -> num (l `div` r)
    [Lit (Int l), Lit (Int r)] | v == vOpExp -> num (l ^ r)
    [Lit (Int l), Lit (Int r)] | v == vOpLt -> bool (l < r)
    [Lit (Int l), Lit (Int r)] | v == vOpGt -> bool (l > r)
    [Lit (Int l), Lit (Int r)] | v == vOpLe -> bool (l <= r)
    [Lit (Int l), Lit (Int r)] | v == vOpGe -> bool (l >= r)

    -- Partial integer reductions
    [x, Lit (Int 0)] | v == vOpAdd -> atom x
    [Lit (Int 0), x] | v == vOpAdd -> atom x
    [Lit (Int 0), x] | v == vOpSub -> atom x
    [x, Lit (Int 1)] | v == vOpMul -> atom x
    [Lit (Int 1), x] | v == vOpMul -> atom x
    [_, Lit (Int 0)] | v == vOpMul -> num 0
    [Lit (Int 0), _] | v == vOpMul -> num 0
    [x, Lit (Int 1)] | v == vOpDiv -> atom x

    -- Primitive string reductions
    [Lit (Str l), Lit (Str r)]  | v == vOpConcat -> str (l `T.append` r)

    -- Partial string reductions
    [x, Lit (Str "")] | v == vOpConcat -> atom x
    [Lit (Str ""), x] | v == vOpConcat -> atom x

    [x, y] | v == vOpApp -> Just (App x y)

    _ -> Nothing
  where
    num = atom . Lit . Int
    str = atom . Lit . Str
    bool x = atom (Lit (if x then LitTrue else LitFalse))
    atom = Just . Atom
