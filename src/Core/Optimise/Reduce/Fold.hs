{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Core.Optimise.Reduce.Fold
  ( foldApply
  ) where

import qualified Data.Text as T

import Core.Optimise.Reduce.Base
import Core.Builtin
import Core.Types

foldApply :: IsVar a => CoVar -> [Atom a] -> Maybe (Term a)
foldApply v xs =
  case xs of
    -- Complete integer reductions
    [Lit (Int l), Lit (Int r)] | v == vOpAdd -> num (l + r)
    [Lit (Int l), Lit (Int r)] | v == vOpSub -> num (l - r)
    [Lit (Int l), Lit (Int r)] | v == vOpMul -> num (l * r)
    [Lit (Int l), Lit (Int r)] | v == vOpDiv -> num (l `div` r)
    [Lit (Int l), Lit (Int r)] | v == vOpExp -> num (l ^ r)
    [Lit (Int l), Lit (Int r)] | v == vOpEq -> bool (l == r)
    [Lit (Int l), Lit (Int r)] | v == vOpNe -> bool (l /= r)
    [Lit (Int l), Lit (Int r)] | v == vOpLt -> bool (l < r)
    [Lit (Int l), Lit (Int r)] | v == vOpGt -> bool (l > r)
    [Lit (Int l), Lit (Int r)] | v == vOpLe -> bool (l <= r)
    [Lit (Int l), Lit (Int r)] | v == vOpGe -> bool (l >= r)

    -- Partial integer reductions
    [x, Lit (Int 0)] | v == vOpAdd -> atom x
    [Lit (Int 0), x] | v == vOpAdd -> atom x
    [x, Lit (Int 0)] | v == vOpSub -> atom x
    [x, Lit (Int 1)] | v == vOpMul -> atom x
    [Lit (Int 1), x] | v == vOpMul -> atom x
    [_, Lit (Int 0)] | v == vOpMul -> num 0
    [Lit (Int 0), _] | v == vOpMul -> num 0
    [x, Lit (Int 1)] | v == vOpDiv -> atom x

    -- Complete string reductions
    [Lit (Str l), Lit (Str r)]  | v == vOpConcat -> str (l `T.append` r)
    [Lit (Str l), Lit (Str r)]  | v == vOpEq -> bool (l == r)
    [Lit (Str l), Lit (Str r)]  | v == vOpNe -> bool (l /= r)

    -- Partial string reductions
    [x, Lit (Str "")] | v == vOpConcat -> atom x
    [Lit (Str ""), x] | v == vOpConcat -> atom x

    -- Complete boolean reductions
    [Lit l@LitTrue,  Lit r] | v == vOpEq -> bool (l == r)
    [Lit l@LitFalse, Lit r] | v == vOpEq -> bool (l == r)
    [Lit l@LitTrue,  Lit r] | v == vOpNe -> bool (l /= r)
    [Lit l@LitFalse, Lit r] | v == vOpNe -> bool (l /= r)

    -- Unit reductions (always equal, so no further checks needed)
    [x, _] | v == vOpEq, approximateAtomType x == tyUnit -> bool True
    [x, _] | v == vOpNe, approximateAtomType x == tyUnit -> bool False

    -- Reduce (@@) operator
    [x, y] | v == vOpApp -> Just (App x y)

    -- Equality of known values
    [Ref l ty, Ref r _]
      | v == vOpEq, l == r
      , compareTy ty
      -> bool True
    [Ref l ty, Ref r _]
      | v == vOpNe, l == r
      , compareTy ty
      -> bool False

    _ -> Nothing
  where
    num = atom . Lit . Int
    str = atom . Lit . Str
    bool x = atom (Lit (if x then LitTrue else LitFalse))
    atom = Just . Atom

    compareTy t = t == tyBool
               || t == tyInt
               || t == tyString
               || t == tyUnit
