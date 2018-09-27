{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Core.Optimise.Reduce.Fold
  ( foldApply
  ) where

import Control.Lens

import qualified Data.Text as T

import Core.Optimise.Reduce.Base
import Core.Builtin
import Core.Types

-- | Apply constant folding an application of some arbitrary function.
--
-- Note the function provided must be the "raw" variant (not applied to
-- any types).
foldApply :: IsVar a => CoVar -> [Atom a] -> Maybe (Term a)
foldApply v _ | v ^. covarId >= 0 = Nothing -- Skip non-builtin functions
foldApply v xs =
  case xs of
    [Lit l, Lit r] -> Atom . Lit <$> foldLiteral v [l, r]

    -- Partial integer reductions
    [x, Lit (Int 0)] | v == vOpAdd -> atom x
    [Lit (Int 0), x] | v == vOpAdd -> atom x
    [x, Lit (Int 0)] | v == vOpSub -> atom x
    [x, Lit (Int 1)] | v == vOpMul -> atom x
    [Lit (Int 1), x] | v == vOpMul -> atom x
    [_, Lit (Int 0)] | v == vOpMul -> num 0
    [Lit (Int 0), _] | v == vOpMul -> num 0
    [x, Lit (Int 1)] | v == vOpDiv -> atom x

    -- Partial string reductions
    [x, Lit (Str "")] | v == vOpConcat -> atom x
    [Lit (Str ""), x] | v == vOpConcat -> atom x

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
    bool x = atom (Lit (if x then LitTrue else LitFalse))
    atom = Just . Atom

    compareTy t = t == tyBool
               || t == tyInt
               || t == tyString
               || t == tyUnit

-- | Attempt to fold an application to a builtin function with a series
-- of literal values.
foldLiteral :: CoVar -> [Literal] -> Maybe Literal
foldLiteral v xs =
  case xs of
    -- Integer reductions
    [Int l, Int r] | v == vOpAdd -> num (l + r)
    [Int l, Int r] | v == vOpSub -> num (l - r)
    [Int l, Int r] | v == vOpMul -> num (l * r)
    [Int l, Int r] | v == vOpDiv -> num (l `div` r)
    [Int l, Int r] | v == vOpExp -> num (l ^ r)
    [Int l, Int r] | v == vOpEq -> bool (l == r)
    [Int l, Int r] | v == vOpNe -> bool (l /= r)
    [Int l, Int r] | v == vOpLt -> bool (l < r)
    [Int l, Int r] | v == vOpGt -> bool (l > r)
    [Int l, Int r] | v == vOpLe -> bool (l <= r)
    [Int l, Int r] | v == vOpGe -> bool (l >= r)

    -- String reductions
    [Str l, Str r]  | v == vOpConcat -> str (l `T.append` r)
    [Str l, Str r]  | v == vOpEq -> bool (l == r)
    [Str l, Str r]  | v == vOpNe -> bool (l /= r)

    -- Boolean reductions
    [l@LitTrue,  r] | v == vOpEq -> bool (l == r)
    [l@LitFalse, r] | v == vOpEq -> bool (l == r)
    [l@LitTrue,  r] | v == vOpNe -> bool (l /= r)
    [l@LitFalse, r] | v == vOpNe -> bool (l /= r)

    -- Unit reductions
    [Unit, _] | v == vOpEq -> bool True
    [Unit, _] | v == vOpNe -> bool False

    _ -> Nothing
  where
    num = Just . Int
    str = Just . Str
    bool x = Just (if x then LitTrue else LitFalse)
