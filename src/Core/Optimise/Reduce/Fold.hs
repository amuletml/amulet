{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Core.Optimise.Reduce.Fold
  ( foldApply
  ) where

import qualified Data.Text as T

import Core.Optimise.Reduce.Base
import Core.Intrinsic

-- | Apply constant folding an application of some arbitrary function.
--
-- Note the function provided must be the "raw" variant (not applied to
-- any types).
foldApply :: Intrinsic -> [Atom] -> Maybe (Term a)
foldApply i [Lit l, Lit r] = foldLiteral i [l, r]

-- Partial integer reductions
foldApply IntAdd [x, Lit (Int 0)] = atom x
foldApply IntAdd [Lit (Int 0), x] = atom x
foldApply IntSub [x, Lit (Int 0)] = atom x
foldApply IntMul [x, Lit (Int 1)] = atom x
foldApply IntMul [Lit (Int 1), x] = atom x
foldApply IntMul [_, Lit (Int 0)] = int 0
foldApply IntMul [Lit (Int 0), _] = int 0
foldApply IntDiv [x, Lit (Int 1)] = atom x
foldApply IntMod [x, Lit (Int 1)] = atom x
foldApply IntEq  [Ref l _, Ref r _] | l == r = bool True

-- We don't apply similar reductions for floats. Given the presence of
-- NaN, most folds aren't safe.

-- Partial string reductions
foldApply StrConcat [x, Lit (Str "")] = atom x
foldApply StrConcat [Lit (Str ""), x] = atom x
foldApply StrEq     [Ref l _, Ref r _] | l == r = bool True

foldApply _ _ = Nothing

-- | Attempt to fold an application to a builtin function with a series
-- of literal values.
foldLiteral :: Intrinsic -> [Literal] -> Maybe (Term a)

-- Integer reductions
foldLiteral IntAdd [Int l, Int r] = int (l + r)
foldLiteral IntSub [Int l, Int r] = int (l - r)
foldLiteral IntMul [Int l, Int r] = int (l * r)
foldLiteral IntDiv [Int l, Int r] = float (fromIntegral l / fromIntegral r)
foldLiteral IntPow [Int l, Int r] = float (fromIntegral l ** fromIntegral r)
foldLiteral IntMod [Int l, Int r] = int (l `mod` r)
foldLiteral IntEq  [Int l, Int r] = bool (l == r)
foldLiteral IntLt  [Int l, Int r] = bool (l < r)
foldLiteral IntLe  [Int l, Int r] = bool (l <= r)

foldLiteral FloatAdd [Float l, Float r] = float (l + r)
foldLiteral FloatSub [Float l, Float r] = float (l - r)
foldLiteral FloatMul [Float l, Float r] = float (l * r)
foldLiteral FloatDiv [Float l, Float r] = float (l / r)
foldLiteral FloatPow [Float l, Float r] = float (l ** r)
foldLiteral FloatEq  [Float l, Float r] = bool (l == r)
foldLiteral FloatLt  [Float l, Float r] = bool (l < r)
foldLiteral FloatLe  [Float l, Float r] = bool (l <= r)

-- String reductions
foldLiteral StrConcat [Str l, Str r] = str (l `T.append` r)
foldLiteral StrEq     [Str l, Str r] = bool (l == r)
foldLiteral StrLt     [Str l, Str r] = bool (l < r)
foldLiteral StrLe     [Str l, Str r] = bool (l <= r)

foldLiteral BoolEq    [l, r] = bool (l == r)

foldLiteral _ _ = Nothing

int :: Integer -> Maybe (Term a)
int = atom . Lit . Int

str :: T.Text -> Maybe (Term a)
str = atom . Lit . Str

float :: Double -> Maybe (Term a)
float = atom . Lit . Float

bool :: Bool -> Maybe (Term a)
bool x =
  let val = if x then LitTrue else LitFalse
  in val `seq` atom (Lit val)

atom :: Atom -> Maybe (Term a)
atom = Just . Atom
