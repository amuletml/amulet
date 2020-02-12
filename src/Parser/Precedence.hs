{-# LANGUAGE FlexibleContexts, OverloadedStrings, TypeFamilies  #-}

{-| Handles rewriting expression and type trees in order with the correct
  precedence.

  As the parser must be able to cope with arbitrary operators, we cannot
  hard-code precedence into it. Therefore we fix any binary operators
  after parsing them.
-}
module Parser.Precedence
  ( fixupExpr
  , fixupType
  ) where

import qualified Data.Text as T
import Data.Spanned
import Data.Span

import Syntax.Var
import Syntax

data Associativity = AssocLeft | AssocRight
  deriving (Eq, Show)

-- | Fix the given binary operator, rewriting the expression tree so
-- binary operators are grouped with the correct precedence and
-- associativity.
--
-- Note, this does not recurse into nested sub-trees - we only handle one
-- level here.
fixupExpr :: Expr Parsed -> Expr Parsed
fixupExpr o@BinOp{} =
  let (es, os) = go [] [] o
      ([x], []) = popUntil es os 0 AssocLeft
  in x

  where
    go es ops (BinOp lhs o@(VarRef o' _) rhs _) =
      let (es', ops') = go es ops lhs
          (pre, ass) = precedenceOf precedence o'
          (es'', ops'') = popUntil es' ops' pre ass
      in go es'' ((o, pre):ops'') rhs
    go es ops e = (e:es, ops)

    popUntil es ((sop, spre):os) opre assoc
      | (assoc == AssocLeft && spre >= opre) || (assoc == AssocRight && spre > opre)
      = let (right:left:es') = es
        in popUntil ( BinOp left sop right
                            (mkSpanUnsafe (spanStart (annotation left)) (spanEnd (annotation right)))
                    : es' ) os opre assoc
    popUntil es os _ _ = (es, os)
fixupExpr o = o

-- | Fix the given type operator, rewriting the type tree so binary
-- operators are grouped with the correct precedence and associativity.
fixupType :: Type Parsed -> Type Parsed
fixupType o@TyOperator{} =
  let (es, os) = go [] [] o
      ([x], []) = popUntil es os 0 AssocLeft
  in x

  where
    go :: [Type Parsed] -> [(Type Parsed, Int)] -> Type Parsed -> ([Type Parsed], [(Type Parsed, Int)])
    go es ops (TyOperator lhs o@(TyCon v _) rhs) =
      let (es', ops') = go es ops lhs
          (pre, ass) = precedenceOf tyPrecedence v
          (es'', ops'') = popUntil es' ops' pre ass
      in go es'' ((o, pre):ops'') rhs
    go es ops e = (e:es, ops)

    popUntil es ((sop, spre):os) opre assoc
      | (assoc == AssocLeft && spre >= opre) || (assoc == AssocRight && spre > opre)
      = let (right:left:es') = es
        in popUntil (TyOperator left sop right:es') os opre assoc
    popUntil es os _ _ = (es, os)
fixupType o = o

-- | Lookup the precedence of some name, using the provided precedence
-- table.
precedenceOf :: (T.Text -> (Int, Associativity)) -> Var Parsed -> (Int, Associativity)
precedenceOf f (Name n) = f n
precedenceOf f (InModule _ n) = precedenceOf f n

-- | The default precedence "table", used by expressions and (mostly) by
-- types.
precedence :: T.Text -> (Int, Associativity)
precedence t
  | T.isPrefixOf "@@" t = (2, AssocRight)
  | T.isPrefixOf "**" t = (10, AssocRight)


  | T.isPrefixOf "*" t = (9, AssocLeft)
  | T.isPrefixOf "/" t = (9, AssocLeft)
  | T.isPrefixOf "%" t = (9, AssocLeft)

  | T.isPrefixOf "+" t = (8, AssocLeft)
  | T.isPrefixOf "-" t = (8, AssocLeft)

  | T.isPrefixOf ":" t = (7, AssocRight)

  | T.isPrefixOf "@" t = (6, AssocRight)
  | T.isPrefixOf "^" t = (6, AssocRight)

  | T.isPrefixOf "=" t = (5, AssocLeft)
  | T.isPrefixOf "!" t = (5, AssocLeft)
  | T.isPrefixOf "<" t = (5, AssocLeft)
  | T.isPrefixOf ">" t = (5, AssocLeft)

  | T.isPrefixOf "&&" t = (4, AssocLeft)
  | T.isPrefixOf "||" t = (3, AssocLeft)

  | T.isPrefixOf "&" t = (5, AssocLeft)
  | T.isPrefixOf "|" t = (5, AssocLeft)

  | otherwise = (11, AssocLeft)

-- | The precedence tables for types. This merely makes @*@ right
-- associative.
tyPrecedence :: T.Text -> (Int, Associativity)
tyPrecedence t
  | T.isPrefixOf "*" t  = (9, AssocRight)
  | otherwise = precedence t
