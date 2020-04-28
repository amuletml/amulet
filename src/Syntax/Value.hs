{-# LANGUAGE TypeFamilies #-}
module Syntax.Value where

import Control.Lens hiding (Lazy, (:>))

import qualified Data.Text as T
import Data.Char

import Syntax.Builtin
import Syntax.Expr
import Syntax.Var

value :: Var a ~ Var Resolved => Expr a -> Bool
value Fun{} = True
value Literal{} = True
value Function{} = True
value (Let _ bs e _) = value e && all valueb bs where
  valueb x = value (x ^. bindBody)
value (Parens e _) = value e
value (Tuple es _) = all value es
value (Begin es _) = all value es
value Lazy{} = True
value TupleSection{} = True
value (Record rs _) = all (value . view fExpr) rs
value (RecordExt e rs _) = value e && all (value . view fExpr) rs
value VarRef{} = True
value (If c t e _) = all value [c, t, e]
value (App f x _) = value x && conVarRef f
value (Match e [] _ _) = value e
value Match{} = False
value BinOp{} = False
value Hole{} = True
value (Ascription e _ _) = value e
value (Vta e _ _) = value e
value (Access e _ _) = value e
value LeftSection{} = True
value RightSection{} = True
value BothSection{} = True
value AccessSection{} = True
value (ListExp xs _) = all value xs
value ListComp{} = False
value ListFrom{} = False
value ListFromTo{} = False
value ListFromThen{} = False
value ListFromThenTo{} = False
value Idiom{} = False
value MLet{} = False
value (OpenIn _ e _) = value e
value (ExprWrapper _ e _) = value e

isFn :: Expr a -> Bool
isFn Fun{} = True
isFn (OpenIn _ e _) = isFn e
isFn (Ascription e _ _) = isFn e
isFn (ExprWrapper _ e _) = isFn e
isFn _ = False

conVarRef :: Var a ~ Var Resolved => Expr a -> Bool
conVarRef (VarRef t _) =
  case t of
    TgName n _ -> t == lAZYName || isUpper (T.head (dropModPrefixes n))
    TgInternal t -> isUpper (T.head t)
conVarRef (Begin [x] _) = conVarRef x
conVarRef (If c t e _) = value c && all conVarRef [t, e]
conVarRef (Ascription e _ _) = conVarRef e
conVarRef (OpenIn _ e _) = conVarRef e
conVarRef (ExprWrapper _ e _) = conVarRef e
conVarRef (Vta e _ _) = conVarRef e
conVarRef MLet{} = False
conVarRef _ = False

dropModPrefixes :: T.Text -> T.Text
dropModPrefixes = last . T.splitOn (T.singleton '.')
