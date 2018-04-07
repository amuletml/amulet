{-# LANGUAGE OverloadedStrings #-}
module Core.Builtin where

import Data.VarSet (IsVar(..))
import Data.Text ()

import Syntax (Var(..))
import Core.Core

tyBool, tyInt, tyString, tyFloat, tyUnit :: IsVar a => Type a
tyBool = ConTy $ fromVar $ TgInternal "bool"
tyInt = ConTy $ fromVar $ TgInternal "int"
tyString = ConTy $ fromVar $ TgInternal "string"
tyUnit = ConTy $ fromVar $ TgInternal "unit"
tyFloat = ConTy $ fromVar $ TgInternal "float"

builtinTyList :: IsVar a => [a]
builtinTyList = map (fromVar . TgInternal) [ "int"
                                           , "bool"
                                           , "string"
                                           , "float"
                                           , "unit" ]

builtinVarList :: (IsVar a, IsVar b) => [(a, Type b)]
builtinVarList = vars where
  op x t = (fromVar (TgInternal x), t)

  boolOp = tyBool `ArrTy` (tyBool `ArrTy` tyBool)
  intOp = tyInt `ArrTy` (tyInt `ArrTy` tyInt)
  floatOp = tyFloat `ArrTy` (tyFloat `ArrTy` tyFloat)
  stringOp = tyString `ArrTy` (tyString `ArrTy` tyString)
  intCmp = tyInt `ArrTy` (tyInt `ArrTy` tyBool)
  floatCmp = tyInt `ArrTy` (tyInt `ArrTy` tyBool)

  cmp = ForallTy name $ VarTy name `ArrTy` (VarTy name `ArrTy` tyBool)

  name = fromVar (TgInternal "a")

  vars = [ op "+" intOp, op "-" intOp, op "*" intOp, op "/" intOp, op "**" intOp
         , op "+." floatOp, op "-." floatOp, op "*." floatOp, op "/." floatOp, op "**." floatOp
         , op "^" stringOp
         , op "<" intCmp, op ">" intCmp, op ">=" intCmp, op "<=" intCmp
         , op "<." floatCmp, op ">." floatCmp, op ">=." floatCmp, op "<=." floatCmp
         , op "==" cmp, op "<>" cmp
         , op "||" boolOp, op "&&" boolOp

        , (fromVar (TgInternal "error"), ForallTy name $ tyString `ArrTy` VarTy name)
        ]
