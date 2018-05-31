{-# LANGUAGE OverloadedStrings #-}
module Core.Builtin where

import Data.Text ()

import Core.Core
import Core.Var

vBool, vInt, vString, vFloat, vUnit, vLazy :: CoVar
vBool   = CoVar (-1) "bool" TypeConVar
vInt    = CoVar (-2) "int" TypeConVar
vString = CoVar (-3) "string" TypeConVar
vFloat  = CoVar (-4) "float" TypeConVar
vUnit   = CoVar (-5) "unit" TypeConVar
vLazy   = CoVar (-34) "lazy" TypeConVar

tyBool, tyInt, tyString, tyFloat, tyUnit, tyLazy :: IsVar a => Type a
tyBool   = ConTy $ fromVar vBool
tyInt    = ConTy $ fromVar vInt
tyString = ConTy $ fromVar vString
tyUnit   = ConTy $ fromVar vUnit
tyFloat  = ConTy $ fromVar vFloat
tyLazy   = ConTy $ fromVar vLazy

builtinTyList :: IsVar a => [a]
builtinTyList = [ fromVar vBool
                , fromVar vInt
                , fromVar vString
                , fromVar vUnit
                , fromVar vFloat
                , fromVar vLazy
                ]

vOpAdd, vOpSub, vOpMul, vOpDiv, vOpExp :: CoVar
vOpAdd = CoVar (-6) "+" ValueVar
vOpSub = CoVar (-7) "-" ValueVar
vOpMul = CoVar (-8) "*" ValueVar
vOpDiv = CoVar (-9) "/" ValueVar
vOpExp = CoVar (-10) "**" ValueVar

vOpLt, vOpGt, vOpLe, vOpGe :: CoVar
vOpLt = CoVar (-11) "<" ValueVar
vOpGt = CoVar (-12) ">" ValueVar
vOpLe = CoVar (-13) "<=" ValueVar
vOpGe = CoVar (-14) ">=" ValueVar

vOpAddF, vOpSubF, vOpMulF, vOpDivF, vOpExpF :: CoVar
vOpAddF = CoVar (-15) "+." ValueVar
vOpSubF = CoVar (-16) "-." ValueVar
vOpMulF = CoVar (-17) "*." ValueVar
vOpDivF = CoVar (-18) "/." ValueVar
vOpExpF = CoVar (-19) "**." ValueVar

vOpLtF, vOpGtF, vOpLeF, vOpGeF :: CoVar
vOpLtF = CoVar (-20) "<." ValueVar
vOpGtF = CoVar (-21) ">." ValueVar
vOpLeF = CoVar (-22) "<=." ValueVar
vOpGeF = CoVar (-23) ">=." ValueVar

vOpConcat :: CoVar
vOpConcat = CoVar (-24) "^" ValueVar

vOpEq, vOpNe :: CoVar
vOpEq = CoVar (-25) "==" ValueVar
vOpNe = CoVar (-26) "<>" ValueVar

vOpAnd, vOpOr :: CoVar
vOpAnd = CoVar (-27) "||" ValueVar
vOpOr  = CoVar (-28) "&&" ValueVar

vError :: CoVar
vError = CoVar (-29) "error" ValueVar

vLAZY, vForce :: CoVar
vLAZY = CoVar (-35) "Lazy" ValueVar
vForce = CoVar (-36) "force" ValueVar

tyvarA, tyvarB :: CoVar
tyvarA = CoVar (-30) "a" TypeVar
tyvarB = CoVar (-31) "b" TypeVar

builtinFst, builtinSnd :: CoVar
builtinFst = CoVar (-32) "fst" ValueVar
builtinSnd = CoVar (-33) "snd" ValueVar

builtinVarList :: (IsVar a, IsVar b) => [(a, Type b)]
builtinVarList = vars where
  op x t = (fromVar x, t)

  arrTy = ForallTy Irrelevant
  boolOp = ForallTy Irrelevant tyBool (ForallTy Irrelevant tyBool tyBool)
  intOp = tyInt `arrTy` (tyInt `arrTy` tyInt)
  floatOp = tyFloat `arrTy` (tyFloat `arrTy` tyFloat)
  stringOp = tyString `arrTy` (tyString `arrTy` tyString)
  intCmp = tyInt `arrTy` (tyInt `arrTy` tyBool)
  floatCmp = tyInt `arrTy` (tyInt `arrTy` tyBool)

  cmp = ForallTy (Relevant name) StarTy $ VarTy name `arrTy` (VarTy name `arrTy` tyBool)

  name = fromVar tyvarA

  vars = [ op vOpAdd intOp, op vOpSub intOp, op vOpMul intOp, op vOpDiv intOp, op vOpExp intOp
         , op vOpLt intCmp, op vOpGt intCmp, op vOpLe intCmp, op vOpGe intCmp

         , op vOpAddF floatOp, op vOpSubF floatOp, op vOpMulF floatOp, op vOpDivF floatOp, op vOpExpF floatOp
         , op vOpLtF floatCmp, op vOpGtF floatCmp, op vOpLeF floatCmp, op vOpGeF floatCmp

         , op vOpConcat stringOp

         , op vOpEq cmp, op vOpNe cmp
         , op vOpOr boolOp, op vOpAnd boolOp

         , (fromVar vError, ForallTy (Relevant name) StarTy $ tyString `arrTy` VarTy name)
         , (fromVar vLAZY, ForallTy (Relevant name) StarTy $ (tyUnit `arrTy` VarTy name) `arrTy` AppTy tyLazy (VarTy name))
         , (fromVar vForce, ForallTy (Relevant name) StarTy $ AppTy tyLazy (VarTy name) `arrTy` VarTy name)
        ]

isError :: IsVar a => a -> Bool
isError = (==vError) . toVar
