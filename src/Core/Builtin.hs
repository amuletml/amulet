{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

-- | This monster of a module declares any built-in variables, or
-- variables the compiler has intrinsic knowledge of.
--
-- Any "internal" variable has a negative index which, like other
-- indexes, should be unique.
module Core.Builtin where

import Data.Text (Text)

import Core.Core
import Core.Var

vBool, vInt, vString, vFloat, vUnit, vLazy, vArrow, vProduct, vList, vRefTy, vKStrTy, vKIntTy, vRowCons :: CoVar
vOpAdd, vOpSub, vOpMul, vOpDiv, vOpExp :: CoVar
vOpLt, vOpGt, vOpLe, vOpGe :: CoVar
vOpAddF, vOpSubF, vOpMulF, vOpDivF, vOpExpF :: CoVar
vOpLtF, vOpGtF, vOpLeF, vOpGeF :: CoVar
vOpConcat :: CoVar
vOpEq, vOpNe :: CoVar
vError :: CoVar
vLAZY, vForce :: CoVar
tyvarA, tyvarB, argvarX :: CoVar
vOpApp :: CoVar
vCONS, vNIL :: CoVar
vAssign, vDeref, vRef :: CoVar
vStrVal, vIntVal :: CoVar
vKSTR, vKINT :: CoVar
vExtend, vRestrict, vROWCONS :: CoVar
tyvarRecord, tyvarNew, tyvarKey, tyvarType :: CoVar
backendRet, backendClone :: CoVar
vEq, vEQ :: CoVar

[ vBool, vInt, vString, vFloat, vUnit, vLazy, vArrow, vProduct, vList, vRefTy, vKStrTy, vKIntTy, vRowCons, vOpAdd, vOpSub, vOpMul, vOpDiv, vOpExp, vOpLt, vOpGt, vOpLe, vOpGe, vOpAddF, vOpSubF, vOpMulF, vOpDivF, vOpExpF, vOpLtF, vOpGtF, vOpLeF, vOpGeF, vOpConcat, vOpEq, vOpNe, vError, vLAZY, vForce, tyvarA, tyvarB, argvarX, vOpApp, vCONS, vNIL, vAssign, vDeref, vRef, vStrVal, vIntVal, vExtend, vRestrict, vKSTR, vKINT, vROWCONS, tyvarRecord, tyvarNew, tyvarKey, tyvarType, vEq, vEQ, backendRet, backendClone ] = makeBuiltins
  [ ("bool", TypeConVar)
  , ("int", TypeConVar)
  , ("string", TypeConVar)
  , ("float", TypeConVar)
  , ("unit", TypeConVar)
  , ("lazy", TypeConVar)
  , ("->", TypeConVar)
  , ("*", TypeConVar)
  , ("list", TypeConVar)
  , ("ref", TypeConVar)
  , ("known_string", TypeConVar)
  , ("known_int", TypeConVar)
  , ("row_cons", TypeConVar)

  , ("+", ValueVar)
  , ("-", ValueVar)
  , ("*", ValueVar)
  , ("/", ValueVar)
  , ("**", ValueVar)

  , ("<", ValueVar)
  , (">", ValueVar)
  , ("<=", ValueVar)
  , (">=", ValueVar)

  , ("+.", ValueVar)
  , ("-.", ValueVar)
  , ("*.", ValueVar)
  , ("/.", ValueVar)
  , ("**.", ValueVar)

  , ("<.", ValueVar)
  , (">.", ValueVar)
  , ("<=.", ValueVar)
  , (">=.", ValueVar)

  , ("^", ValueVar)

  , ("==", ValueVar)
  , ("<>", ValueVar)

  , ("error", ValueVar)

  , ("lazy", ValueVar)
  , ("force", ValueVar)

  , ("a", TypeVar)
  , ("b", TypeVar)
  , ("x", ValueVar)

  , ("@@", ValueVar)

-- Lists
  , ("Cons", DataConVar)
  , ("Nil", DataConVar)

-- References
  , (":=", ValueVar)
  , ("!", ValueVar)
  , ("ref", ValueVar)

-- Type:fire: classes
  , ("string_value", ValueVar)
  , ("int_value", ValueVar)
  , ("extend_row", ValueVar)
  , ("restrict_row", ValueVar)

  , ("$KnownString", ValueVar)
  , ("$KnownInt", ValueVar)
  , ("$RowCons", ValueVar)

  , ("record", TypeVar)
  , ("new", TypeVar)
  , ("key", TypeVar)
  , ("type", TypeVar)
  , ("~", TypeConVar)
  , ("$Refl", ValueVar)

  -- Backend specific variables
  , ("<ret>", ValueVar)
  , ("<clone>", ValueVar)
  ]

tyBool, tyInt, tyString, tyFloat, tyUnit, tyLazy, tyList, tyRef, tyKStr, tyKInt, tyRowCons, tyEq :: IsVar a => Type a
tyBool    = ConTy $ fromVar vBool
tyInt     = ConTy $ fromVar vInt
tyString  = ConTy $ fromVar vString
tyUnit    = ConTy $ fromVar vUnit
tyFloat   = ConTy $ fromVar vFloat
tyLazy    = ConTy $ fromVar vLazy
tyList    = ConTy $ fromVar vList
tyRef     = ConTy $ fromVar vRefTy
tyKStr    = ConTy $ fromVar vKStrTy
tyKInt    = ConTy $ fromVar vKIntTy
tyRowCons = ConTy $ fromVar vRowCons
tyEq      = ConTy $ fromVar vEq

builtinTyList :: IsVar a => [a]
builtinTyList = [ fromVar vBool
                , fromVar vInt
                , fromVar vString
                , fromVar vUnit
                , fromVar vFloat
                , fromVar vLazy
                , fromVar vArrow
                , fromVar vProduct
                , fromVar vList
                , fromVar vRefTy
                , fromVar vKStrTy
                , fromVar vKIntTy
                , fromVar vRowCons
                , fromVar vEq
                ]

builtinVarList :: forall a b. (IsVar a, IsVar b) => [(a, Type b)]
builtinVarList = vars where
  op x t = (fromVar x, t)

  tupTy = ValuesTy
  arrTy = ForallTy Irrelevant
  prodTy a b = RowsTy NilTy [("_1", a), ("_2", b)]

  intOp, floatOp, stringOp, intCmp, floatCmp :: Type b
  intOp = tupTy [tyInt, tyInt] `arrTy` tyInt
  floatOp = tupTy [tyFloat, tyFloat] `arrTy` tyFloat
  stringOp = tupTy [tyString, tyString] `arrTy` tyString
  intCmp = tupTy [tyInt, tyInt] `arrTy` tyBool
  floatCmp = tupTy [tyFloat, tyFloat] `arrTy` tyBool

  cmp = ForallTy (Relevant name) StarTy $ tupTy [VarTy name, VarTy name] `arrTy` tyBool

  name, name', record, ttype, key, new :: b
  name = fromVar tyvarA
  name' = fromVar tyvarB
  record = fromVar tyvarRecord
  ttype = fromVar tyvarType
  key = fromVar tyvarKey
  new = fromVar tyvarNew

  appsTy :: [Type b] -> Type b
  appsTy = foldl1 AppTy

  vars :: [(a, Type b)]
  vars = [ op vOpAdd intOp, op vOpSub intOp
         , op vOpMul intOp, op vOpDiv (tupTy [tyInt, tyInt] `arrTy` tyFloat), op vOpExp intOp
         , op vOpLt intCmp, op vOpGt intCmp, op vOpLe intCmp, op vOpGe intCmp

         , op vOpAddF floatOp, op vOpSubF floatOp, op vOpMulF floatOp, op vOpDivF floatOp, op vOpExpF floatOp
         , op vOpLtF floatCmp, op vOpGtF floatCmp, op vOpLeF floatCmp, op vOpGeF floatCmp

         , op vOpConcat stringOp

         , op vOpEq cmp, op vOpNe cmp
         , op vOpApp
            (ForallTy (Relevant name) StarTy $
               ForallTy (Relevant name') StarTy $
                 ValuesTy [VarTy name `arrTy` VarTy name', VarTy name] `arrTy` VarTy name')

         , op vError (ForallTy (Relevant name) StarTy $ tyString `arrTy` VarTy name)
         , op vLAZY (ForallTy (Relevant name) StarTy $
            (tyUnit `arrTy` VarTy name) `arrTy` AppTy tyLazy (VarTy name))
         , op vForce (ForallTy (Relevant name) StarTy $ AppTy tyLazy (VarTy name) `arrTy` VarTy name)
         , op vCONS (ForallTy (Relevant name) StarTy $
                      VarTy name `prodTy` AppTy tyList (VarTy name) `arrTy` AppTy tyList (VarTy name))
         , op vNIL (ForallTy (Relevant name) StarTy $ AppTy tyList (VarTy name))
         , op vRef (ForallTy (Relevant name) StarTy $ VarTy name `arrTy` AppTy tyRef (VarTy name))
         , op vAssign (ForallTy (Relevant name) StarTy $ ValuesTy [AppTy tyRef (VarTy name), VarTy name] `arrTy` tyUnit)
         , op vDeref (ForallTy (Relevant name) StarTy $ AppTy tyRef (VarTy name) `arrTy` VarTy name)

         , op vStrVal (ForallTy (Relevant name) tyString $ AppTy tyKStr (VarTy name) `arrTy` tyString)
         , op vKSTR (ForallTy (Relevant name) tyString $ tyString `arrTy` AppTy tyKStr (VarTy name))

         , op vIntVal (ForallTy (Relevant name) tyInt $ AppTy tyKInt (VarTy name) `arrTy` tyInt)
         , op vKINT (ForallTy (Relevant name) tyInt $ tyInt `arrTy` AppTy tyKInt (VarTy name))

         , op vExtend $
             ForallTy (Relevant key) tyString $
               ForallTy (Relevant record) StarTy $
                 ForallTy (Relevant ttype) StarTy $
                   ForallTy (Relevant new) StarTy $
                     tupTy
                      [ appsTy [tyRowCons, VarTy record, VarTy key, VarTy ttype, VarTy new ]
                      , VarTy ttype
                      , VarTy record ]
                      `arrTy` VarTy new

         , op vRestrict $
             ForallTy (Relevant key) tyString $
               ForallTy (Relevant record) StarTy $
                 ForallTy (Relevant ttype) StarTy $
                   ForallTy (Relevant new) StarTy $
                     tupTy
                      [ appsTy [tyRowCons, VarTy record, VarTy key, VarTy ttype, VarTy new ]
                      , VarTy new ]
                      `arrTy` ExactRowsTy [ ("_1", VarTy ttype), ("_2", VarTy record) ]

         , op vROWCONS $
            ForallTy (Relevant key) tyString $
              ForallTy (Relevant record) StarTy $
                ForallTy (Relevant ttype) StarTy $
                  ForallTy (Relevant new) StarTy $
                    tyString `arrTy` appsTy [tyRowCons, VarTy record, VarTy ttype, VarTy key, VarTy new ]
         , op vEQ $ ForallTy (Relevant name) StarTy $ appsTy [ tyEq, VarTy name, VarTy name ] 
         ]

isError :: IsVar a => a -> Bool
isError = (==vError) . toVar

fakeStrVal, fakeKSTR, fakeIntVal, fakeKINT :: IsVar a => Term a
fakeStrVal =
  Lam (TypeArgument (fromVar tyvarA) tyString) $
    Lam (TermArgument (fromVar argvarX) (AppTy tyKStr (VarTy (fromVar tyvarA)))) $
      Cast (Ref (fromVar argvarX) (AppTy tyKStr (VarTy (fromVar tyvarA))))
        (SameRepr (AppTy tyKStr (VarTy (fromVar tyvarA))) tyString)

fakeKSTR =
  Lam (TypeArgument (fromVar tyvarA) tyString) $
    Lam (TermArgument (fromVar argvarX) (AppTy tyKStr (VarTy (fromVar tyvarA)))) $
      Cast (Ref (fromVar argvarX) (AppTy tyKStr (VarTy (fromVar tyvarA))))
        (Symmetry (SameRepr (AppTy tyKStr (VarTy (fromVar tyvarA))) tyString))

fakeIntVal =
  Lam (TypeArgument (fromVar tyvarA) tyInt) $
    Lam (TermArgument (fromVar argvarX) (AppTy tyKInt (VarTy (fromVar tyvarA)))) $
      Cast (Ref (fromVar argvarX) (AppTy tyKInt (VarTy (fromVar tyvarA))))
        (SameRepr (AppTy tyKInt (VarTy (fromVar tyvarA))) tyInt)

fakeKINT =
  Lam (TypeArgument (fromVar tyvarA) tyInt) $
    Lam (TermArgument (fromVar argvarX) (AppTy tyKInt (VarTy (fromVar tyvarA)))) $
      Cast (Ref (fromVar argvarX) (AppTy tyKInt (VarTy (fromVar tyvarA))))
        (Symmetry (SameRepr (AppTy tyKInt (VarTy (fromVar tyvarA))) tyInt))

makeBuiltins :: [ (Text, VarInfo) ] -> [CoVar]
makeBuiltins xs = zipWith go xs [-1, -2 ..] where
  go (name, t) id = CoVar id name t
