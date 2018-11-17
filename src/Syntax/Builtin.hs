{-# LANGUAGE OverloadedStrings #-}
module Syntax.Builtin
  ( builtinResolve, builtinModules
  , builtinEnv

  , tyUnitName, tyBoolName, tyIntName, tyStringName, tyFloatName
  , tyLazyName, tyConstraintName, tyArrowName, tyTupleName

  , tyUnit, tyBool, tyInt, tyString, tyFloat
  , tyLazy, tyConstraint, tyArrow

  , forceName, lAZYName, forceTy, lAZYTy, forceTy', lAZYTy'
  , opAppName
  ) where

import qualified Data.Map as Map
import qualified Data.Text ()

import qualified Syntax.Resolve.Scope as R
import qualified Syntax.Types as T

import Syntax.Var
import Syntax

import qualified Core.Builtin as C
import Core.Var

tyUnitName, tyBoolName, tyIntName, tyStringName, tyFloatName, tyLazyName, tyConstraintName, tyArrowName, tyTupleName :: Var Typed
tyIntName    = ofCore C.vInt
tyStringName = ofCore C.vString
tyBoolName   = ofCore C.vBool
tyUnitName   = ofCore C.vUnit
tyFloatName  = ofCore C.vFloat
tyLazyName   = ofCore C.vLazy
tyArrowName  = ofCore C.vArrow
tyTupleName  = ofCore C.vProduct
tyConstraintName = TgInternal "constraint"

tyUnit, tyBool, tyInt, tyString, tyFloat, tyLazy, tyConstraint, tyArrow :: Type Typed
tyInt    = TyCon tyIntName
tyString = TyCon tyStringName
tyBool   = TyCon tyBoolName
tyUnit   = TyCon tyUnitName
tyFloat  = TyCon tyFloatName
tyLazy   = TyCon tyLazyName
tyArrow  = TyCon tyArrowName
tyConstraint = TyCon tyConstraintName

forceName, lAZYName :: Var Typed
forceName = ofCore C.vForce
lAZYName  = ofCore C.vLAZY

forceTy, lAZYTy :: Type Typed
forceTy = a *. TyApp tyLazy (TyVar a) ~> TyVar a
lAZYTy = a *. (tyUnit ~> TyVar a) ~> TyApp tyLazy (TyVar a)

forceTy', lAZYTy' :: Type Typed -> Type Typed
forceTy' x = TyApp tyLazy x ~> x
lAZYTy' x = TyArr tyUnit x ~> TyApp tyLazy x

opAppName :: Var Typed
opAppName = ofCore C.vOpApp

data BuiltinModule = BM
  { vars    :: [(Var Resolved, Type Typed)]
  , types   :: [(Var Resolved, Type Typed)]
  , modules :: [(Var Resolved, BuiltinModule)]
  }

builtins :: BuiltinModule
builtins =
  BM { vars = [ intOp C.vOpAdd, intOp C.vOpSub, intOp C.vOpMul, opOf C.vOpDiv (tyInt ~> tyInt ~> tyFloat), intOp C.vOpExp
              , intCmp C.vOpLt, intCmp C.vOpGt, intCmp C.vOpLe, intCmp C.vOpGe

              , floatOp C.vOpAddF, floatOp C.vOpSubF, floatOp C.vOpMulF, floatOp C.vOpDivF, floatOp C.vOpExpF
              , floatCmp C.vOpLtF, floatCmp C.vOpGtF, floatCmp C.vOpLeF, floatCmp C.vOpGeF

              , stringOp C.vOpConcat

              , cmp C.vOpEq, cmp C.vOpNe
              , (opAppName, a *. b *. (TyVar a ~> TyVar b) ~> TyVar a ~> TyVar b)
              , (lAZYName, lAZYTy), (forceName, forceTy)
              ]

     , types = [ tp C.vBool, tp C.vInt, tp C.vString, tp C.vFloat, tp C.vUnit
               , (tyLazyName, TyType ~> TyType)
               , (tyArrowName, TyType ~> TyType ~> TyType)
               , (tyTupleName, TyType ~> TyType ~> a *. TyVar a)
               , (tyConstraintName, TyType)
               ]
     , modules = []
     }

  where

    -- Helper functions for operators
    opOf x t = (ofCore x, t)
    op t x = (ofCore x, t)

    intOp = op $ tyInt ~> tyInt ~> tyInt
    floatOp = op $ tyFloat ~> tyFloat ~> tyFloat
    stringOp = op $ tyString ~> tyString ~> tyString
    intCmp = op $ tyInt ~> tyInt ~> tyBool
    floatCmp = op $ tyInt ~> tyInt ~> tyBool
    cmp = op $ a *.TyVar a ~> TyVar a ~> tyBool

    -- Helper functions for types
    tp x = (ofCore x, TyType)


builtinResolve :: R.Scope
builtinModules :: R.ModuleScope
(builtinResolve, builtinModules) = go builtins where
  go (BM vs ts _) =
    -- TODO: Handle modules
    ( R.Scope
      { R.varScope = buildVars vs
      , R.tyScope = buildVars ts
      , R.tyvarScope = mempty, R.modStack = mempty }
    , R.ModuleScope mempty )

  buildVars :: [(Var Resolved, Type Typed)] -> Map.Map (Var Parsed) R.ScopeVariable
  buildVars = foldr (\(var, _) -> Map.insert (Name (getName var)) (R.SVar var)) mempty

  getName (TgInternal x) = x
  getName (TgName x _) = x

builtinEnv :: T.Env
builtinEnv = go builtins where
  go (BM vs ts ms) = foldr ((<>) . go . snd) (T.envOf (T.scopeFromList vs <> T.scopeFromList ts)) ms

-- | Construct a syntax variable from a core one
ofCore :: CoVar -> Var Resolved
ofCore (CoVar i n _) = TgName n i

-- Declare some syntactic sugar to make building types easier
(~>) :: Type Typed -> Type Typed -> Type Typed
(~>) = TyArr

(*.) :: Var Typed -> Type Typed -> Type Typed
v *. t = TyForall v (Just TyType) t

infixr ~>
infixr *.

-- Some internal variables
a, b :: Var Resolved
a = ofCore C.tyvarA
b = ofCore C.tyvarB
