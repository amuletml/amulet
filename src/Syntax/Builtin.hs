{-# LANGUAGE OverloadedStrings #-}
module Syntax.Builtin
  ( builtinResolve, builtinModules
  , builtinEnv

  , tyUnitName, tyBoolName, tyIntName, tyStringName, tyFloatName
  , tyLazyName, tyConstraintName, tyArrowName, tyTupleName

  , tyUnit, tyBool, tyInt, tyString, tyFloat
  , tyLazy, tyConstraint, tyArrow

  , forceName, lAZYName, forceTy, lAZYTy, forceTy', lAZYTy'
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
tyIntName = TgInternal "int"
tyStringName = TgInternal "string"
tyBoolName = TgInternal "bool"
tyUnitName = TgInternal "unit"
tyFloatName = TgInternal "float"
tyLazyName = ofCore C.vLazy
tyConstraintName = TgName "constraint" (-37)
tyArrowName = ofCore C.vArrow
tyTupleName = ofCore C.vProduct

tyUnit, tyBool, tyInt, tyString, tyFloat, tyLazy, tyConstraint, tyArrow :: Type Typed
tyInt = TyCon tyIntName
tyString = TyCon tyStringName
tyBool = TyCon tyBoolName
tyUnit = TyCon tyUnitName
tyFloat = TyCon tyFloatName
tyLazy = TyCon tyLazyName
tyConstraint = TyCon tyConstraintName
tyArrow = TyCon tyArrowName

forceName, lAZYName :: Var Typed
forceName = TgInternal "force"
lAZYName = TgInternal "lazy"

forceTy, lAZYTy :: Type Typed
forceTy = a *. TyApp tyLazy (TyVar a) ~> TyVar a
lAZYTy = a *. (tyUnit ~> TyVar a) ~> TyApp tyLazy (TyVar a)

forceTy', lAZYTy' :: Type Typed -> Type Typed
forceTy' x = TyApp tyLazy x ~> x
lAZYTy' x = TyArr tyUnit x ~> TyApp tyLazy x

data BuiltinModule = BM
  { vars    :: [(Var Resolved, Type Typed)]
  , types   :: [(Var Resolved, Type Typed)]
  , modules :: [(Var Resolved, BuiltinModule)]
  }

builtins :: BuiltinModule
builtins =
  BM { vars = [ intOp "+", intOp "-", intOp "*", opOf "/" (tyInt ~> tyInt ~> tyFloat), intOp "**"
              , intCmp "<", intCmp ">", intCmp ">=", intCmp "<="

              , floatOp "+.", floatOp "-.", floatOp "*.", floatOp "/.", floatOp "**."
              , floatCmp "<.", floatCmp ">.", floatCmp ">=.", floatCmp "<=."

              , stringOp "^"

              , cmp "==", cmp "<>"
              , opOf "@@" $ a *. b *. (TyVar a ~> TyVar b) ~> TyVar a ~> TyVar b

              , (lAZYName, lAZYTy), (forceName, forceTy)
              ]

     , types = [ tp "int", tp "string", tp "bool", tp "unit", tp "float"

               , (tyArrowName, TyType ~> TyType ~> TyType)
               , (tyTupleName, TyType ~> TyType ~> a *. TyVar a)
               , (tyLazyName, TyType ~> TyType)
               , (tyConstraintName, TyType)
               ]
     , modules = []
     }

  where

    -- Helper functions for operators
    opOf x t = (TgInternal x, t)
    op t x = (TgInternal x, t)

    intOp = op $ tyInt ~> tyInt ~> tyInt
    floatOp = op $ tyFloat ~> tyFloat ~> tyFloat
    stringOp = op $ tyString ~> tyString ~> tyString
    intCmp = op $ tyInt ~> tyInt ~> tyBool
    floatCmp = op $ tyInt ~> tyInt ~> tyBool
    cmp = op $ a *.TyVar a ~> TyVar a ~> tyBool

    -- Helper functions for types
    tp x = (TgInternal x, TyType)


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
a = TgInternal "a"
b = TgInternal "b"
