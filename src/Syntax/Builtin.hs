{-# LANGUAGE OverloadedStrings #-}

{-| A shared source of builtin varibles for the resolver and type checker.

This largely defines variables in terms of those defined in
"Core.Builtin". We use 'TgName', along with core's variable IDs in order
to make lowering easier, and ensure core and syntax remain consistent.

For variables which will be removed by the time lowering occurs (such as
TC or resolver specific names), one may use 'TgInternal'.
-}
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

instance Semigroup BuiltinModule where
  (BM v t m) <> (BM v' t' m') = BM (v <> v') (t <> t') (m <> m')

instance Monoid BuiltinModule where
  mempty = BM { vars = mempty, types = mempty, modules = mempty }

builtins :: BuiltinModule
builtins =
  mempty
  { vars = [ intOp C.vOpAdd, intOp C.vOpSub, intOp C.vOpMul, opOf C.vOpDiv (tyInt ~> tyInt ~> tyFloat), intOp C.vOpExp
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

-- | The builtin scope and module list for the resolver
builtinResolve :: R.Scope
builtinModules :: R.ModuleScope
(builtinResolve, builtinModules) = R.ModuleScope <$> go builtins where
  go :: BuiltinModule -> (R.Scope, Map.Map (Var Parsed) (Var Resolved, R.Scope))
  go (BM vs ts ms) =
    let scp = R.Scope
              { R.varScope = buildVars vs
              , R.tyScope = buildVars ts
              , R.tyvarScope = mempty, R.modStack = mempty }

    in foldr (\(n, mod) (scp, ms) ->
                let (scp', ms') = go mod
                    n' = getName n
                in ( R.Scope
                     { R.varScope = R.varScope scp <> nest n' (R.varScope scp')
                     , R.tyScope  = R.tyScope scp  <> nest n' (R.tyScope scp')
                     , R.tyvarScope = mempty, R.modStack = mempty
                     }
                   , ms <> nest n' ms' <> Map.singleton (Name n') (n, scp')) )
       (scp, mempty) ms

  buildVars :: [(Var Resolved, Type Typed)] -> Map.Map (Var Parsed) R.ScopeVariable
  buildVars = foldr (\(var, _) -> Map.insert (Name (getName var)) (R.SVar var)) mempty

  nest n = Map.mapKeys (InModule n)

  getName (TgInternal x) = x
  getName (TgName x _) = x

-- | The builtin scope for type inference
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

-- | Some internal type variables. These do not need to be unique as they
-- are bound by a forall.
a, b :: Var Resolved
a = ofCore C.tyvarA
b = ofCore C.tyvarB
