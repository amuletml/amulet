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
  , tyShowName

  , tyUnit, tyBool, tyInt, tyString, tyFloat
  , tyLazy, tyConstraint, tyArrow, tyList
  , tyShow

  , forceName, lAZYName, forceTy, lAZYTy, forceTy', lAZYTy', showName, showTy

  , cONSName, nILName, cONSTy, nILTy, cONSTy', nILTy'
  , sHOWName, sHOWTy
  , opAppName
  ) where

import Control.Lens

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text ()

import Data.Span (internal)

import qualified Syntax.Resolve.Scope as R
import qualified Syntax.Types as T

import Syntax.Var
import Syntax

import qualified Core.Builtin as C
import Core.Var

tyUnitName, tyBoolName, tyIntName, tyStringName :: Var Typed
tyFloatName, tyLazyName, tyConstraintName, tyArrowName, tyTupleName, tyListName, tyShowName :: Var Typed
tyIntName    = ofCore C.vInt
tyStringName = ofCore C.vString
tyBoolName   = ofCore C.vBool
tyUnitName   = ofCore C.vUnit
tyFloatName  = ofCore C.vFloat
tyLazyName   = ofCore C.vLazy
tyArrowName  = ofCore C.vArrow
tyTupleName  = ofCore C.vProduct
tyListName  = ofCore C.vList
tyShowName  = ofCore C.vShow
tyConstraintName = TgInternal "constraint"

tyUnit, tyBool, tyInt, tyString, tyFloat, tyLazy, tyConstraint, tyArrow, tyList, tyShow :: Type Typed
tyInt    = TyCon tyIntName
tyString = TyCon tyStringName
tyBool   = TyCon tyBoolName
tyUnit   = TyCon tyUnitName
tyFloat  = TyCon tyFloatName
tyLazy   = TyCon tyLazyName
tyArrow  = TyCon tyArrowName
tyList   = TyCon tyListName
tyShow   = TyCon tyShowName
tyConstraint = TyCon tyConstraintName

forceName, lAZYName :: Var Typed
forceName = ofCore C.vForce
lAZYName  = ofCore C.vLAZY

cONSName, nILName :: Var Typed
cONSName = ofCore C.vCONS
nILName  = ofCore C.vNIL

forceTy, lAZYTy, cONSTy, nILTy, showTy, sHOWTy :: Type Typed
forceTy = a *. TyApp tyLazy (TyVar a) ~> TyVar a
lAZYTy = a *. (tyUnit ~> TyVar a) ~> TyApp tyLazy (TyVar a)
cONSTy = a *. TyTuple (TyVar a) (TyApp tyList (TyVar a)) ~> TyApp tyList (TyVar a)
nILTy = a *. TyApp tyList (TyVar a)
showTy = a *. TyPi (Implicit (TyApp tyShow (TyVar a))) (TyVar a ~> tyString)
sHOWTy = a *. (TyVar a ~> tyString) ~> TyApp tyShow (TyVar a)

forceTy', lAZYTy', cONSTy', nILTy' :: Type Typed -> Type Typed
forceTy' x = TyApp tyLazy x ~> x
lAZYTy' x = TyArr tyUnit x ~> TyApp tyLazy x
cONSTy' x = TyTuple x (TyApp tyList x) ~> TyApp tyList x
nILTy' = TyApp tyList

opAppName, showName, sHOWName :: Var Typed
opAppName = ofCore C.vOpApp
showName = ofCore C.vOpShow
sHOWName = ofCore C.vSHOW

data BuiltinModule = BM
  { vars    :: [(Var Resolved, Type Typed)]
  , types   :: [(Var Resolved, Type Typed)]
  , modules :: [(Var Resolved, BuiltinModule)]
  , constructors :: Map.Map (Var Resolved) (Set.Set (Var Typed))
  , classes :: Map.Map (Var Typed) T.ClassInfo
  }

instance Semigroup BuiltinModule where
  (BM v t m c i) <> (BM v' t' m' c' i') = BM (v <> v') (t <> t') (m <> m') (c <> c') (i <> i')

instance Monoid BuiltinModule where
  mempty = BM { vars = mempty, types = mempty, modules = mempty, constructors = mempty, classes = mempty }

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
           , (cONSName, cONSTy), (nILName, nILTy)
           , (showName, showTy)
           ]

  , types = [ tp C.vBool, tp C.vInt, tp C.vString, tp C.vFloat, tp C.vUnit
            , (tyLazyName, TyType ~> TyType)
            , (tyArrowName, TyType ~> TyType ~> TyType)
            , (tyTupleName, TyType ~> TyType ~> a *. TyVar a)
            , (tyConstraintName, TyType)
            , (tyListName, TyType ~> TyType)
            , (tyShowName, TyType ~> tyConstraint)
            ]
  , constructors = Map.fromList
      [ (tyListName, Set.fromList [cONSName, nILName] )
      ]
  , classes = Map.fromList
      [ ( tyShowName
        , T.ClassInfo
            { T._ciName = tyShowName
            , T._ciHead = TyApp tyShow (TyVar a)
            , T._ciMethods = Map.fromList [ (showName, TyVar a ~> tyString) ]
            , T._ciContext = mempty
            , T._ciConstructorName = sHOWName
            , T._ciConstructorTy = sHOWTy
            , T._ciClassSpan = internal
            , T._ciDefaults = mempty
            }
        )
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
  go (BM vs ts ms _ _) =
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
  go (BM vs ts ms cs ci) =
    foldr ((<>) . go . snd) (T.envOf (T.scopeFromList vs <> T.scopeFromList ts)) ms
      & T.types %~ mappend cs
      & T.classDecs %~ mappend ci

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
