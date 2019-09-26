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
  , tyRefName, tyKStrName, tyKIntName, tyEqName

  , tyUnit, tyBool, tyInt, tyString, tyFloat
  , tyLazy, tyConstraint, tyArrow, tyList
  , tyRef, tyKStr, tyKInt, tyRowCons, tyEq

  , forceName, lAZYName, forceTy, lAZYTy, forceTy', lAZYTy'
  , assignName, derefName, refName

  , cONSName, nILName, cONSTy, nILTy, cONSTy', nILTy'
  , opAppName

  , strValName, strValTy, intValName, intValTy
  , knownStrName, knownStrTy, knownStrTy'
  , knownIntName, knownIntTy, knownIntTy'

  , rowConsName, extendName, restrictName, rOWCONSName
  , rowConsTy, rowConsTy', rowConsTy'', rowConsTy''', rowConsTy''''
  , rEFLName, rEFLTy, rEFLTy'
  ) where

import Control.Lens

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text ()

import qualified Syntax.Resolve.Scope as R
import qualified Syntax.Types as T

import Data.Span

import Syntax.Var
import Syntax

import qualified Core.Builtin as C
import Core.Var

tyUnitName, tyBoolName, tyIntName, tyStringName, tyFloatName, tyLazyName, tyConstraintName, tyArrowName, tyTupleName, tyListName, tyRefName, tyKStrName, tyKIntName, tyEqName :: Var Typed 
tyIntName    = ofCore C.vInt
tyStringName = ofCore C.vString
tyBoolName   = ofCore C.vBool
tyUnitName   = ofCore C.vUnit
tyFloatName  = ofCore C.vFloat
tyLazyName   = ofCore C.vLazy
tyArrowName  = ofCore C.vArrow
tyTupleName  = ofCore C.vProduct
tyListName   = ofCore C.vList
tyRefName    = ofCore C.vRefTy
tyKStrName   = ofCore C.vKStrTy
tyKIntName   = ofCore C.vKIntTy
tyEqName     = ofCore C.vEq
tyConstraintName = TgInternal "constraint"

tyUnit, tyBool, tyInt, tyString, tyFloat, tyLazy, tyConstraint, tyArrow, tyList, tyRef, tyKStr, tyKInt, tyRowCons, tyEq :: Type Typed
tyInt        = TyCon tyIntName
tyString     = TyCon tyStringName
tyBool       = TyCon tyBoolName
tyUnit       = TyCon tyUnitName
tyFloat      = TyCon tyFloatName
tyLazy       = TyCon tyLazyName
tyArrow      = TyCon tyArrowName
tyList       = TyCon tyListName
tyConstraint = TyCon tyConstraintName
tyRef        = TyCon tyRefName
tyKStr       = TyCon tyKStrName
tyKInt       = TyCon tyKIntName
tyRowCons    = TyCon rowConsName
tyEq         = TyCon tyEqName

forceName, lAZYName :: Var Typed
forceName = ofCore C.vForce
lAZYName  = ofCore C.vLAZY

cONSName, nILName :: Var Typed
cONSName = ofCore C.vCONS
nILName  = ofCore C.vNIL

assignName, derefName, refName :: Var Typed
assignName = ofCore C.vAssign
derefName = ofCore C.vDeref
refName = ofCore C.vRef

forceTy, lAZYTy, cONSTy, nILTy :: Type Typed
forceTy = a *. TyApp tyLazy (TyVar a) ~> TyVar a
lAZYTy = a *. (tyUnit ~> TyVar a) ~> TyApp tyLazy (TyVar a)
cONSTy = a *. TyTuple (TyVar a) (TyApp tyList (TyVar a)) ~> TyApp tyList (TyVar a)
nILTy = a *. TyApp tyList (TyVar a)

forceTy', lAZYTy', cONSTy', nILTy' :: Type Typed -> Type Typed
forceTy' x = TyApp tyLazy x ~> x
lAZYTy' x = TyArr tyUnit x ~> TyApp tyLazy x
cONSTy' x = TyTuple x (TyApp tyList x) ~> TyApp tyList x
nILTy' = TyApp tyList

opAppName :: Var Typed
opAppName = ofCore C.vOpApp

strValName, knownStrName :: Var Typed
strValName = ofCore C.vStrVal
knownStrName = ofCore C.vKSTR

intValName, knownIntName :: Var Typed
intValName = ofCore C.vIntVal
knownIntName = ofCore C.vKINT

strValTy, knownStrTy, intValTy, knownIntTy :: Type Typed
strValTy = TyPi (Invisible a (Just tyString) Req) $ TyPi (Implicit (TyApp tyKStr (TyVar a))) tyString
knownStrTy = TyPi (Invisible a (Just tyString) Infer) $ tyString ~> TyApp tyKStr (TyVar a)
intValTy = TyPi (Invisible a (Just tyInt) Req) $ TyPi (Implicit (TyApp tyKInt (TyVar a))) tyInt
knownIntTy = TyPi (Invisible a (Just tyInt) Infer) $ tyInt ~> TyApp tyKInt (TyVar a)

knownStrTy', knownIntTy' :: Type Typed -> Type Typed
knownStrTy' a = tyString ~> TyApp tyKStr a
knownIntTy' a = tyInt ~> TyApp tyKInt a

rowConsName, rOWCONSName :: Var Typed
rowConsName = ofCore C.vRowCons
rOWCONSName = ofCore C.vROWCONS

extendName, restrictName :: Var Typed
extendName = ofCore C.vExtend
restrictName = ofCore C.vRestrict


rowConsTy :: Type Typed
rowConsTy = TyPi (Invisible key (Just tyString) Infer) $ record *. ttype *. new *. tyString ~> TyApps tyRowCons [TyVar record, TyVar key, TyVar ttype, TyVar new]

rowConsTy' :: Type Typed -> Type Typed
rowConsTy'' :: Type Typed -> Type Typed -> Type Typed
rowConsTy''' :: Type Typed -> Type Typed -> Type Typed -> Type Typed
rowConsTy'''' :: Type Typed -> Type Typed -> Type Typed -> Type Typed -> Type Typed
rowConsTy' x = record *. ttype *. new *. tyString ~> foldl1 TyApp [tyRowCons, TyVar record, x, TyVar ttype, TyVar new ]
rowConsTy'' x y = ttype *. new *. tyString ~> foldl1 TyApp [tyRowCons, y, x, TyVar ttype, TyVar new ]
rowConsTy''' x y z = new *. tyString ~> foldl1 TyApp [tyRowCons, y, x, z, TyVar new ]
rowConsTy'''' x y z a = tyString ~> foldl1 TyApp [tyRowCons, y, x, z, a ]

rEFLName :: Var Typed
rEFLName = ofCore C.vEQ

rEFLTy :: Type Typed
rEFLTy = a *. TyApps tyEq [TyVar a, TyVar a]

rEFLTy' :: Type Typed -> Type Typed
rEFLTy' x = TyApps tyEq [x, x]

data BuiltinModule = BM
  { vars    :: [(Var Resolved, Type Typed)]
  , types   :: [(Var Resolved, Type Typed)]
  , modules :: [(Var Resolved, BuiltinModule)]
  , constructors :: Map.Map (Var Resolved) (Set.Set (Var Typed))
  , classes :: [(Var Resolved, T.ClassInfo)]
  }

instance Semigroup BuiltinModule where
  (BM v t m c ci) <> (BM v' t' m' c' ci') = BM (v <> v') (t <> t') (m <> m') (c <> c') (ci <> ci')

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

           , (assignName, a *. TyApp tyRef (TyVar a) ~> (TyVar a ~> tyUnit))
           , (derefName, a *. TyApp tyRef (TyVar a) ~> TyVar a)
           , (refName, a *. TyVar a ~> TyApp tyRef (TyVar a))
           ]

  , types = [ tp C.vBool, tp C.vInt, tp C.vString, tp C.vFloat, tp C.vUnit
            , (tyLazyName, TyType ~> TyType)
            , (tyArrowName, TyType ~> TyType ~> TyType)
            , (tyTupleName, TyType ~> TyType ~> a *. TyVar a)
            , (tyConstraintName, TyType)
            , (tyListName, TyType ~> TyType)
            , (tyRefName, TyType ~> TyType)
            , (tyEqName, a *. TyVar a ~> TyVar a ~> tyConstraint)
            ]

  , constructors = Map.fromList
      [ (tyListName, Set.fromList [cONSName, nILName] )
      ]

  , classes = [ (tyEqName, T.MagicInfo []) ]

  , modules =
      [ ( TgInternal "Amc"
        , mempty { vars = [ (strValName, strValTy)
                          , (intValName, intValTy)
                          , ( extendName
                            , TyPi (Invisible key (Just tyString) Req) $
                              TyPi (Invisible record (Just TyType) Spec) $
                              TyPi (Invisible ttype (Just TyType) Spec) $
                              TyPi (Invisible new (Just TyType) Spec) $
                              TyPi (Implicit (foldl1 TyApp [tyRowCons, TyVar record, TyVar key, TyVar ttype, TyVar new ] )) $
                                TyVar ttype ~> TyVar record ~> TyVar new
                            )
                          , ( restrictName
                            , TyPi (Invisible key (Just tyString) Req) $
                              TyPi (Invisible record (Just TyType) Spec) $
                              TyPi (Invisible ttype (Just TyType) Spec) $
                              TyPi (Invisible new (Just TyType) Spec) $
                              TyPi (Implicit (foldl1 TyApp [tyRowCons, TyVar record, TyVar key, TyVar ttype, TyVar new ] )) $
                                TyVar new ~> (TyVar ttype `TyTuple` TyVar record)
                            )
                          ]
                 , types = [ (tyKStrName, tyString ~> tyConstraint)
                           , (tyKIntName, tyInt ~> tyConstraint)
                           , (rowConsName, TyType ~> tyString ~> TyType ~> TyType ~> tyConstraint)
                           ]
                 , classes = [ (tyKStrName, T.MagicInfo [])
                             , (tyKIntName, T.MagicInfo [])
                             , (rowConsName, T.MagicInfo [ ( [0, 1, 2], [3], internal )
                                                         , ( [1, 3], [2, 0], internal ) ])
                             ]
                 }
        ) ]
  }

  where

    -- Helper functions for operators
    opOf x t = (ofCore x, t)
    op t x = (ofCore x, t)

    intOp = op $ tyInt ~> tyInt ~> tyInt
    floatOp = op $ tyFloat ~> tyFloat ~> tyFloat
    stringOp = op $ tyString ~> tyString ~> tyString
    intCmp = op $ tyInt ~> tyInt ~> tyBool
    floatCmp = op $ tyFloat ~> tyFloat ~> tyBool
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
      & T.modules %~ mappend (fake ms)
      & T.classDecs %~ mappend (Map.fromList ci)
  fake ms = Map.fromList (ms & map (_2 .~ mempty))

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
a, b, record, ttype, key, new :: Var Resolved
a = ofCore C.tyvarA
b = ofCore C.tyvarB
record = ofCore C.tyvarRecord
ttype = ofCore C.tyvarType
key = ofCore C.tyvarKey
new = ofCore C.tyvarNew

