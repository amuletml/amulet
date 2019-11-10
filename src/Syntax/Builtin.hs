{-# LANGUAGE OverloadedStrings #-}

{-| A shared source of builtin varibles for the resolver and type checker.

This largely defines variables in terms of those defined in
"Core.Builtin". We use 'TgName', along with core's variable IDs in order
to make lowering easier, and ensure core and syntax remain consistent.

For variables which will be removed by the time lowering occurs (such as
TC or resolver specific names), one may use 'TgInternal'.
-}
module Syntax.Builtin
  ( builtinResolve
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
  , tyTypeError_n, tyeString_n, tyHCat_n, tyVCat_n, tyShowType_n

  , tyTypeable_n, tyTypeRep_n, eqTypeRep_n, typeOf_n, mkTypeRep_n
  , tvProxy_n, tvA_n, tvB_n
  , typeable_CI
  , tcTypeable_app, tcTypeable_kk
  ) where

import Control.Lens

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text ()

import qualified Syntax.Resolve.Scope as R
import qualified Syntax.Types as T

import Data.Span

import Syntax.Implicits
import Syntax.Boolean
import Syntax.Var
import Syntax.Type hiding (record)

import qualified Core.Builtin as C
import Core.Var

import {-# SOURCE #-} Types.Derive.Typeable

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

tyTypeError_n, tyErrMsg_n, tyeString_n, tyHCat_n, tyVCat_n, tyShowType_n :: Var Typed
tyErrMsg :: Type Typed
tyTypeError_n = ofCore C.tcTypeError
tyErrMsg_n = ofCore C.tcErrKind
tyeString_n = ofCore C.tcString
tyHCat_n = ofCore C.tcHCat
tyVCat_n = ofCore C.tcVCat
tyShowType_n = ofCore C.tcShowType

tyErrMsg = TyCon tyErrMsg_n

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
tyKStr       = TyCon (TgInternal "Amc" <> tyKStrName)
tyKInt       = TyCon (TgInternal "Amc" <> tyKIntName)
tyRowCons    = TyCon (TgInternal "Amc" <> rowConsName)
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

typeOf_n, tyTypeable_n, tyTypeRep_n, mkTypeRep_n, eqTypeRep_n :: Var Typed
typeOf_n = ofCore C.tcUnTypeable
tyTypeable_n = ofCore C.tcTypeable
tyTypeRep_n = ofCore C.tcTypeRep
mkTypeRep_n = ofCore C.tcTYPEREP
eqTypeRep_n = ofCore C.tcEqTypeRep

tvProxy_n :: Var Typed
tvProxy_n = ofCore C.tyvarProxy

tcTypeable_app, tcTypeable_kk :: Var Typed
tcTypeable_app = ofCore C.tcTypeableApp
tcTypeable_kk = ofCore C.tcTypeableKnownKnown

data BuiltinModule = BM
  { vars         :: [(Var Resolved, Type Typed)]
  , types        :: [(Var Resolved, Type Typed)]
  , modules      :: [(Var Resolved, BuiltinModule)]
  , constructors :: Map.Map (Var Resolved) (Set.Set (Var Typed))
  , classes      :: [(Var Resolved, T.ClassInfo)]
  , families     :: [(Var Resolved, T.TySymInfo)]
  }

instance Semigroup BuiltinModule where
  (BM v t m c ci fi) <> (BM v' t' m' c' ci' fi') = BM (v <> v') (t <> t') (m <> m') (c <> c') (ci <> ci') (fi <> fi')

instance Monoid BuiltinModule where
  mempty = BM { vars = mempty, types = mempty, modules = mempty, constructors = mempty, classes = mempty, families = mempty }

builtins :: BuiltinModule
builtins =
  mempty
  { vars = [ (opAppName, a *. b *. (TyVar a ~> TyVar b) ~> TyVar a ~> TyVar b)
           , (lAZYName, lAZYTy)
           , (forceName, forceTy)
           , (cONSName, cONSTy)
           , (nILName, nILTy)

           , (assignName, a *. TyApp tyRef (TyVar a) ~> (TyVar a ~> tyUnit))
           , (derefName, a *. TyApp tyRef (TyVar a) ~> TyVar a)
           , (refName, a *. TyVar a ~> TyApp tyRef (TyVar a))

           , ( eqTypeRep_n, a *. b *. new
                         *. TyApps (TyCon tyTypeRep_n) [ TyVar a ]
                         ~> TyApps (TyCon tyTypeRep_n) [ TyVar b ]
                         ~> TyPi (Implicit (TyApps tyEq [ TyVar a, TyVar b ])) (tyUnit ~> TyVar new)
                         ~> (tyUnit ~> TyVar new)
                         ~> TyVar new )

           , ( typeOf_n, a *. tvProxy_n
                      *. TyPi (Implicit (TyApps (TyCon tyTypeable_n) [ TyVar a ]))
                          (TyApps (TyVar tvProxy_n) [ TyVar a ] ~> TyApps (TyCon tyTypeRep_n) [ TyVar a ] ))
           , ( mkTypeRep_n
             , a *. TyExactRows [ ("fingerprint", tyInt) , ("name", tyString) ]
                 ~> TyApps (TyCon tyTypeRep_n) [ TyVar a ] )

           ]

  , types = [ tp C.vBool, tp C.vInt, tp C.vString, tp C.vFloat, tp C.vUnit
            , (tyLazyName, TyType ~> TyType)
            , (tyArrowName, TyType ~> TyType ~> TyType)
            , (tyTupleName, TyType ~> TyType ~> TyType)
            , (tyConstraintName, TyType)
            , (tyListName, TyType ~> TyType)
            , (tyRefName, TyType ~> TyType)
            , (tyEqName, a *. TyVar a ~> TyVar a ~> tyConstraint)
            , (tyTypeable_n, a *. TyVar a ~> tyConstraint)
            , (tyTypeRep_n, a *. TyVar a ~> TyType)
            ]

  , constructors = Map.fromList
      [ (tyListName, Set.fromList [cONSName, nILName] )
      ]

  , classes = [ (tyEqName, T.MagicInfo [] Nothing)
              , (tyTypeable_n, typeable_CI
                  )
            ]

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
                          , ( tyeString_n, tyString ~> tyErrMsg )
                          , ( tyHCat_n, tyErrMsg ~> tyErrMsg ~> tyErrMsg )
                          , ( tyVCat_n, tyErrMsg ~> tyErrMsg ~> tyErrMsg )
                          , ( tyShowType_n, a *. TyVar a ~> tyErrMsg )
                          , ( tyTypeError_n, a *. tyErrMsg ~> TyVar a )
                          ]
                 , types = [ (tyKStrName, tyString ~> tyConstraint)
                           , (tyKIntName, tyInt ~> tyConstraint)
                           , (rowConsName, TyType ~> tyString ~> TyType ~> TyType ~> tyConstraint)
                           , (tyErrMsg_n, TyType)
                           , (tyeString_n, tyString ~> tyErrMsg )
                           , (tyHCat_n, tyErrMsg ~> tyErrMsg ~> tyErrMsg )
                           , (tyVCat_n, tyErrMsg ~> tyErrMsg ~> tyErrMsg )
                           , (tyShowType_n, a *. TyVar a ~> tyErrMsg )
                           , (tyTypeError_n, a *. tyErrMsg ~> TyVar a )
                           ]
                 , classes = [ (tyKStrName, T.MagicInfo [] Nothing)
                             , (tyKIntName, T.MagicInfo [] Nothing)
                             , (rowConsName, T.MagicInfo [ ( [0, 1, 2], [3], internal )
                                                         , ( [1, 3], [2, 0], internal ) ]
                                                         Nothing )
                             ]
                 , constructors = Map.fromList [(tyErrMsg_n, Set.fromList [tyeString_n, tyHCat_n, tyVCat_n, tyShowType_n])]
                 }
        ) ]
  , families = [ (tyTypeError_n, T.TyFamInfo { T._tsName = tyTypeError_n
                                             , T._tsEquations = []
                                             , T._tsArgs = [a]
                                             , T._tsKind = a *. tyErrMsg ~> TyVar a
                                             , T._tsConstraint = Nothing
                                             }) ]
  }

  where
    -- Helper functions for types
    tp x = (ofCore x, TyType)

builtinInstances :: ImplicitScope T.ClassInfo Typed
builtinInstances = builtinTypeableInsts

typeable_CI :: T.ClassInfo
typeable_CI =
  T.ClassInfo { T._ciName = tyTypeable_n
              , T._ciHead = TyApps (TyCon tyTypeable_n) [ TyVar a ]
              , T._ciMethods =
                  Map.fromList [ (typeOf_n, tvProxy_n
                                         *. TyApps (TyVar tvProxy_n) [ TyVar a ]
                                         ~> TyApps (TyCon tyTypeRep_n) [ TyVar a ])
                               ]
              , T._ciAssocTs = mempty
              , T._ciContext = mempty
              , T._ciConstructorName = ofCore C.tcTYPEABLE
              , T._ciConstructorTy =
                a *. (tvProxy_n *. TyApps (TyVar tvProxy_n) [ TyVar a ] ~> TyApps (TyCon tyTypeRep_n) [ TyVar a ])
                  ~> TyApps (TyCon tyTypeable_n) [ TyVar a ]
              , T._ciClassSpan = internal
              , T._ciDefaults = mempty
              , T._ciMinimal = Var "type_of"
              , T._ciFundep = []
              , T._ciDerive = Just deriveTypeable
              }

-- | The builtin scope and module list for the resolver
builtinResolve :: R.Signature
builtinResolve = go builtins where
  go :: BuiltinModule -> R.Signature
  go (BM vs ts ms _ _ _) =
    R.Signature
    { R._vals = buildVars vs
    , R._types = buildVars ts
    , R._modules = foldr (\(var, mod) -> Map.insert (getName var) (var, Just (go mod))) mempty ms
    }

  buildVars :: [(Var Resolved, Type Typed)] -> Map.Map R.VarName R.Slot
  buildVars = foldr (\(var, _) -> Map.insert (getName var) (R.SVar var)) mempty

  getName (TgInternal x) = x
  getName (TgName x _) = x

-- | The builtin scope for type inference
builtinEnv :: T.Env
builtinEnv = go builtins where
  go (BM vs ts ms cs ci fi) =
    foldr ((<>) . go . snd) (T.envOf (T.scopeFromList vs <> T.scopeFromList ts)) ms
      & T.types %~ mappend cs
      & T.modules %~ mappend (fake ms)
      & T.classDecs %~ mappend (Map.fromList ci)
      & T.tySyms %~ mappend (Map.fromList fi)
      & T.classes %~ const builtinInstances
  fake ms = Map.fromList (ms & map (_2 .~ mempty))

-- | Construct a syntax variable from a core one
ofCore :: CoVar -> Var Resolved
ofCore v@(CoVar i _ _) = TgName (covarDisplayName v) i

-- Declare some syntactic sugar to make building types easier
(~>) :: Type Typed -> Type Typed -> Type Typed
(~>) = TyArr

(*.) :: Var Typed -> Type Typed -> Type Typed
v *. t = TyPi (Invisible v (Just TyType) Spec) t

infixr ~>
infixr *.

-- | Some internal type variables. These do not need to be unique as they
-- are bound by a forall.
tvA_n, tvB_n :: Var Resolved
tvA_n = a
tvB_n = b

a, b, record, ttype, key, new :: Var Resolved
a = ofCore C.tyvarA
b = ofCore C.tyvarB
record = ofCore C.tyvarRecord
ttype = ofCore C.tyvarType
key = ofCore C.tyvarKey
new = ofCore C.tyvarNew
