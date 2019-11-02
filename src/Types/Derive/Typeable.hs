{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Types.Derive.Typeable (deriveTypeable, builtinTypeableInsts) where

import Syntax.Implicits
import Syntax.Toplevel
import Syntax.Builtin
import Syntax.Types
import Syntax.Type
import Syntax.Expr
import Syntax.Var

import Data.Span


deriveTypeable :: DerivingStrat
deriveTypeable = DerivingStrat genTypeable

genTypeable :: Applicative m
            => Type Typed
            -> Ann Resolved
            -> m (Maybe (Toplevel Desugared))
genTypeable (TyApps _ [ TyCon (TgName nm id) ]) ann =
  let ty_con :: Expr Desugared
      ty_con = App (VarRef mkTypeRep_n ann)
                  (Record [ Field "fingerprint" (Literal (LiInt (fromIntegral id)) ann) ann
                          , Field "name" (Literal (LiStr nm) ann) ann
                          ]
                    ann)
                  ann
      type_of :: InstanceItem Desugared
      type_of =
        MethodImpl
          (Binding
            typeOf_n
            (Fun (PatParam (Wildcard ann))
                ty_con
                ann)
            False
            ann)
  in
    pure . pure $ Instance tyTypeable_n
                            Nothing
                            (TyApps (TyCon tyTypeable_n) [TyCon (TgName nm id)])
                            [ type_of ]
                            True
                            ann
genTypeable _ _ = undefined

builtinTypeableInsts :: ImplicitScope ClassInfo Typed
builtinTypeableInsts =
  insert
    internal
    InstSort
    tcTypeable_app
    ( tvA_n *. tvB_n *.
      TyTuple (TyApp (TyCon tyTypeable_n) (TyVar tvA_n))
              (TyApp (TyCon tyTypeable_n) (TyVar tvB_n))
  :=> TyApp (TyCon tyTypeable_n) (TyApp (TyVar tvA_n) (TyVar tvB_n)))
    typeable_CI
    (foldr builtin_typeable mempty [ tyInt
                                   , tyString
      , tyBool
      , tyUnit
      , tyFloat
      , tyLazy
      , tyArrow
      , TyCon tyTupleName
      , tyList
      , tyRef
      , tyKStr
      , tyKInt
      , tyEq
      , tyConstraint ])

builtin_typeable :: Type Typed -> ImplicitScope ClassInfo Typed -> ImplicitScope ClassInfo Typed
builtin_typeable tau@(TyCon (TgName name id)) scope =
  insert
    internal
    InstSort
    tcTypeable_kk
    ( tvA_n *. TyApp tyKInt (TyLit (LiInt (fromIntegral id)))
  :=> (TyApp tyKStr (TyLit (LiStr name)) :=> TyApp (TyCon tyTypeable_n) tau ))
    typeable_CI
    scope
builtin_typeable _ scope = scope

(*.) :: Var Typed -> Type Typed -> Type Typed
a *. b = TyForall a (Just TyType) b

infixr 7 *.
