{-# LANGUAGE OverloadedStrings, ConstraintKinds, FlexibleContexts #-}
module Core.Lower.Basic
  ( LowerState(..)
  , LowerTrack
  , MonadLower
  , mkTyvar, mkVal, mkType, mkCo, mkCon, mkVar
  , lowerType
  , lowerLiteral
  ) where

import Control.Monad.Reader
import Control.Monad.Namey
import Control.Monad.State

import qualified Data.VarMap as VarMap
import qualified Data.VarSet as VarSet
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Maybe

import qualified Core.Core as C
import qualified Core.Builtin as C
import Core.Core hiding (Atom, Term, Stmt, Type, Pattern, Arm)
import Core.Var

import qualified Syntax as S
import Syntax.Var (VarResolved(..), Var, Resolved, Typed)
import Syntax (Lit(..), Skolem(..))


data LowerState
  = LS
    { vars  :: VarMap.Map (C.Type CoVar)
    , ctors :: VarMap.Map (C.Type CoVar)
    , types :: VarMap.Map VarSet.Set
    } deriving (Eq, Show)

type LowerTrack = VarMap.Map CoVar

type MonadLower m
  = ( MonadNamey m
    , MonadState LowerTrack m
    , MonadReader LowerState m )

mkTyvar, mkVal, mkType, mkCo, mkCon :: Var Resolved -> CoVar
mkTyvar = mkVar TypeVar
mkVal = mkVar ValueVar
mkType = mkVar TypeConVar
mkCo = mkVar CastVar
mkCon = mkVar DataConVar

-- | Make a core variable from a "Syntax" variable and a given kind.
mkVar :: VarInfo -> Var Resolved -> CoVar
mkVar k (TgName t i) = CoVar i t k
mkVar k (TgInternal name) = CoVar (builtin name) name k where
  builtin name = fromMaybe (error ("Cannot find builtin " ++ show name)) (Map.lookup name builtins)

  builtins = Map.fromList (map ex C.builtinTyList
                           ++ map (ex . fst) (C.builtinVarList :: [(CoVar, C.Type CoVar)])
                           ++ map ex [C.tyvarA, C.tyvarB]
                          )

  ex :: CoVar -> (T.Text, Int)
  ex (CoVar v n _) = (n, v)

-- | Lower a type from "Syntax" to one in "Core.Core".
lowerType :: S.Type Typed -> C.Type CoVar
lowerType t@S.TyTuple{} = go t where
  go (S.TyTuple a b) = ExactRowsTy [("_1", lowerType a), ("_2", lowerType b)]
  go x = lowerType x
lowerType (S.TyPi bind b)
  | S.Invisible v Nothing <- bind =
    ForallTy (Relevant (mkTyvar v)) StarTy (lowerType b)
  | S.Invisible v (Just c) <- bind =
    ForallTy (Relevant (mkTyvar v)) (lowerType c) (lowerType b)
  | S.Anon a <- bind =
    ForallTy Irrelevant (lowerType a) (lowerType b)
  | S.Implicit a <- bind =
    ForallTy Irrelevant (lowerType a) (lowerType b)
lowerType (S.TyApp (S.TyApp (S.TyCon (TgName _ (-38))) l) r) = ForallTy Irrelevant (lowerType l) (lowerType r)
lowerType (S.TyApp a b) = AppTy (lowerType a) (lowerType b)
lowerType (S.TyRows rho vs) = RowsTy (lowerType rho) (map (fmap lowerType) vs)
lowerType (S.TyExactRows []) = NilTy
lowerType (S.TyExactRows vs) = ExactRowsTy (map (fmap lowerType) vs)
lowerType (S.TyVar v) = VarTy (mkTyvar v)
lowerType (S.TyCon (TgName _ (-37))) = StarTy
lowerType (S.TyCon v) = ConTy (mkType v)
lowerType (S.TyPromotedCon v) = ConTy (mkCon v) -- TODO this is in the wrong scope
lowerType (S.TySkol (Skolem (TgName _ v) (TgName n _) _ _)) = VarTy (CoVar v n TypeVar)
lowerType (S.TySkol _) = error "impossible lowerType TySkol"
lowerType (S.TyOperator l o r) = (ConTy (mkType o) `AppTy` lowerType l) `AppTy` lowerType r
lowerType (S.TyWildcard (Just t)) = lowerType t
lowerType (S.TyWildcard _) = error "impossible lowerType TyWildcard"
lowerType (S.TyParens t) = lowerType t
lowerType (S.TyWithConstraints _ t) = lowerType t
lowerType S.TyType = StarTy

-- | Lower a literal from "Syntax" to one from "Core.Core".
lowerLiteral :: Lit -> Literal
lowerLiteral (LiFloat d) = Float d
lowerLiteral (LiInt i) = Int i
lowerLiteral (LiStr t) = Str t
lowerLiteral (LiBool True) = LitTrue
lowerLiteral (LiBool False) = LitFalse
lowerLiteral LiUnit = Unit
