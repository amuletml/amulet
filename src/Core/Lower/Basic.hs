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
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Maybe

import qualified Core.Core as C
import qualified Core.Builtin as C
import Core.Core hiding (Atom, Term, Stmt, Type, Pattern, Arm)
import Core.Var

import qualified Syntax as S
import Syntax.Var (Var(..), Resolved, Typed)
import Syntax (Lit(..), Skolem(..))


data LowerState
  = LS
    { vars  :: Map.Map (Var Resolved) (C.Type CoVar)
    , ctors :: Map.Map (Var Resolved) (C.Type CoVar)
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
  | S.Invisible v Nothing <- bind = ForallTy (Relevant (mkTyvar (S.unTvName v))) StarTy (lowerType b)
  | S.Invisible v (Just c) <- bind = ForallTy (Relevant (mkTyvar (S.unTvName v))) (lowerType c) (lowerType b)
  | S.Implicit a <- bind = ForallTy Irrelevant (lowerType a) (lowerType b)
  | S.Anon a <- bind = ForallTy Irrelevant (lowerType a) (lowerType b)
lowerType (S.TyApp a b) = AppTy (lowerType a) (lowerType b)
lowerType (S.TyRows rho vs) = RowsTy (lowerType rho) (map (fmap lowerType) vs)
lowerType (S.TyExactRows []) = NilTy
lowerType (S.TyExactRows vs) = ExactRowsTy (map (fmap lowerType) vs)
lowerType (S.TyVar (TvName v)) = VarTy (mkTyvar v)
lowerType (S.TyCon (TvName v)) = ConTy (mkType v)
lowerType (S.TyPromotedCon (TvName v)) = ConTy (mkCon v) -- TODO this is in the wrong scope
lowerType (S.TySkol (Skolem (TvName (TgName _ v)) (TvName (TgName n _)) _ _)) = VarTy (CoVar v n TypeVar)
lowerType (S.TySkol _) = error "impossible lowerType TySkol"
lowerType (S.TyWildcard (Just t)) = lowerType t
lowerType (S.TyWildcard _) = error "impossible lowerType TyWildcard"
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
