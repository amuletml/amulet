{-# LANGUAGE FlexibleContexts, TypeFamilies, ScopedTypeVariables #-}

{-| A simplified version of pmcheck's inhabited type checker. This determines
if a type is:
 - Trivially inhabitable (i.e. has an inhabitable constructor),
 - Uninhabitable (all constructors are uninhabitable), or
 - Unknown (relies on additional information, such as constraints or type variables)
-}
module Types.Inhabited (inhabited) where

import Control.Lens

import Syntax.Var
import Syntax.Type
import Syntax.Types
import Syntax.Toplevel

inhabited :: forall p. Var p ~ VarResolved
          => Env -> Var p -> [Constructor p] -> Inhabited
inhabited env tyName = goCtors where
  goCtors :: [Constructor p] -> Inhabited
  goCtors [] = Uninhabited
  goCtors (UnitCon{}:_) = Inhabited
  goCtors (ArgCon _ _ t _:cs) = orInhabited (goType t) (goCtors cs)
  goCtors (GadtCon{}:cs) =
    -- :( I cannot think of a way to do this sensibly, so we let the verifier
    -- check this for us.
    orInhabited Unknown (goCtors cs)

  goTypes :: [Type p] -> Inhabited
  goTypes [] = Inhabited
  goTypes (t:ts) =
      case goType t of
          Inhabited -> goTypes ts
          x -> x

  goType :: Type p -> Inhabited
  -- We can't know if a type variable is inhabited or not!
  goType TyVar{} = Unknown

  goType (TyApp t _) = goType t
  goType (TyCon v _)
    | v == tyName = Unknown
    | otherwise = case env ^. types . at v of
        Nothing -> Inhabited -- Abstract types are considered inhabited.
        Just t -> t ^. tdInhabited

  -- We don't really have the concept of uninhabited types, so we assume
  -- type names are.
  goType TyPromotedCon{} = Inhabited
  -- For now, we'll just assume all Pi types are inhabited. Thankfully,
  -- they all are.
  goType TyPi{} = Inhabited
  -- All the boring types: just determine if the children are inhabited
  goType (TyRows f fs) = goTypes (f:map snd fs)
  goType (TyExactRows fs) = goTypes (snd <$> fs)
  goType (TyTuple l r) = goTypes [l, r]
  goType (TyOperator l v r) = goType (TyApp (TyApp v l) r)
  goType (TyWildcard t) = maybe Unknown goType t
  goType (TyParens t) = goType t

  goType TySkol{} = Inhabited
  goType (TyWithConstraints _ t) = goType t
  goType TyType = Inhabited
  goType TyLit{} = Inhabited
  goType TyTupleL{} = Inhabited

orInhabited :: Inhabited -> Inhabited -> Inhabited
orInhabited Inhabited _ = Inhabited
orInhabited Uninhabited x = x
orInhabited Unknown Inhabited = Inhabited
orInhabited Unknown _ = Unknown
