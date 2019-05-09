{-# LANGUAGE ViewPatterns #-}
module Types.Infer.Errors where

-- import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Control.Monad.Infer
import Control.Lens

import Types.Wellformed -- skols

import Syntax.Transform
import Syntax.Implicits
import Syntax.Subst
import Syntax

import Text.Pretty.Semantic

gadtConShape :: (Type Typed, Type Typed) -> Type Typed -> TypeError -> TypeError
gadtConShape (t, _) (TyArr c d) oerr = k . fix . flip Note curry $ err where
  curry = string "Generalised constructors can not be curried"
  fix = case t of
    TyArr x _ -> flip Suggestion (string "Perhaps use a tuple:" <+> verbatim (TyArr (TyTuple x c) d))
    TyForall v k (TyArr x _) ->
      flip Suggestion (string "Perhaps use a tuple:" <+> verbatim (TyForall v k (TyArr (TyTuple x c) d)))
    _ -> id
  (err, k) = getErr oerr
gadtConShape (_, t) ty oerr = k . fix . flip Note (itShouldBe <#> itIs) . flip Note msg $ err where
  msg = string "The type"
    <+> nest 2 (pretty ty
      </> indent 2 (string "is not an instance of the type being declared"))
  itShouldBe = case t of
    TyApp{} -> string "It must end in an application like" <+> pretty t
    TyCon{} -> string "It must end in a reference to" <+> pretty t
    _ -> error "malformed"
  itIs = indent 4 $ string "but it ends in a" <> case ty of
    TyTuple{} -> empty <+> keyword "tuple"
    TyRows{} -> empty <+> keyword "polymorphic record"
    TyExactRows{} -> empty <+> keyword "record"
    TyApp{} -> char 'n' <+> string "application of some other type"
    TyVar{} -> empty <+> keyword "type variable"
    TyCon{} -> empty <+> keyword "type constructor"
    _ -> empty

  fix = case ty of
    TyTuple a b ->
      flip Suggestion $
        string "did you mean a function,"
          <+> nest 2 (string "like" </> pretty (TyArr a b) <> char '?')
    TyCon v -> case spine t of
      TyCon v':_ | v == v' ->
        flip Suggestion $
          string "did you mean to saturate it with respect to universals,"
            <#> string "as in" <+> pretty t <> char '?'
      [TyCon v'] | v /= v' ->
        flip Suggestion $
          string "did you mean to use" <+> pretty v' <+> string "instead of" <+> pretty v <> char '?'
      _ -> id
    TyApp{} -> case (spine ty, spine t) of
      (TyCon v':xs, TyCon v:_) | v /= v' ->
        flip Suggestion $
          string "did you mean" <+> pretty (rewind v xs) </> string "instead of" <+> pretty ty <> char '?'
      _ -> id
    _ -> id
  (err, k) = getErr oerr

getErr :: TypeError -> (TypeError, TypeError -> TypeError)
getErr (ArisingFrom e blame) = case getErr e of
  (e, k) -> (e, flip ArisingFrom blame . k)
getErr x = (x, id)

rewind :: Var Typed -> [Type Typed] -> Type Typed
rewind x = foldl TyApp (TyCon x)

foundHole :: Var Typed -> Type Typed -> Subst Typed -> TypeError
foundHole hole ht sub = helpMaybe (FoundHole hole (apply sub ty)) where
  unskolemise (TySkol v) = case sub ^. at (v ^. skolIdent) of
    Just t -> t
    _ -> TyVar (v ^. skolVar)
  unskolemise x = x

  go :: Type Typed -> Type Typed
  go = transformType unskolemise

  ty = go (apply sub ht)
  skolvars = Set.toList (skols ty)

  helpMaybe
    | null skolvars = id
    | otherwise = (`Note` help)

  help :: Doc
  help =
    let oneEquality (view skolIdent -> x) =
          case sub ^. at x of
            Just ty -> pure (pretty x <+> soperator (char '~') <+> pretty ty)
            Nothing -> []
        oneEquality :: Skolem Typed -> [Doc]
     in string "The following equalities might be relevant:"
        <#> vsep (map bullet (concatMap oneEquality skolvars))
