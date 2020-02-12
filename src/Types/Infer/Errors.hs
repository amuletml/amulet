{-# LANGUAGE FlexibleContexts, GADTs, TupleSections #-}
module Types.Infer.Errors where
--
import Control.Monad.Infer
import Control.Lens

import qualified Data.Set as Set
import Data.Maybe

import {-# SOURCE #-} Types.Holes

import Syntax.Transform
import Syntax.Builtin
import Syntax.Types
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
    TyCon v () -> case appsView t of
      TyCon v' ():_ | v == v' ->
        flip Suggestion $
          string "did you mean to saturate it with respect to universals,"
            <#> string "as in" <+> pretty t <> char '?'
      [TyCon v' ()] | v /= v' ->
        flip Suggestion $
          string "did you mean to use" <+> pretty v' <+> string "instead of" <+> pretty v <> char '?'
      _ -> id
    TyApp{} -> case (appsView ty, appsView t) of
      (TyCon v' ():xs, TyCon v ():_) | v /= v' ->
        flip Suggestion $
          string "did you mean" <+> pretty (rewind v xs) </> string "instead of" <+> pretty ty <> char '?'
      _ -> id
    _ -> id
  (err, k) = getErr oerr

gadtConManyArgs :: Constructor Desugared -> TypeError
gadtConManyArgs c@(GadtCon _ _ t _) = ArisingFrom (Malformed t) (BecauseOf c) `Note` tooManyArgs where
  tooManyArgs = string "Generalised constructors must have either 0 or 1 arguments."
gadtConManyArgs _ = undefined

getErr :: TypeError -> (TypeError, TypeError -> TypeError)
getErr (ArisingFrom e blame) = case getErr e of
  (e, k) -> (e, flip ArisingFrom blame . k)
getErr x = (x, id)

rewind :: Var Typed -> [Type Typed] -> Type Typed
rewind x = foldl TyApp (TyCon x ())

foundHole :: MonadNamey m => Env -> Ann Typed -> Var Typed -> Type Typed -> Subst Typed -> m TypeError
foundHole env ann hole ht sub =
  FoundHole hole (apply sub hole_t) <$> findHoleCandidate sub (ann ^. _1) env (apply sub hole_t)
  where
    unskolemise k (TySkol v) = case sub ^. at (v ^. skolIdent) of
      Just t -> apply sub t
      _ -> k v
    unskolemise _ x = x

    hole_t = transformType (unskolemise TySkol) (apply sub ht)

unsatClassCon :: (MonadReader Env m, Reasonable f p)
              => f p
              -> Constraint Typed
              -> WhyUnsat
              -> m TypeError
unsatClassCon why con@(ConImplicit _ _ _ (TyApps (TyCon clss_tc ()) args)) unsat | clss_tc /= tyEqName =
  do
    scope_ts <- view tySyms
    class_info <- view (classDecs . at clss_tc . non (error "unsatClassCon: TyCon{} class with no info in scope"))
    let dropped = can_drop (class_info ^. ciFundep)
        relevant_args = map snd . filter (flip Set.notMember dropped . fst) . zip [0..] $ args
        tfs = mapMaybe (isTf_app scope_ts) relevant_args
        notes = foldMap mkNote (Set.toList (Set.fromList tfs))

    pure (appEndo notes (ArisingFrom (UnsatClassCon (BecauseOf why) con unsat) (BecauseOf why)))
  where
    isTf ts t =
      case ts ^. at t of
        Just TyFamInfo{ _tsConstraint = x } -> Just (isJust x)
        _ -> Nothing

    isTf_app ts (TyApps (TyCon v ()) _) = (, v) <$> isTf ts v
    isTf_app _ _ = Nothing

    can_drop fd =
      case fd of
        [] -> mempty
        (f:fds) -> foldr (Set.intersection . fd_det) (fd_det f) fds
      where fd_det (_, x, _) = Set.fromList x

    mkNote :: (Bool, Var Typed) -> Endo TypeError
    mkNote ~(is_assoc, v) = Endo $ \x ->
      let what =
            if is_assoc
               then string "an" <+> keyword "associated type"
               else string "a" <+> keyword "type function"
       in Note x
            (displayType (TyCon v () :: Type Typed)
              <+> string "is" <+> what <> string ", and so may not be injective")

unsatClassCon why con unsat = pure (UnsatClassCon (BecauseOf why) con unsat)
