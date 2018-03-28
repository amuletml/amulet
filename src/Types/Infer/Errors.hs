module Types.Infer.Errors where

import Control.Monad.Infer
import Syntax
import Pretty

import Debug.Trace

gadtConShape :: (Type Typed, Type Typed) -> Type Typed -> TypeError -> TypeError
gadtConShape (t, _) (TyArr c d) oerr = k . fix . flip Note (string "Generalised constructors can not be curried") $ err where
  fix = case t of
    TyArr x _ -> flip Suggestion (string "Perhaps use a tuple:" <+> verbatim (TyArr (TyTuple x c) d))
    TyForall vs (TyArr x _) -> flip Suggestion (string "Perhaps use a tuple:" <+> verbatim (TyForall vs (TyArr (TyTuple x c) d)))
    _ -> id
  (err, k) = getErr oerr
gadtConShape (_, t) ty oerr = k . fix . flip Note (itShouldBe <#> itIs) . flip Note msg $ err where
  msg = string "The type"
    <+> nest 2 (pretty ty
      </> string "is not an instance of the type being declared")
  itShouldBe = case t of
    TyApp{} -> string "It must end in an application like" <+> pretty t
    TyCon{} -> string "It must end in a reference to" <+> pretty t
    _ -> error "malformed"
  itIs = indent 2 $ string "but it ends in a" <> case ty of
    TyTuple{} -> empty <+> keyword "tuple"
    TyRows{} -> empty <+> keyword "polymorphic record"
    TyExactRows{} -> empty <+> keyword "record"
    TyApp{} -> char 'n' <+> string "application of some other type"
    TyVar{} -> empty <+> keyword "type variable"
    TyCon{} -> empty <+> keyword "type constructor"
    _ -> empty

  fix = case ty of
    TyTuple a b -> flip Suggestion (string "did you mean a function," <+> nest 2 (string "like" </> pretty (TyArr a b) <> char '?'))
    TyCon v -> case spine t of
      TyCon v':_ | v == v' -> flip Suggestion (string "did you mean to saturate it with respect to universals,"
                                           <#> string "as in" <+> pretty t <> char '?')
      [TyCon v'] | v /= v' -> flip Suggestion (string "did you mean to use" <+> pretty v' <+> string "instead of" <+> pretty v <> char '?')
      xs -> traceShow xs
    TyApp{} -> case (spine ty, spine t) of
      (TyCon v':xs, TyCon v:_) | v /= v' ->
        flip Suggestion (string "did you mean" <+> pretty (rewind v xs) </> string "instead of" <+> pretty ty <> char '?')
      _ -> id
    _ -> id
  (err, k) = getErr oerr

getErr :: TypeError -> (TypeError, TypeError -> TypeError)
getErr (ArisingFrom e blame) = case getErr e of
  (e, k) -> (e, flip ArisingFrom blame . k)
getErr x = (x, id)

spine :: Type p -> [Type p]
spine = go [] where
  go acc (TyApp fs x) = go (x:acc) fs
  go acc t = t:acc

rewind :: Var Typed -> [Type Typed] -> Type Typed
rewind x = foldl TyApp (TyCon x) . traceShowId 
