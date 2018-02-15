{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Errors where

import Control.Monad.Infer

import qualified Data.Text as T
import Data.Text (Text)

import Data.Spanned

import Data.Function

import qualified Control.Monad.Infer as I

import qualified Syntax.Resolve as R
import Syntax.Resolve
import Syntax
import Pretty

instance Pretty TypeError where
  pprint (NotEqual a b) =
    pprint "Type error: failed to unify "
       <+> verbatim a
       <+> " with " <+> verbatim b
  pprint (KindsNotEqual a b) =
    pprint "Kind error: failed to unify "
       <+> verbatim a
       <+> " with " <+> verbatim b
  pprint (Occurs v t) = "Occurs check: Variable "
                    <+> verbatim v
                    <+> " occurs in "
                    <+> verbatim t
  pprint (I.NotInScope e) = "Variable not in scope: "
                         <+> verbatim e
  pprint (I.EmptyMatch e) = annotation e <+> ": Empty match expression"
  pprint (I.EmptyBegin e) = annotation e <+> ": Empty begin expression"
  pprint (I.ArisingFrom er ex) = do
    annotation ex <+> ": " <+> er
    block 1 . (newline <+>) $
      bullet "Arising from use of " <+> verbatim ex
  pprint (FoundHole xs) = interleave newline (map prnt xs) where
    prnt :: Expr Typed -> PrettyP
    prnt (Hole v s)
      | prettyPrint (verbatim v) == T.pack "`_`"
      = fst s <+> ": Found typed hole of type " <+> verbatim (snd s)
      | otherwise
      = fst s <+> ": Found typed hole "
              <+> verbatim v
              <+> " (of type " <+> verbatim (snd s) <+> ")"
    prnt _ = undefined
  pprint (Note te m) = do
    pprint te
    block 1 . (newline <+>) $
      bullet (opClr "Note: ") <+> block 2 m
  pprint (Suggestion te m) = do
    pprint te
    block 1 . (newline <+>) $
      bullet (opClr "Suggestion: ") <+> block 4 m
  pprint (CanNotInstance rec new)
    | (TyRows rho _) <- rec
    , prettyPrint new == prettyPrint rho
    = pprint (Malformed rec)
    | otherwise
    =   "Can not instance hole of record type " <+> verbatim rec
    <+> " to type " <+> verbatim new
  pprint (Malformed tp) = do
    "The type " <+> verbatim tp <+> " is malformed."
    body 1 [ bullet (opClr "Note: ")
              <+> "This type was rejected by the well-formedness check."
           , bullet (opClr "Note: ") <+> "This might be a bug." ]
  pprint (NoOverlap ta tb)
    | TyExactRows ra <- ta
    , TyRows _ rb <- tb
    =   "No overlap between " <+> kwClr "exact" <+> " record " <+> verbatim ta
    <+> " and " <+> kwClr "polymorphic " <+> "record " <+> verbatim tb
    <+> block 1 (missing ra rb)
    | TyExactRows rb <- tb
    , TyRows _ ra <- ta
    =   "No overlap between " <+> kwClr "exact" <+> " record " <+> verbatim ta
    <+> " and " <+> kwClr "polymorphic " <+> "record " <+> verbatim tb
    <+> block 1 (missing ra rb)
    | TyExactRows ra <- ta
    , TyExactRows rb <- tb
    =   "No overlap between " <+> kwClr "exact" <+> " records " <+> verbatim ta
    <+> " and " <+> verbatim tb
    <+> block 1 (missing ra rb)
    | otherwise
    =   "\x1b[1;32minternal compiler error\x1b[0m: NoOverlap "
    <+> interleave " " [ta, tb]
  pprint (IllegalTypeApp ex ta _)
    = body 1 [ "Illegal type application " <+> verbatim ex
             , bullet "because of type " <+> verbatim ta ]
  pprint (EscapedSkolems esc ty) =
    body 1 [ "Illegal type " <+> verbatim ty <+> "; "
           , case esc of
               [x] -> "since skolem type constant " <+> x <+> " has escaped "
               _ -> "since skolem type constants " <+> interleave ", " esc <+> " have escaped "
           ]
  pprint (SkolBinding a b)
    = "Can not unify skolem type constant " <+> a <+> " with type " <+> verbatim b

instance Pretty ResolveError where
  pprint (R.NotInScope e) = "Variable not in scope: "
                         <+> verbatim e
  pprint (R.EmptyMatch e) = annotation e <+> ": Empty match expression"
  pprint (R.EmptyBegin e) = annotation e <+> ": Empty begin expression"
  pprint (R.ArisingFrom er ex) = do
    annotation ex <+> ": " <+> er
    block 1 . (newline <+>) $
      bullet "Arising from use of " <+> verbatim ex
  pprint (R.ArisingFromTop er top) = do
    annotation top <+> ": " <+> er
    block 1 . (newline <+>) $
      bullet "Arising in " <+> verbatim top

prettyRows :: Pretty (Var p) => [(T.Text, Type p)] -> PrettyP
prettyRows = braces
           . interleave (", " :: String)
           . map (\(x, y) -> x <+> opClr (" : " :: String) <+> y)


missing :: [(Text, b)] -> [(Text, b)] -> PrettyP
missing ra rb
  | length ra < length rb
  = body 1 [ bullet "Namely, the following fields are missing: "
                <+> interleave ", " (diff ra rb)]
  | length ra > length rb
  = body 1 [ bullet "Namely, the following fields should not be present: "
                <+> interleave ", " (diff ra rb)]
  | length ra == length rb
  = body 1 [ bullet (kwClr "Note: ") <+> "No fields match"
           , bullet "The following fields are missing: "
                <+> interleave ", " (diff ra rb)]
missing _ _ = undefined -- freaking GHC

diff :: (Eq a, Pretty a) => [(a, b)] -> [(a, b)] -> [PrettyP]
diff ra rb = map (tvClr . fst) (deleteFirstsBy ((==) `on` fst) rb ra)

report :: Pretty p => p -> T.Text -> IO ()
report err _ = ppr $ pprint err

-- Some errors:
rejectedExistential :: Pretty (Var p) => Type p -> TypeError -> TypeError
rejectedExistential ttp e = Suggestion (Note (Note e rejected) explanation) fix where
  rejected, explanation :: String
  rejected = "GADT-style data constructors with existential type variables are rejected"
  explanation = "Our type system is not powerful enough to deal with the implications of existentials quite yet."

  fix :: PrettyP
  fix = body 0 [ pprint "Possible fix: rewriting the type so that all variables are universal:"
               , case ttp of
                   TyForall vs tp ->
                     "Consider changing " <+> verbatim ttp <+> " into " <+> verbatim (replaceTail vs tp)
                   _ -> error "rejectedExistentials only occur on types with existentials"
               ]

  replaceTail :: [Var p] -> Type p -> Type p
  replaceTail ap (TyArr t cs) = TyArr t (replaceTail ap cs)
  replaceTail ap x = foldl TyApp x (map TyVar ap)
