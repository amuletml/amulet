{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Errors where

import Control.Monad.Infer

import qualified Data.Text as T
import Data.Text (Text)

import Data.Function

import Syntax
import Pretty

instance Pretty TypeError where
  pprint (NotEqual a b) = 
    pprint "Type error: failed to unify "
       <+> verbatim a
       <+> " with " <+> verbatim b
  pprint (Occurs v t) = "Occurs check: Variable "
                    <+> verbatim v
                    <+> " occurs in "
                    <+> verbatim t
  pprint (NotInScope e) = "Variable not in scope: "
                      <+> verbatim e
  pprint (EmptyMatch e) = annotation e <+> ": Empty match expression"
  pprint (EmptyBegin e) = annotation e <+> ": Empty begin expression"
  pprint (ArisingFrom er ex) = do
    annotation ex <+> ": " <+> er
    block 1 . (newline <+>) $
      bullet "Arising from use of " <+> ex
  pprint (ExpectedArrow ap k v) = do
    "Kind error: In application " <+> verbatim ap
    block 1 . (newline <+>) $
      bullet "expected arrow kind, but got "
         <+> k
         <+> parens " (kind of " <+> v <+> ")"
  pprint (FoundHole xs) = interleave newline (map prnt xs) where
    prnt (Hole v s)
      | prettyPrint (verbatim v) == T.pack "`_`"
      = s <+> ": Found typed hole of type " <+> verbatim (varType v)
      | otherwise
      = s <+> ": Found typed hole "
          <+> verbatim v
          <+> " (of type " <+> verbatim (varType v) <+> ")"
    prnt _ = undefined
  pprint (Note te m) = do
    te <+> newline
    block 1 . (newline <+>) $
      bullet (opClr "Note: ") <+> m
  pprint (CanNotInstance rec new)
    | (TyRows rho _ _) <- rec
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
    | TyExactRows ra _ <- ta
    , TyRows _ rb _ <- tb
    =   "No overlap between " <+> kwClr "exact" <+> " record " <+> verbatim ta
    <+> " and " <+> kwClr "polymorphic " <+> "record " <+> verbatim tb
    <+> block 1 (missing ra rb)
    | TyExactRows rb _ <- tb
    , TyRows _ ra _ <- ta
    =   "No overlap between " <+> kwClr "exact" <+> " record " <+> verbatim ta
    <+> " and " <+> kwClr "polymorphic " <+> "record " <+> verbatim tb
    <+> block 1 (missing ra rb)
    | TyExactRows ra _ <- ta
    , TyExactRows rb _ <- tb
    =   "No overlap between " <+> kwClr "exact" <+> " records " <+> verbatim ta
    <+> " and " <+> verbatim tb
    <+> block 1 (missing ra rb)
    | otherwise
    =   "\x1b[1;32minternal compiler error\x1b[0m: NoOverlap "
    <+> interleave " " [ta, tb] 

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
diff ra rb = (map (tvClr . fst) (deleteFirstsBy ((==) `on` fst) rb ra)) 

varType :: Var Typed -> Type Typed
varType (TvName _ x) = x
varType (TvRefresh v _) = varType v

report :: TypeError -> T.Text -> IO ()
report err _ = ppr $ do
  pprint err
