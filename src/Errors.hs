{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Errors where

import Control.Monad.Infer

import qualified Data.Text as T
import Data.Text (Text)
import Data.List

import Data.Spanned

import Data.Function

import qualified Control.Monad.Infer as I

import Pretty
import qualified Syntax.Resolve as R
import Syntax.Resolve
import Syntax

verbatim :: Pretty a => a -> Doc
verbatim = enclose (char '`') (char '`') . pretty

bullet :: Doc -> Doc
bullet = (char '·' <+>)

instance Pretty TypeError where
  pretty (NotEqual a b) = string "Type error: failed to" <+> align (string "unify" <+> verbatim a </> string "with" <+> verbatim b)
  pretty (KindsNotEqual a b) = string "Kind error: failed to" <+> align (string "unify" <+> verbatim a </> string "with" <+> verbatim b)
  pretty (Occurs v t) = string "Occurs check: Variable" <+> align (verbatim v </> string "occurs in" <+> verbatim t)
  pretty (I.NotInScope e) = string "Variable not in scope:" <+> verbatim e
  pretty (I.EmptyMatch e) = pretty (annotation e) <> string ": Empty match expression"
  pretty (I.EmptyBegin e) = pretty (annotation e) <> string ": Empty begin expression"
  pretty (I.ArisingFrom er ex) = pretty (annotation ex) <> colon </> pretty er <#> indent 2 (nest 4 (bullet (string "Arising from use of") </> pretty ex))
  pretty (FoundHole xs) = hsep (map prnt xs) where
    prnt :: Expr Typed -> Doc
    prnt (Hole v s)
      = pretty (fst s) <> string ": Found typed hole" <+> verbatim v <+> parens (string "of type " <+> verbatim (snd s))
    prnt _ = undefined
  pretty (Note te m) = pretty te <+> indent 2 (bullet (string "Note: ") <+> align (pretty m))
  pretty (Suggestion te m) = pretty te <+> indent 2 (bullet (string "Suggestion: ") <+> align (pretty m))
  pretty (CanNotInstance rec new) = string "Can not instance hole of record type" <+> align (verbatim rec </> string " to type " <+> verbatim new)
  pretty (Malformed tp) = string "The type" <+> verbatim tp <+> string "is malformed."
  pretty (NoOverlap ta tb)
    | TyExactRows ra <- ta
    , TyRows _ rb <- tb
    =   string "No overlap between exact record" </> (verbatim ta <+> string "and polymorphic record" <+> verbatim tb)
    <#> indent 1 (missing ra rb)
    | TyExactRows ra <- tb
    , TyRows _ rb <- ta
    =   string "No overlap between exact record" </> (verbatim ta <+> string "and polymorphic record" <+> verbatim tb)
    <#> indent 1 (missing ra rb)
    | TyExactRows ra <- ta
    , TyExactRows rb <- tb
    =   string "No overlap between extact records " </> (verbatim ta <+> string "and" <+> verbatim tb)
    <+> indent 1 (missing ra rb)
    | otherwise
    = string "\x1b[1;32minternal compiler error\x1b[0m: NoOverlap" <+> verbatim ta <+> verbatim tb
  pretty (IllegalTypeApp ex ta _)
    = hsep [ string "Illegal type application " <+> verbatim ex
           , indent 2 (bullet (string "because of type ") <+> verbatim ta)
           ]
  pretty (EscapedSkolems esc _) =
    case esc of
      [Skolem var u ty] ->
        string "Skolem type constant "
        </> verbatim var <+> string "has escaped its scope of" <+> verbatim ty
            <+> bullet (string "Note: ") <+> verbatim var <+> string "stands for the type variable" <+> pretty (TyVar u)
      _ -> error (show esc)

  pretty (SkolBinding (Skolem _ x _) (TySkol (Skolem _ y _))) = pretty (NotEqual (TyVar x) (TyVar y))
  pretty (SkolBinding (Skolem a _ _) b) = string "Can not unify skolem type constant" <+> verbatim a <+> string "with type" <+> verbatim b

instance Pretty ResolveError where
  pretty (R.NotInScope e) = string "Variable not in scope:" <> verbatim e
  pretty (R.EmptyMatch e) = pretty (annotation e) <> string ": Empty match expression"
  pretty (R.EmptyBegin e) = pretty (annotation e) <> string ": Empty begin expression"
  pretty (R.ArisingFrom er ex) = pretty (annotation ex) <> colon <+> pretty er <#> indent 2 (bullet (string "Arising from use of ") <+> verbatim ex)
  pretty (R.ArisingFromTop er ex) = pretty (annotation ex) <> colon <+> pretty er <#> indent 2 (bullet (string "Arising in") <+> verbatim ex)

prettyRows :: Pretty (Var p) => [(T.Text, Type p)] -> Doc
prettyRows = braces . hsep . punctuate comma . map (\(x, y) -> string (T.unpack x) <+> colon <+> pretty y)


missing :: [(Text, b)] -> [(Text, b)] -> Doc
missing ra rb
  | length ra < length rb
  =  bullet (string "Namely, the following fields are missing:") <+> hsep (punctuate comma (diff ra rb))
  | length ra > length rb
  =  bullet (string "Namely, the following fields should not be present:") <+> hsep (punctuate comma (diff ra rb))
  | length ra == length rb
  = indent 2 . hsep $  [ bullet (string "Note: no fields match")
                       , bullet (string "The following fields are missing:")
                         <+> hsep (punctuate comma (diff ra rb))]
missing _ _ = undefined -- freaking GHC

diff :: [(Text, b)] -> [(Text, b)] -> [Doc]
diff ra rb = map ((squote <>) . string . T.unpack . fst) (deleteFirstsBy ((==) `on` fst) rb ra)

report :: Pretty p => p -> T.Text -> IO ()
report err _ = putDoc (pretty err)
