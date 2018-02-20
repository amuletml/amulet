{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DeriveDataTypeable, FlexibleInstances #-}
module Core.Pretty where


import Text.PrettyPrint.Leijen

import Data.Text (unpack)

import Core.Core

import Syntax.Pretty()
import Syntax

instance Pretty CoTerm where
  pretty (CotRef v _) = pretty v
  pretty (CotLam Big (v, t) c)
    = char 'Λ' <+> parens (pretty v <+> colon <+> pretty t) <> nest 2 (dot </> pretty c)
  pretty (CotLam Small (v, t) c)
    = char 'λ' <+> parens (pretty v <+> colon <+> pretty t) <> nest 2 (dot </> pretty c)
  pretty (CotApp f x) = f' <+> x' where
    f' = case f of
      CotLam{} -> parens (pretty f)
      _ -> pretty f
    x' = case x of
      CotLam{} -> parens (pretty x)
      CotApp{} -> parens (pretty x)
      _ -> pretty x

  pretty (CotLet xs e) = text "let" <+> pprLet xs </> (text "in" <+> pretty e)
  pretty (CotBegin e x) = text "begin" <+> pprBegin (map pretty (e ++ [x]))
  pretty (CotLit l) = pretty l
  pretty (CotMatch e ps) = text "match" <+> pretty e <+> pprCases ps
  pretty (CotTyApp f t) = pretty f <+> char '@' <> squotes (pretty t)
  pretty (CotExtend x rs) = braces $ pretty x <+> char '|' <+> prettyRows rs where
    prettyRows = hsep . punctuate comma . map (\(x, t, v) ->
      text (unpack x) <+> colon <+> pretty t <+> equals <+> pretty v)

pprLet :: [(Var Resolved, CoType, CoTerm)] -> Doc
pprLet = braces' . vsep . map (indent 2) . punctuate semi . map one where
  one (a, b, c) = pretty a <+> colon <+> pretty b <+> nest 2 (equals </> pretty c)

pprBegin :: [Doc] -> Doc
pprBegin = braces' . vsep . map (indent 2) . punctuate semi

pprCases :: [(CoPattern, CoType, CoTerm)] -> Doc
pprCases = braces' . vsep . map (indent 2) . punctuate semi . map one where
  one (a, b, c) = pretty a <+> colon <+> pretty b <+> text "->" <+> pretty c

braces' :: Doc -> Doc
braces' = enclose (lbrace <> linebreak) (linebreak <> rbrace)

instance Pretty CoPattern where
  pretty (CopCapture v t) = parens (pretty v <+> colon <+> pretty t)
  pretty (CopConstr v) = pretty v
  pretty (CopDestr v p) = parens (pretty v <+> pretty p)
  pretty (CopExtend p rs) = braces $ pretty p <+> char '|' <+> prettyRows rs where
    prettyRows = hsep . punctuate comma . map (\(x, v) ->
      text (unpack x) <+> equals <+> pretty v)
  pretty (CopLit l) = pretty l

instance Pretty CoType where
  pretty (CotyCon v) = pretty v
  pretty (CotyVar v) = squote <> pretty v
  pretty (CotyForall vs v)
    = char '∀' <+> hsep (map ((squote <>) . pretty) vs) <> dot <+> pretty v

  pretty (CotyArr x e)
    | CotyArr{} <- x = parens (pretty x) <+> text "->" <+> pretty e
    | CotyForall{} <- x = parens (pretty x) <+> text "->" <+> pretty e
    | otherwise = pretty x <+> text "->" <+> pretty e

  pretty (CotyRows p rows) = braces $ pretty p <+> char '|' <+> prettyRows rows where
    prettyRows = hsep . punctuate comma . map (\(x, t) -> text (unpack x) <+> colon <+> pretty t)

  pretty (CotyExactRows rows) = braces $ prettyRows rows where
    prettyRows = hsep . punctuate comma . map (\(x, t) -> text (unpack x) <+> colon <+> pretty t)

  pretty (CotyApp e x@CotyApp{}) = pretty e <+> parens (pretty x)
  pretty (CotyApp x e) = pretty x <+> pretty e
  pretty CotyStar = char '*'

instance Pretty CoLiteral where
  pretty ColFalse = text "false"
  pretty ColTrue = text "true"
  pretty ColUnit = text "unit"
  pretty ColRecNil = braces empty
  pretty (ColInt l) = pretty l
  pretty (ColStr s) = dquotes (text (unpack s))

instance Pretty CoStmt where
  pretty (CosForeign v t _) = pretty v <+> colon <+> pretty t <+> equals <+> text "foreign"
  pretty (CosLet vs) = text "let" <+> pprLet vs
  pretty (CosType v cs) = text "type" <+> pretty v <+> pprBegin (map pprCons cs) where
    pprCons (x, t) = pretty x <+> colon <+> pretty t

instance Pretty [CoStmt] where
  pretty = vcat . map pretty
