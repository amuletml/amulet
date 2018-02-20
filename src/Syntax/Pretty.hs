{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
module Syntax.Pretty() where

import Text.PrettyPrint.Leijen

import Prelude hiding ((<$>))

import qualified Data.Text as T
import Data.Text (Text)
import Data.Span

import Syntax

instance (Pretty (Var p)) => Pretty (Expr p) where
  pretty (VarRef v _) = pretty v
  pretty (Let [] _ _) = error "absurd: never parsed"
  pretty (Let ((n, v, _):xs) e _) =
    let prettyBind (n, v, _) = string "and" <+> pretty n <+> nest 2 (equals </> pretty v)
     in align $ string "let" <+> pretty n <+> nest 2 (equals </> pretty v)
            <$> case xs of
              [] -> string "in" <+> pretty e
              _ -> vsep (map prettyBind xs) <$> string "in" <+> pretty e
  pretty (If c t e _) = string "if" <+> pretty c
                    <$> indent 2 (vsep [ string "then" <+> pretty t
                                       , string "else" <+> pretty e
                                       ])
  pretty (App c (e@App{}) _) = pretty c <+> parens (pretty e)
  pretty (App c (e@Fun{}) _) = pretty c <+> parens (pretty e)
  pretty (App f x _) = pretty f <+> pretty x
  pretty (Fun v e _) = string "fun" <+> pretty v <+> nest 2 (string "->" </> pretty e)
  pretty (Begin e _) =
    vsep [ string "begin", indent 2 (vsep (punctuate semi (map pretty e))), string "end" ]
  pretty (Literal l _) = pretty l
  pretty (BinOp l o r _) = parens (pretty l <+> pretty o <+> pretty r)
  pretty (Match t bs _) = vsep ((string "match" <+> pretty t <+> string "with"):prettyMatches bs)
  pretty (Hole v _) = pretty v -- A typed hole
  pretty (Ascription e t _) = parens $ pretty e <+> colon <+> pretty t
  pretty (Record rows _) = record (map (\(n, v) -> text (T.unpack n) <+> equals <+> pretty v) rows)
  pretty (RecordExt var rows _) = braces $ pretty var <> text "with" <> hsep (punctuate comma (prettyRows rows))
  pretty (Access x@VarRef{} f _) = pretty x <> dot <> text (T.unpack f)
  pretty (Access e f _) = parens (pretty e) <> dot <> text (T.unpack f)

  pretty (LeftSection op vl _) = parens $ pretty op <+> pretty vl
  pretty (RightSection op vl _) = parens $ pretty vl <+> pretty op
  pretty (BothSection op _) = parens $ pretty op
  pretty (AccessSection k _) = parens $ dot <> text (T.unpack k)

  pretty (Tuple es _) = tupled (map pretty es)
  pretty (TypeApp f x _) = pretty f <+> text "@" <> pretty x

prettyMatches :: Pretty (Var p) => [(Pattern p, Expr p)] -> [Doc]
prettyMatches = map (\(a, b) -> char '|' <+> pretty a <+> string "->" <+> pretty b)

prettyRows :: Pretty x => [(Text, x)] -> [Doc]
prettyRows = map (\(n, v) -> text (T.unpack n) <+> equals <+> pretty v)

instance (Pretty (Var p)) => Pretty (Pattern p) where
  pretty Wildcard{} = char '_'
  pretty (Capture x _) = pretty x
  pretty (Destructure x Nothing   _) = pretty x
  pretty (Destructure x (Just xs) _) = parens $ pretty x <+> pretty xs
  pretty (PType p x _) = parens $ pretty p <+> colon <+> pretty x
  pretty (PRecord rows _) = record (prettyRows rows)
  pretty (PTuple ps _) = tupled (map pretty ps)

instance Pretty Lit where
  pretty (LiStr s) = dquotes (text (T.unpack s))
  pretty (LiInt s) = integer s
  pretty (LiBool True) = text "true"
  pretty (LiBool False) = text "false"
  pretty LiUnit = text "unit"

instance (Pretty (Var p)) => Pretty (Type p) where
  pretty (TyCon v) = pretty v
  pretty (TyVar v) = squote <> pretty v
  pretty (TySkol (Skolem v _ _)) = dquote <> pretty v
  pretty (TyForall vs v)
    = text "forall" <+> hsep (map ((squote <>) . pretty) vs) <> dot <+> pretty v

  pretty (TyArr x e)
    | TyArr{} <- x = parens (pretty x) <+> text "->" <+> pretty e
    | TyForall{} <- x = parens (pretty x) <+> text "->" <+> pretty e
    | TyTuple{} <- x = parens (pretty x) <+> text "->" <+> pretty e
    | otherwise = pretty x <+> text "->" <+> pretty e

  pretty (TyRows p rows) = braces $ pretty p <+> char '|' <+> hsep (punctuate comma (prettyRows rows)) 
  pretty (TyExactRows rows) = record (prettyRows rows)

  pretty (TyApp e x@TyApp{}) = pretty e <+> parens (pretty x)
  pretty (TyApp x e) = pretty x <+> pretty e
  pretty (TyTuple a b)
    | TyTuple{} <- a
    = parens (pretty a) <+> char '*' <+> pretty b
    | otherwise
    = pretty a <+> char '*' <+> pretty b

instance Pretty (Var p) => Pretty (Kind p) where
  pretty KiStar = text "Type"
  pretty (KiArr a b)
    | KiArr{} <- a = parens (pretty a) <+> text "->" <+> pretty b
    | otherwise = pretty a <+> text "->" <+> pretty b

  pretty (KiVar v) = squote <> pretty v
  pretty (KiForall vs v)
    = text "forall" <+> hsep (map ((squote <>) . pretty) vs) <> dot <+> pretty v

instance (Pretty (Var p)) => Pretty (Toplevel p) where
  pretty (LetStmt []) = error "absurd!"
  pretty (LetStmt ((n, v, _):xs)) =
    let prettyBind (n, v, _) = string "and" <+> pretty n <+> nest 2 (equals </> pretty v)
     in align $ string "let" <+> pretty n <+> nest 2 (equals </> pretty v)
            <$> vsep (map prettyBind xs)
  pretty (ForeignVal v d ty _) = text "foreign val" <+> pretty v <+> colon <+> pretty ty <+> equals <+> dquotes (text (T.unpack d))
  pretty (TypeDecl ty args ctors) = text "type" <+> pretty ty
                                <+> hsep (map ((squote <>) . pretty) args)
                                <+> equals
                                <$> vsep (map ((char '|' <+>) . pretty) ctors)

  pretty (Open m Nothing) = text "open" <+> pretty m
  pretty (Open m (Just a)) = text "open" <+> pretty m <+> text "as" <+> pretty a

  pretty (Module m bod) =
    vsep [ text "module" <+> pretty m <+> equals <+> text "begin"
         , indent 2 (align (pretty bod))
         , text "end"
         ]

instance (Pretty (Var p)) => Pretty [Toplevel p] where
  pretty = vsep . map pretty

instance (Pretty (Var p)) => Pretty (Constructor p) where
  pretty (UnitCon p _) = pretty p
  pretty (ArgCon p t _) = pretty p <+> text "of" <+> pretty t

instance Pretty (Var Parsed) where
  pretty (Name v) = text (T.unpack v)
  pretty (InModule t v) = text (T.unpack t) <> dot <> pretty v

instance Pretty (Var Resolved) where
  pretty (TgName v _) = text (T.unpack v)
  -- pretty (TgName v i) = pretty v <+> "#" <+> i
  pretty (TgInternal v) = text (T.unpack v)

instance Pretty (Var Typed) where
  pretty (TvName v) = pretty v
  -- pretty (TvName v t)
    -- | t == internalTyVar = pretty v
    -- | otherwise = parens $ v <+> opClr " : " <+> t
    --
instance Pretty (Span, Type Typed) where
  pretty (x, _) = pretty x

record :: [Doc] -> Doc
record = encloseSep lbrace rbrace comma
