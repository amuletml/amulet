{-# LANGUAGE DeriveDataTypeable #-}
module Core.Core where

import Data.Text (Text)

import Syntax (Var(..), Resolved)

import Data.Data (Data, Typeable)

import Pretty

data CoTerm
  = CotRef (Var Resolved) CoType
  | CotLam Size (Var Resolved, CoType) CoTerm
  | CotApp CoTerm CoTerm -- removes a λ

  | CotLet [(Var Resolved, CoType, CoTerm)] CoTerm
  | CotMatch CoTerm [(CoPattern, CoType, CoTerm)]
  | CotBegin [CoTerm] CoTerm

  | CotLit CoLiteral

  | CotExtend CoTerm [(Text, CoType, CoTerm)]

  | CotTyApp CoTerm CoType -- removes a Λ
  deriving (Eq, Show, Ord, Data, Typeable)

data CoPattern
  = CopCapture (Var Resolved)
  | CopConstr (Var Resolved)
  | CopDestr (Var Resolved) CoPattern
  | CopRecord [(Text, CoPattern)]

  | CopLit CoLiteral
  deriving (Eq, Show, Ord, Data, Typeable)

data CoLiteral
  = ColInt Integer
  | ColStr Text
  | ColTrue | ColFalse
  | ColUnit | ColRecNil
  deriving (Eq, Show, Ord, Data, Typeable)

data CoType
  = CotyCon (Var Resolved)
  | CotyVar (Var Resolved)
  | CotyForall [Var Resolved] CoType
  | CotyArr CoType CoType
  | CotyApp CoType CoType
  | CotyRows CoType [(Text, CoType)]
  | CotyExactRows [(Text, CoType)]
  | CotyStar -- * :: *
  deriving (Eq, Show, Ord, Data, Typeable)

data Size
  = Big | Small
  deriving (Eq, Show, Ord, Read, Data, Typeable)

data CoStmt
  = CosForeign (Var Resolved) CoType Text
  | CosLet [(Var Resolved, CoType, CoTerm)]
  | CosType (Var Resolved) CoType [(Var Resolved, CoType)]
  deriving (Eq, Show, Ord, Data, Typeable)

instance Pretty CoTerm where
  pprint (CotRef v _) = pprint v
  pprint (CotLam Big (v, t) c)
    = opClr "Λ" <+> (v <+> opClr " : " <+> t) <+> opClr ". " <+> c
  pprint (CotLam Small (v, t) c)
    = opClr "λ" <+> (v <+> opClr " : " <+> t) <+> opClr ". " <+> c
  pprint (CotApp f x)
    | CotApp{} <- x = f <+> " " <+> parens x
    | CotLam{} <- x = f <+> " " <+> parens x
    | CotMatch{} <- x = f <+> " " <+> parens x
    | otherwise = f <+> " " <+> x
  pprint (CotLet xs e) = do
    kwClr "let " <+> braces (pprLet xs) <+> kwClr " in " <+> e
  pprint (CotBegin xs e) = kwClr "begin" <+> interleave (opClr "; ") (xs ++ [e]) <+> kwClr "end"
  pprint (CotLit l) = pprint l
  pprint (CotMatch e ps) = kwClr "match " <+> e <+> braces (pprCases ps)
  pprint (CotTyApp f t) = f <+> opClr " @" <+> t
  pprint (CotExtend x rs) = braces $ x <+> opClr " | " <+> prettyRows rs where
    prettyRows = interleave ", " . map (\(x, t, v) -> x <+> opClr " : "
                                                        <+> t
                                                        <+> opClr " = "
                                                        <+> v)


pprLet :: [(Var Resolved, CoType, CoTerm)] -> PrettyP
pprLet xs = interleave (opClr "; ") (map one xs) where
  one (a, b, c) = a <+> opClr " : " <+> b <+> opClr " = " <+> c

pprCases :: [(CoPattern, CoType, CoTerm)] -> PrettyP
pprCases xs = interleave (opClr "; ") (map one xs) where
  one (a, b, c) = a <+> opClr " : " <+> b <+> opClr " -> " <+> c

instance Pretty CoPattern where
  pprint (CopCapture v) = pprint v
  pprint (CopConstr v) = pprint v
  pprint (CopDestr v p) = parens (v <+> " " <+> p)
  pprint (CopRecord rs)
    = braces $ interleave ", " $ map (\(x, y) -> x <+> opClr " = " <+> y) rs
  pprint (CopLit l) = pprint l

instance Pretty CoType where
  pprint (CotyCon v) = typeClr v
  pprint (CotyVar v) = opClr "'" <+> tvClr v
  pprint (CotyForall vs v)
    = kwClr "∀ " <+> interleave " " (map (\x -> "'" <+> tvClr x) vs) <+> opClr ". " <+> v

  pprint (CotyArr x e)
    | CotyArr{} <- x = parens x <+> opClr " -> " <+> e
    | CotyForall{} <- x = parens x <+> opClr " -> " <+> e
    | otherwise = x <+> opClr " -> " <+> e
  pprint (CotyRows p rows) = braces $ p <+> opClr " | " <+> prettyRows rows where
    prettyRows = interleave ", " . map (\(x, t) -> x <+> opClr " : " <+> t)

  pprint (CotyExactRows rows) = braces $ prettyRows rows where
    prettyRows = interleave ", " . map (\(x, t) -> x <+> opClr " : " <+> t)

  pprint (CotyApp e x@CotyApp{}) = e <+> " " <+> parens x
  pprint (CotyApp x e) = x <+> opClr " " <+> e
  pprint CotyStar = kwClr "*"

instance Pretty CoLiteral where
  pprint ColFalse = kwClr "false"
  pprint ColTrue = kwClr "true"
  pprint ColUnit = kwClr "unit"
  pprint ColRecNil = kwClr "{}"
  pprint (ColInt l) = pprint l
  pprint (ColStr s) = str s

instance Pretty CoStmt where
  pprint (CosForeign v t e) = v <+> opClr " : " <+> t <+> kwClr " = foreign " <+> str e
  pprint (CosLet vs) = kwClr "let " <+> pprLet vs
  pprint (CosType v k cs) = kwClr "type " <+> v <+> opClr " : "
                                          <+> k <+> braces (pprCons cs)
    where pprCons = interleave (opClr "; ") . map (\(x, t) -> x <+> opClr " : " <+> t)
