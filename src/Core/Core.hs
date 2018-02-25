{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, ScopedTypeVariables, DeriveFunctor #-}
module Core.Core where

import Pretty

import qualified Data.VarSet as VarSet
import Data.Data (Data, Typeable)
import Data.Text (Text, pack)
import Data.Triple

import Syntax

data CoAtom a
  = CoaRef a (CoType a)
  | CoaLam Size (a, CoType a) (CoTerm a)
  | CoaLit CoLiteral
  deriving (Eq, Show, Ord, Data, Typeable, Functor)

data CoTerm a
  = CotAtom (CoAtom a)
  | CotApp (CoAtom a) (CoAtom a) -- removes a λ

  | CotLet [(a, CoType a, CoTerm a)] (CoTerm a)
  | CotMatch (CoAtom a) [(CoPattern a, CoType a, CoTerm a)]

  | CotExtend (CoAtom a) [(Text, CoType a, CoAtom a)]

  | CotTyApp (CoAtom a) (CoType a) -- removes a Λ
  deriving (Eq, Show, Ord, Data, Typeable, Functor)

data CoPattern a
  = CopCapture a (CoType a)
  | CopConstr a
  | CopDestr a (CoPattern a)
  | CopExtend (CoPattern a) [(Text, CoPattern a)]

  | CopLit CoLiteral
  deriving (Eq, Show, Ord, Data, Typeable, Functor)

data CoLiteral
  = ColInt Integer
  | ColStr Text
  | ColTrue | ColFalse
  | ColUnit | ColRecNil
  deriving (Eq, Show, Ord, Data, Typeable)

data CoType a
  = CotyCon a
  | CotyVar a
  | CotyForall a (CoType a)
  | CotyArr (CoType a) (CoType a)
  | CotyApp (CoType a) (CoType a)
  | CotyRows (CoType a) [(Text, CoType a)]
  | CotyExactRows [(Text, CoType a)]
  | CotyStar -- * :: *
  deriving (Eq, Show, Ord, Data, Typeable, Functor)

data Size
  = Big | Small
  deriving (Eq, Show, Ord, Data, Typeable)

data CoStmt a
  = CosForeign a (CoType a) Text
  | CosLet [(a, CoType a, CoTerm a)]
  | CosType a [(a, CoType a)]
  deriving (Eq, Show, Ord, Data, Typeable, Functor)

instance Pretty a => Pretty (CoAtom a) where
  pretty (CoaRef v _) = pretty v
  pretty (CoaLam Big (v, t) c)
    = soperator (char 'Λ') <+> parens (pretty v <+> colon <+> pretty t) <> nest 2 (dot </> pretty c)
  pretty (CoaLam Small (v, t) c)
    = soperator (char 'λ') <+> parens (pretty v <+> colon <+> pretty t) <> nest 2 (dot </> pretty c)
  pretty (CoaLit l) = pretty l

instance Pretty a => Pretty (CoTerm a) where
  pretty (CotAtom a) = pretty a
  pretty (CotApp f x) = pretty f <+> pretty x
  pretty (CotTyApp f t) = pretty f <+> soperator (char '@') <> pretty t

  pretty (CotLet [x] e) = keyword "let" <+> braces (space <> pprLet1 x <> space) <+> keyword "in" <#> pretty e
  pretty (CotLet xs e) = keyword "let" <+> pprLet xs </> (keyword "in" <+> pretty e)
  pretty (CotMatch e ps) = keyword "match" <+> pretty e <+> pprCases ps
  pretty (CotExtend x rs) = braces $ pretty x <+> pipe <+> prettyRows rs where
    prettyRows = hsep . punctuate comma . map (\(x, t, v) -> text x <+> colon <+> pretty t <+> equals <+> pretty v)

pprLet :: Pretty a => [(a, CoType a, CoTerm a)] -> Doc
pprLet = braces' . vsep . map (indent 2) . punctuate semi . map pprLet1

pprLet1 :: Pretty a => (a, CoType a, CoTerm a) -> Doc
pprLet1 (a, b, c) = pretty a <+> colon <+> pretty b <+> nest 2 (equals </> pretty c)

pprBegin :: [Doc] -> Doc
pprBegin = braces' . vsep . map (indent 2) . punctuate semi

pprCases :: Pretty a => [(CoPattern a, CoType a, CoTerm a)] -> Doc
pprCases = braces' . vsep . map (indent 2) . punctuate semi . map one where
  one (a, b, c) = pretty a <+> colon <+> pretty b <+> nest 2 (arrow </> pretty c)

braces' :: Doc -> Doc
braces' = enclose (lbrace <> linebreak) (linebreak <> rbrace)

instance Pretty a => Pretty (CoPattern a) where
  pretty (CopCapture v _) = pretty v
  pretty (CopConstr v) = pretty v
  pretty (CopDestr v p) = parens (pretty v <+> pretty p)
  pretty (CopExtend p rs) = braces $ pretty p <+> pipe <+> prettyRows rs where
    prettyRows = hsep . punctuate comma . map (\(x, v) ->
      text x <+> equals <+> pretty v)
  pretty (CopLit l) = pretty l

instance Pretty a => Pretty (CoType a) where
  pretty (CotyCon v) = stypeCon (pretty v)
  pretty (CotyVar v) = stypeVar (squote <> pretty v)
  pretty (CotyForall vs v)
    = skeyword (char '∀') <+> stypeVar (pretty vs) <> dot <+> pretty v

  pretty (CotyArr x e)
    | CotyArr{} <- x = parens (pretty x) <+> arrow <+> pretty e
    | CotyForall{} <- x = parens (pretty x) <+> arrow <+> pretty e
    | otherwise = pretty x <+> arrow <+> pretty e

  pretty (CotyRows p rows) = braces $ pretty p <+> pipe <+> prettyRows rows where
    prettyRows = hsep . punctuate comma . map (\(x, t) -> text x <+> colon <+> pretty t)

  pretty (CotyExactRows rows) = braces $ prettyRows rows where
    prettyRows = hsep . punctuate comma . map (\(x, t) -> text x <+> colon <+> pretty t)

  pretty (CotyApp e x@CotyApp{}) = pretty e <+> parens (pretty x)
  pretty (CotyApp x e) = pretty x <+> pretty e
  pretty CotyStar = prod

instance Pretty CoLiteral where
  pretty ColFalse = sliteral (string "false")
  pretty ColTrue = sliteral (string "true")
  pretty ColUnit = sliteral (string "unit")
  pretty ColRecNil = sliteral (braces empty)
  pretty (ColInt l) = sliteral (integer l)
  pretty (ColStr s) = sstring (dquotes (text s))

instance Pretty a => Pretty (CoStmt a) where
  pretty (CosForeign v t _) = pretty v <+> colon <+> pretty t <+> equals <+> keyword "foreign"
  pretty (CosLet vs) = keyword "let" <+> pprLet vs
  pretty (CosType v cs) = keyword "type" <+> pretty v <+> pprBegin (map pprCons cs) where
    pprCons (x, t) = pretty x <+> colon <+> pretty t

instance Pretty a => Pretty [CoStmt a] where
  pretty = vcat . map pretty

freeInAtom :: VarSet.IsVar a => CoAtom a -> VarSet.Set
freeInAtom (CoaRef v _) = VarSet.singleton (VarSet.toVar v)
freeInAtom (CoaLam Small (v, _) e) = VarSet.delete (VarSet.toVar v) (freeIn e)
freeInAtom (CoaLam Big _ e) = freeIn e
freeInAtom (CoaLit _) = mempty

freeIn :: VarSet.IsVar a => CoTerm a -> VarSet.Set
freeIn (CotAtom a) = freeInAtom a
freeIn (CotApp f x) = freeInAtom f <> freeInAtom x
freeIn (CotLet vs e) = VarSet.difference (freeIn e <> foldMap (freeIn . thd3) vs)
                                         (VarSet.fromList (map (VarSet.toVar . fst3) vs))
freeIn (CotMatch e bs) = freeInAtom e <> foldMap freeInBranch bs where
  freeInBranch (b, _, e) = VarSet.difference (freeIn e) (bound b)
  bound (CopCapture v _) = VarSet.singleton (VarSet.toVar v)
  bound (CopDestr _ p) = bound p
  bound (CopExtend p ps) = foldMap (bound . snd) ps <> bound p
  bound _ = mempty
freeIn (CotExtend c rs) = freeInAtom c <> foldMap (freeInAtom . thd3) rs
freeIn (CotTyApp f _) = freeInAtom f

isError :: CoAtom (Var Resolved) -> Bool
isError (CoaRef (TgInternal n) _) = n == pack "error"
isError _ = False
