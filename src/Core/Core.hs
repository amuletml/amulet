{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, ScopedTypeVariables, DeriveFunctor #-}
module Core.Core where

import Pretty

import qualified Data.VarSet as VarSet
import Data.Generics hiding (empty)
import Data.Data (Data, Typeable)
import Data.Text (Text, pack)
import Data.Triple

import Syntax.Pretty (Var(..), Resolved)

data CoTerm a
  = CotRef a (CoType a)
  | CotLam Size (a, CoType a) (CoTerm a)
  | CotApp (CoTerm a) (CoTerm a) -- removes a λ

  | CotLet [(a, CoType a, CoTerm a)] (CoTerm a)
  | CotMatch (CoTerm a) [(CoPattern a, CoType a, CoTerm a)]

  | CotLit CoLiteral

  | CotExtend (CoTerm a) [(Text, CoType a, CoTerm a)]

  | CotTyApp (CoTerm a) (CoType a) -- removes a Λ
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
  | CotyForall [a] (CoType a)
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

parenFun :: Pretty a => CoTerm a -> Doc
parenFun f = case f of
  CotLam{} -> parens (pretty f)
  CotLet{} -> parens (pretty f)
  CotMatch{} -> parens (pretty f)
  CotApp{} -> parens (pretty f)
  _ -> pretty f

parenArg :: Pretty a => CoTerm a -> Doc
parenArg f = case f of
  CotTyApp{} -> parens (pretty f)
  _ -> parenFun f

instance Pretty a => Pretty (CoTerm a) where
  pretty (CotRef v _) = pretty v
  pretty (CotLam Big (v, t) c)
    = soperator (char 'Λ') <+> parens (pretty v <+> colon <+> pretty t) <> nest 2 (dot </> pretty c)
  pretty (CotLam Small (v, t) c)
    = soperator (char 'λ') <+> parens (pretty v <+> colon <+> pretty t) <> nest 2 (dot </> pretty c)
  pretty (CotApp f x) = parenFun f <+> parenArg x
  pretty (CotTyApp f t) = parenFun f <+> soperator (char '@') <> pretty t

  pretty (CotLet xs e) = keyword "let" <+> pprLet xs </> (keyword "in" <+> pretty e)
  pretty (CotLit l) = pretty l
  pretty (CotMatch e ps) = keyword "match" <+> pretty e <+> pprCases ps
  pretty (CotExtend x rs) = braces $ pretty x <+> pipe <+> prettyRows rs where
    prettyRows = hsep . punctuate comma . map (\(x, t, v) -> text x <+> colon <+> pretty t <+> equals <+> pretty v)

pprLet :: Pretty a => [(a, CoType a, CoTerm a)] -> Doc
pprLet = braces' . vsep . map (indent 2) . punctuate semi . map one where
  one (a, b, c) = pretty a <+> colon <+> pretty b <+> nest 2 (equals </> pretty c)

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
    = skeyword (char '∀') <+> hsep (map (stypeVar . (squote <>) . pretty) vs) <> dot <+> pretty v

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

{-# ANN freeIn "HLint: ignore" #-}
-- Rationale: can't use <> because of Doc. Ughr.
freeIn :: VarSet.IsVar a => CoTerm a -> VarSet.Set
freeIn (CotRef v _) = VarSet.singleton (VarSet.toVar v)
freeIn (CotLam Small (v, _) e) = VarSet.delete (VarSet.toVar v) (freeIn e)
freeIn (CotLam Big _ e) = freeIn e
freeIn (CotApp f x) = freeIn f `mappend` freeIn x
freeIn (CotLet vs e) = VarSet.difference (freeIn e `mappend` foldMap (freeIn . thd3) vs)
                                         (VarSet.fromList (map (VarSet.toVar . fst3) vs))
freeIn (CotMatch e bs) = freeIn e `mappend` foldMap freeInBranch bs where
  freeInBranch (b, _, e) = VarSet.difference (freeIn e) (bound b)
  bound (CopCapture v _) = VarSet.singleton (VarSet.toVar v)
  bound (CopDestr _ p) = bound p
  bound (CopExtend p ps) = foldMap (bound . snd) ps `mappend` bound p
  bound _ = mempty
freeIn (CotLit _) = mempty
freeIn (CotExtend c rs) = freeIn c `mappend` foldMap (freeIn . thd3) rs
freeIn (CotTyApp f _) = freeIn f

isError :: CoTerm (Var Resolved) -> Bool
isError (CotApp (CotTyApp (CotRef (TgInternal n) _) _) _) = n == pack "error"
isError _ = False

stripTyApp :: forall a. (Typeable a, Data a) => CoTerm a -> CoTerm a
stripTyApp = everywhere (mkT go) where
  go :: CoTerm a -> CoTerm a
  go (CotTyApp x _) = x
  go x = x
