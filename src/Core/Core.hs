{-# LANGUAGE DeriveDataTypeable, FlexibleInstances #-}
module Core.Core where

import Text.PrettyPrint.Leijen

import qualified Data.VarSet as VarSet
import Data.Generics hiding (empty)
import Data.Data (Data, Typeable)
import Data.Text (Text, pack, unpack)
import Data.Triple

import Syntax (Var(..), Resolved)

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
  = CopCapture (Var Resolved) CoType
  | CopConstr (Var Resolved)
  | CopDestr (Var Resolved) CoPattern
  | CopExtend CoPattern [(Text, CoPattern)]

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
  deriving (Eq, Show, Ord, Data, Typeable)

data CoStmt
  = CosForeign (Var Resolved) CoType Text
  | CosLet [(Var Resolved, CoType, CoTerm)]
  | CosType (Var Resolved) [(Var Resolved, CoType)]
  deriving (Eq, Show, Ord, Data, Typeable)

instance Pretty CoTerm where
  pretty (CotRef v _) = pretty v
  pretty (CotLam Big (v, t) c)
    = char 'Λ' <+> parens (pretty v <+> colon <+> pretty t) <> dot <+> pretty c
  pretty (CotLam Small (v, t) c)
    = char 'λ' <+> parens (pretty v <+> colon <+> pretty t) <> dot <+> pretty c
  pretty (CotApp f x) = parens (pretty f) <+> parens (pretty x)
  pretty (CotLet xs e) = text "let" </> pprLet xs </> (text "in" <+> pretty e)
  pretty (CotBegin e _) = text "begin" <+> braces (indent 2 (hsep (punctuate semi (map pretty e))))
  pretty (CotLit l) = pretty l
  pretty (CotMatch e ps) = text "match" <+> pretty e <+> pprCases ps
  pretty (CotTyApp f t) = pretty f <+> char '@' <> squotes (pretty t)
  pretty (CotExtend x rs) = braces $ pretty x <+> char '|' <+> prettyRows rs where
    prettyRows = hsep . punctuate comma . map (\(x, t, v) ->
      text (unpack x) <+> colon <+> pretty t <+> equals <+> pretty v)

pprLet :: [(Var Resolved, CoType, CoTerm)] -> Doc
pprLet xs = semiBraces (map one xs) where
  one (a, b, c) = pretty a <+> colon <+> pretty b <+> equals </> indent 2 (pretty c)

pprCases :: [(CoPattern, CoType, CoTerm)] -> Doc
pprCases xs = semiBraces (map one xs) where
  one (a, b, c) = pretty a <+> colon <+> pretty b <+> text "->" <+> pretty c

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
  pretty (CosForeign v t e) = pretty v <+> colon <+> pretty t <+> equals <+> text "foreign"
  pretty (CosLet vs) = text "let" <+> pprLet vs
  pretty (CosType v cs) = text "type" <+> pretty v <+> semiBraces (pprCons cs) where
    pprCons = map (\(x, t) -> pretty x <+> colon <+> pretty t)

instance Pretty [CoStmt] where
  pretty = vsep . map pretty

freeIn :: CoTerm -> VarSet.Set
freeIn (CotRef v _) = VarSet.singleton v
freeIn (CotLam Small (v, _) e) = VarSet.delete v (freeIn e)
freeIn (CotLam Big _ e) = freeIn e
freeIn (CotApp f x) = freeIn f `mappend` freeIn x
freeIn (CotLet vs e) = VarSet.difference (freeIn e `mappend` foldMap (freeIn . thd3) vs)
                                         (VarSet.fromList (map fst3 vs))
freeIn (CotMatch e bs) = freeIn e `mappend` foldMap freeInBranch bs where
  freeInBranch (b, _, e) = VarSet.difference (freeIn e) (bound b)
  bound (CopCapture v _) = VarSet.singleton v
  bound (CopDestr _ p) = bound p
  bound (CopExtend p ps) = foldMap (bound . snd) ps `mappend` bound p
  bound _ = mempty
freeIn (CotLit _) = mempty
freeIn (CotExtend c rs) = freeIn c `mappend` foldMap (freeIn . thd3) rs
freeIn (CotTyApp f _) = freeIn f
freeIn (CotBegin xs x) = foldMap freeIn xs `mappend` freeIn x

isError :: CoTerm -> Bool
isError (CotApp (CotTyApp (CotRef (TgInternal n) _) _) _) = n == pack "error"
isError _ = False

stripTyApp :: CoTerm -> CoTerm
stripTyApp = everywhere (mkT go) where
  go (CotTyApp x _) = x
  go x = x
