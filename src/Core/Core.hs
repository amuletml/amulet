{-# LANGUAGE DeriveDataTypeable, FlexibleInstances #-}
module Core.Core where

import qualified Data.VarSet as VarSet
import Data.Generics
import Data.Data (Data, Typeable)
import Data.Text (Text, pack)
import Data.Triple

import Syntax (Var(..), Resolved)


import Pretty

data CoTerm
  = CotRef (Var Resolved) CoType
  | CotLam Size (Var Resolved, CoType) CoTerm

  | CotApp CoTerm CoTerm -- removes a λ
  | CotTyApp CoTerm CoType -- removes a Λ

  | CotLet [(Var Resolved, CoType, CoTerm)] CoTerm
  | CotMatch CoTerm [(CoPattern, CoType, CoTerm)]
  | CotBegin [CoTerm] CoTerm

  | CotLit CoLiteral

  | CotExtend CoTerm [(Text, CoType, CoTerm)]
  | CotAccess CoTerm Text
  deriving (Eq, Show, Ord, Data, Typeable)

data CoPattern
  = CopCapture (Var Resolved) CoType
  | CopConstr (Var Resolved)
  | CopDestr (Var Resolved) CoPattern

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
  pprint (CotRef v _) = pprint v
  pprint (CotLam Big (v, t) c)
    = opClr "Λ" <+> (v <+> opClr " : " <+> t) <+> opClr ". " <+> c
  pprint (CotLam Small (v, t) c)
    = opClr "λ" <+> (v <+> opClr " : " <+> t) <+> opClr ". " <+> c
  pprint (CotApp f x)
    | CotLam{} <- f = parens f <+> " " <+> parens x
    | CotLet{} <- f = parens f <+> " " <+> x
    | CotApp{} <- x = f <+> " " <+> parens x
    | CotLam{} <- x = f <+> " " <+> parens x
    | CotMatch{} <- x = f <+> " " <+> parens x
    | otherwise = f <+> " " <+> x
  pprint (CotLet xs e) =
    kwClr "let " <+> block 2 (braces (newline *> pprLet xs)) <+> kwClr " in " <+> e
  pprint (CotBegin xs e) = kwClr "begin " <+> interleave (opClr "; ") (xs ++ [e]) <+> kwClr " end"
  pprint (CotLit l) = pprint l
  pprint (CotMatch e ps) = kwClr "match " <+> e <+> " " <+> block 2 (braces (newline *> pprCases ps))
  pprint (CotTyApp f t) = f <+> opClr " @" <+> t
  pprint (CotExtend x rs) = braces $ x <+> opClr " | " <+> prettyRows rs where
    prettyRows = interleave ", " . map (\(x, t, v) ->
      x <+> opClr " : "
        <+> t
        <+> opClr " = "
        <+> v)
  pprint (CotAccess e k) = parens (pprint e) <+> opClr "." <+> k

pprLet :: [(Var Resolved, CoType, CoTerm)] -> PrettyP
pprLet xs = interleave newline (map one xs) where
  one (a, b, c) = do
    a <+> opClr " : " <+> b <+> opClr " = "
    block 2 (newline <* pprint c)

pprCases :: [(CoPattern, CoType, CoTerm)] -> PrettyP
pprCases xs = interleave newline (map one xs) where
  one (a, b, c) = a <+> opClr " : " <+> b <+> opClr " -> " <+> c

instance Pretty CoPattern where
  pprint (CopCapture v _) = pprint v
  pprint (CopConstr v) = pprint v
  pprint (CopDestr v p) = parens (v <+> " " <+> p)
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
  pprint (CosLet vs) = kwClr "let " <+> braces (pprLet vs)
  pprint (CosType v cs) = kwClr "type "
                        <+> v <+> " " <+> braces (pprCons cs)
    where pprCons = interleave (opClr "; ") . map (\(x, t) -> x <+> opClr " : " <+> t)

instance Pretty [CoStmt] where
  pprint = interleave ";"


freeIn :: CoTerm -> VarSet.Set
freeIn (CotRef v _) = VarSet.singleton v
freeIn (CotLam Small (v, _) e) = VarSet.delete v (freeIn e)
freeIn (CotLam Big _ e) = freeIn e
freeIn (CotApp f x) = freeIn f <> freeIn x
freeIn (CotLet vs e) = VarSet.difference (freeIn e <> foldMap (freeIn . thd3) vs)
                                         (VarSet.fromList (map fst3 vs))
freeIn (CotMatch e bs) = freeIn e <> foldMap freeInBranch bs where
  freeInBranch (b, _, e) = VarSet.difference (freeIn e) (bound b)
  bound (CopCapture v _) = VarSet.singleton v
  bound (CopDestr _ p) = bound p
  bound _ = mempty
freeIn (CotLit _) = mempty
freeIn (CotExtend c rs) = freeIn c <> foldMap (freeIn . thd3) rs
freeIn (CotTyApp f _) = freeIn f
freeIn (CotBegin xs x) = foldMap freeIn xs <> freeIn x
freeIn (CotAccess e _) = freeIn e

isError :: CoTerm -> Bool
isError (CotApp (CotTyApp (CotRef (TgInternal n) _) _) _) = n == pack "error"
isError _ = False

stripTyApp :: CoTerm -> CoTerm
stripTyApp = everywhere (mkT go) where
  go (CotTyApp x _) = x
  go x = x
