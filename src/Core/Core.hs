{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, ScopedTypeVariables, DeriveFunctor #-}
module Core.Core where

import Pretty

import qualified Data.VarSet as VarSet
import Data.VarSet (IsVar(..))
import Data.Data (Data, Typeable)
import Data.Text (Text, pack)
import Data.Triple

import Syntax (Var(..), Resolved)

data Atom a
  = Ref a (Type a)
  | Lam Size (a, Type a) (Term a)
  | Lit Literal
  deriving (Eq, Show, Ord, Data, Typeable, Functor)

data Term a
  = Atom (Atom a)
  | App (Atom a) (Atom a) -- removes a λ

  | Let [(a, Type a, Term a)] (Term a)
  | Match (Atom a) [(Pattern a, Type a, Term a)]

  | Extend (Atom a) [(Text, Type a, Atom a)]

  | TyApp (Atom a) (Type a) -- removes a Λ
  | Cast (Atom a) (Coercion a)
  deriving (Eq, Show, Ord, Data, Typeable, Functor)

data Pattern a
  = Capture a (Type a)
  | Constr a
  | Destr a (Pattern a)
  | PatExtend (Pattern a) [(Text, Pattern a)]

  | PatLit Literal
  deriving (Eq, Show, Ord, Data, Typeable, Functor)

data Coercion a
  = SameRepr (Type a) (Type a)
  | Domain (Coercion a)
  | Codomain (Coercion a)
  | Symmetry (Coercion a)
  deriving (Eq, Show, Ord, Data, Typeable, Functor)

data Literal
  = Int Integer
  | Str Text
  | LitTrue | LitFalse
  | Unit | RecNil
  deriving (Eq, Show, Ord, Data, Typeable)

data Type a
  = ConTy a
  | VarTy a
  | ForallTy a (Type a)
  | ArrTy (Type a) (Type a)
  | AppTy (Type a) (Type a)
  | RowsTy (Type a) [(Text, Type a)]
  | ExactRowsTy [(Text, Type a)]
  | StarTy -- * :: *
  deriving (Eq, Show, Ord, Data, Typeable, Functor)

data Size
  = Big | Small
  deriving (Eq, Show, Ord, Data, Typeable)

data Stmt a
  = Foreign a (Type a) Text
  | StmtLet [(a, Type a, Term a)]
  | Type a [(a, Type a)]
  deriving (Eq, Show, Ord, Data, Typeable, Functor)

instance Pretty a => Pretty (Atom a) where
  pretty (Ref v _) = pretty v
  pretty (Lam Big (v, t) c)
    = soperator (char 'Λ') <+> parens (pretty v <+> colon <+> pretty t) <> nest 2 (dot </> pretty c)
  pretty (Lam Small (v, t) c)
    = soperator (char 'λ') <+> parens (pretty v <+> colon <+> pretty t) <> nest 2 (dot </> pretty c)
  pretty (Lit l) = pretty l

instance Pretty a => Pretty (Term a) where
  pretty (Atom a) = pretty a
  pretty (App f x) = pretty f <+> pretty x
  pretty (TyApp f t) = pretty f <+> soperator (char '@') <> pretty t

  pretty (Let [x] e) = keyword "let" <+> braces (space <> pprLet1 x <> space) <+> keyword "in" <#> pretty e
  pretty (Let xs e) = keyword "let" <+> pprLet xs </> (keyword "in" <+> pretty e)
  pretty (Match e ps) = keyword "match" <+> pretty e <+> pprCases ps
  pretty (Extend x rs) = braces $ pretty x <+> pipe <+> prettyRows rs where
    prettyRows = hsep . punctuate comma . map (\(x, t, v) -> text x <+> colon <+> pretty t <+> equals <+> pretty v)
  pretty (Cast a phi) = parens $ pretty a <+> soperator (string "|>") <+> pretty phi

instance Pretty a => Pretty (Coercion a) where
  pretty (SameRepr a b) = pretty a <+> soperator (char '~') <+> pretty b
  pretty (Domain f) = keyword "dom" <+> parens (pretty f)
  pretty (Codomain f) = keyword "cod" <+> parens (pretty f)
  pretty (Symmetry f) = keyword "sym" <+> parens (pretty f)

pprLet :: Pretty a => [(a, Type a, Term a)] -> Doc
pprLet = braces' . vsep . map (indent 2) . punctuate semi . map pprLet1

pprLet1 :: Pretty a => (a, Type a, Term a) -> Doc
pprLet1 (a, b, c) = pretty a <+> colon <+> pretty b <+> nest 2 (equals </> pretty c)

pprBegin :: [Doc] -> Doc
pprBegin = braces' . vsep . map (indent 2) . punctuate semi

pprCases :: Pretty a => [(Pattern a, Type a, Term a)] -> Doc
pprCases = braces' . vsep . map (indent 2) . punctuate semi . map one where
  one (a, b, c) = pretty a <+> colon <+> pretty b <+> nest 2 (arrow </> pretty c)

braces' :: Doc -> Doc
braces' = enclose (lbrace <> linebreak) (linebreak <> rbrace)

instance Pretty a => Pretty (Pattern a) where
  pretty (Capture v _) = pretty v
  pretty (Constr v) = pretty v
  pretty (Destr v p) = parens (pretty v <+> pretty p)
  pretty (PatExtend p rs) = braces $ pretty p <+> pipe <+> prettyRows rs where
    prettyRows = hsep . punctuate comma . map (\(x, v) ->
      text x <+> equals <+> pretty v)
  pretty (PatLit l) = pretty l

instance Pretty a => Pretty (Type a) where
  pretty (ConTy v) = stypeCon (pretty v)
  pretty (VarTy v) = stypeVar (squote <> pretty v)
  pretty (ForallTy vs v)
    = skeyword (char '∀') <+> stypeVar (pretty vs) <> dot <+> pretty v

  pretty (ArrTy x e)
    | ArrTy{} <- x = parens (pretty x) <+> arrow <+> pretty e
    | ForallTy{} <- x = parens (pretty x) <+> arrow <+> pretty e
    | otherwise = pretty x <+> arrow <+> pretty e

  pretty (RowsTy p rows) = braces $ pretty p <+> pipe <+> prettyRows rows where
    prettyRows = hsep . punctuate comma . map (\(x, t) -> text x <+> colon <+> pretty t)

  pretty (ExactRowsTy rows) = braces $ prettyRows rows where
    prettyRows = hsep . punctuate comma . map (\(x, t) -> text x <+> colon <+> pretty t)

  pretty (AppTy e x@AppTy{}) = pretty e <+> parens (pretty x)
  pretty (AppTy x e) = pretty x <+> pretty e
  pretty StarTy = prod

instance Pretty Literal where
  pretty LitFalse = sliteral (string "false")
  pretty LitTrue = sliteral (string "true")
  pretty Unit = sliteral (string "unit")
  pretty RecNil = sliteral (braces empty)
  pretty (Int l) = sliteral (integer l)
  pretty (Str s) = sstring (dquotes (text s))

instance Pretty a => Pretty (Stmt a) where
  pretty (Foreign v t _) = pretty v <+> colon <+> pretty t <+> equals <+> keyword "foreign"
  pretty (StmtLet vs) = keyword "let" <+> pprLet vs
  pretty (Type v cs) = keyword "type" <+> pretty v <+> pprBegin (map pprCons cs) where
    pprCons (x, t) = pretty x <+> colon <+> pretty t

instance Pretty a => Pretty [Stmt a] where
  pretty = vcat . map pretty

freeInAtom :: IsVar a => Atom a -> VarSet.Set
freeInAtom (Ref v _) = VarSet.singleton (VarSet.toVar v)
freeInAtom (Lam Small (v, _) e) = VarSet.delete (VarSet.toVar v) (freeIn e)
freeInAtom (Lam Big _ e) = freeIn e
freeInAtom (Lit _) = mempty

freeIn :: IsVar a => Term a -> VarSet.Set
freeIn (Atom a) = freeInAtom a
freeIn (App f x) = freeInAtom f <> freeInAtom x
freeIn (Let vs e) = VarSet.difference (freeIn e <> foldMap (freeIn . thd3) vs)
                                         (VarSet.fromList (map (VarSet.toVar . fst3) vs))
freeIn (Match e bs) = freeInAtom e <> foldMap freeInBranch bs where
  freeInBranch (b, _, e) = VarSet.difference (freeIn e) (patternVars b)
freeIn (Extend c rs) = freeInAtom c <> foldMap (freeInAtom . thd3) rs
freeIn (TyApp f _) = freeInAtom f
freeIn (Cast f _) = freeInAtom f

occursInAtom :: IsVar a => a -> Atom a -> Bool
occursInAtom v (Ref v' _) = toVar v == toVar v'
occursInAtom _ (Lit _) = False
occursInAtom v (Lam _ _ b) = occursInTerm v b

occursInTerm :: IsVar a => a -> Term a -> Bool
occursInTerm v (Atom a) = occursInAtom v a
occursInTerm v (App f x) = occursInAtom v f || occursInAtom v x
occursInTerm v (TyApp f _) = occursInAtom v f
occursInTerm v (Cast f _) = occursInAtom v f
occursInTerm v (Let vs e) = any (occursInTerm v . thd3) vs || occursInTerm v e
occursInTerm v (Match e bs) = occursInAtom v e || any (occursInTerm v . thd3) bs
occursInTerm v (Extend e fs) = occursInAtom v e || any (occursInAtom v . thd3) fs

isError :: Atom (Var Resolved) -> Bool
isError (Ref (TgInternal n) _) = n == pack "error"
isError _ = False

patternVars :: VarSet.IsVar a => Pattern a -> VarSet.Set
patternVars (Capture v _) = VarSet.singleton (VarSet.toVar v)
patternVars (Destr _ p) = patternVars p
patternVars (PatExtend p ps) = foldMap (patternVars . snd) ps <> patternVars p
patternVars Constr{} = mempty
patternVars PatLit{} = mempty

patternVarsA :: (Monoid (m a), Applicative m) => Pattern a -> m a
patternVarsA (Capture v _) =  pure v
patternVarsA (Destr _ p) = patternVarsA p
patternVarsA (PatExtend p ps) = mconcat (patternVarsA p : map (patternVarsA . snd) ps)
patternVarsA Constr{} = mempty
patternVarsA PatLit{} = mempty

relates :: Coercion a -> Maybe (Type a, Type a)
relates (SameRepr a b) = Just (a, b)
relates (Symmetry x) = do
  (a, b) <- relates x
  pure (b, a)
relates (Domain x) = do
  (ArrTy a _, ArrTy b _) <- relates x
  pure (a, b)
relates (Codomain x) = do
  (ArrTy _ a, ArrTy _ b) <- relates x
  pure (a, b)
