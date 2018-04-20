{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, DeriveFunctor, DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}
module Core.Core where

import Pretty

import qualified Data.VarSet as VarSet
import qualified Data.Set as Set
import Data.VarSet (IsVar(..))
import Data.Text (Text, pack)
import Data.Triple

import Control.Lens.Plated

import GHC.Generics

import Syntax (Var(..), Resolved)

data AnnAtom b a
  = Ref a (Type a)
  | Lam (Argument a) (AnnTerm b a)
  | Lit Literal
  deriving (Eq, Show, Ord, Functor, Generic)

type Atom = AnnAtom ()

data Argument a
  = TermArgument a (Type a)
  | TypeArgument a (Type a) -- TODO kinds
  deriving (Eq, Show, Ord, Functor, Generic)

data AnnTerm b a
  = AnnAtom b (AnnAtom b a)
  | AnnApp b (AnnAtom b a) (AnnAtom b a) -- removes a λ

  | AnnLet b (AnnBinding b a) (AnnTerm b a)
  | AnnMatch b (AnnAtom b a) [(Pattern a, Type a, AnnTerm b a)]

  | AnnExtend b (AnnAtom b a) [(Text, Type a, AnnAtom b a)]

  | AnnTyApp b (AnnAtom b a) (Type a) -- removes a Λ
  | AnnCast b (AnnAtom b a) (Coercion a)
  deriving (Eq, Show, Ord, Functor, Generic)

type Term = AnnTerm ()

{-# COMPLETE Atom, App, Let, Match, Extend, TyApp, Cast #-}

pattern Atom :: Atom a -> Term a
pattern Atom a = AnnAtom () a

pattern App :: Atom a -> Atom a -> Term a
pattern App f x = AnnApp () f x

pattern Let :: Binding a -> Term a -> Term a
pattern Let b r = AnnLet () b r

pattern Match :: Atom a -> [(Pattern a, Type a, Term a)] -> Term a
pattern Match t b = AnnMatch () t b

pattern Extend :: Atom a -> [(Text, Type a, Atom a)] -> Term a
pattern Extend f fs = AnnExtend () f fs

pattern TyApp :: Atom a -> Type a -> Term a
pattern TyApp f x = AnnTyApp () f x

pattern Cast :: AnnAtom () a -> Coercion a -> Term a
pattern Cast a ty = AnnCast () a ty

data AnnBinding b a
  = One (a, Type a, AnnTerm b a) -- uncurried for convenience
  | Many [(a, Type a, AnnTerm b a)]
  deriving (Eq, Show, Ord, Functor, Generic)

type Binding = AnnBinding ()

data Pattern a
  = Capture a (Type a)
  | Constr a
  | Destr a (Pattern a)
  | PatExtend (Pattern a) [(Text, Pattern a)]

  | PatLit Literal
  deriving (Eq, Show, Ord, Functor, Generic)

data Coercion a
  = SameRepr (Type a) (Type a)
  | Symmetry (Coercion a)

  | Application (Coercion a) (Coercion a)
  | Quantified (BoundTv a) (Coercion a) (Coercion a)
  | ExactRecord [(Text, Coercion a)]
  | Record (Coercion a) [(Text, Coercion a)]

  | CoercionVar a

  | Domain (Coercion a)
  | Codomain (Coercion a)
  deriving (Eq, Show, Ord, Functor, Generic)

data Literal
  = Int Integer
  | Str Text
  | Float Double
  | LitTrue | LitFalse
  | Unit | RecNil
  deriving (Eq, Show, Ord)

data Type a
  = ConTy a
  | VarTy a
  | ForallTy (BoundTv a) (Type a) (Type a)
  | AppTy (Type a) (Type a)
  | RowsTy (Type a) [(Text, Type a)]
  | ExactRowsTy [(Text, Type a)]
  | StarTy -- * :: *
  deriving (Eq, Show, Ord, Functor, Generic)

data BoundTv a = Irrelevant | Relevant a
  deriving (Eq, Show, Ord, Functor, Generic)

data AnnStmt b a
  = Foreign a (Type a) Text
  | StmtLet [(a, Type a, AnnTerm b a)]
  | Type a [(a, Type a)]
  deriving (Eq, Show, Ord, Functor, Generic)

type Stmt = AnnStmt ()

instance Pretty a => Pretty (Atom a) where
  pretty (Ref v ty) = pretty v <> scomment (string ":[" <> pretty ty <> string "]")
  pretty (Lam (TypeArgument v t) c)
    = soperator (char 'Λ') <+> parens (pretty v <+> colon <+> pretty t) <> nest 2 (dot </> pretty c)
  pretty (Lam (TermArgument v t) c)
    = soperator (char 'λ') <+> parens (pretty v <+> colon <+> pretty t) <> nest 2 (dot </> pretty c)
  pretty (Lit l) = pretty l

instance Pretty a => Pretty (Term a) where
  pretty (Atom a) = pretty a
  pretty (App f x) = pretty f <+> pretty x
  pretty (TyApp f t) = pretty f <+> braces (pretty t)

  pretty (Let (One x) e) = keyword "let" <+> braces (space <> pprLet1 x <> space) <+> keyword "in" <#> pretty e
  pretty (Let (Many xs) e) = keyword "let rec" <+> pprLet xs </> (keyword "in" <+> pretty e)
  pretty (Match e ps) = keyword "match" <+> pretty e <+> pprCases ps
  pretty (Extend x rs) = braces $ pretty x <+> pipe <+> prettyRows rs where
    prettyRows = hsep . punctuate comma . map (\(x, t, v) -> text x <+> colon <+> pretty t <+> equals <+> pretty v)
  pretty (Cast a phi) = parens $ pretty a <+> soperator (string "|>") <+> pretty phi

instance Pretty a => Pretty (Coercion a) where
  pretty (SameRepr a b) = pretty a <+> soperator (char '~') <+> pretty b
  pretty (Application c c') = pretty c <+> pretty c'
  pretty (Record r s) = enclose (lbrace <> space) (space <> rbrace) (pretty r <+> hsep (punctuate comma (map pprCoRow s)))
  pretty (ExactRecord r) = enclose (lbrace <> space) (space <> rbrace) (hsep (punctuate comma (map pprCoRow r)))
  pretty (Domain f) = keyword "dom" <+> parens (pretty f)
  pretty (Codomain f) = keyword "cod" <+> parens (pretty f)
  pretty (Symmetry f) = keyword "sym" <+> parens (pretty f)
  pretty (Quantified (Relevant v) dom c) = keyword "∀" <> (pretty v <+> colon <+> pretty dom) <> dot <+> pretty c
  pretty (Quantified Irrelevant dom c) = pretty dom <+> arrow <+> pretty c
  pretty (CoercionVar x) = pretty x

pprLet :: Pretty a => [(a, Type a, Term a)] -> Doc
pprLet = braces' . vsep . map (indent 2) . punctuate semi . map pprLet1

pprLet1 :: Pretty a => (a, Type a, Term a) -> Doc
pprLet1 (a, b, c) = pretty a <+> colon <+> pretty b <+> nest 2 (equals </> pretty c)

pprBegin :: [Doc] -> Doc
pprBegin = braces' . vsep . map (indent 2) . punctuate semi

pprCases :: Pretty a => [(Pattern a, Type a, Term a)] -> Doc
pprCases = braces' . vsep . map (indent 2) . punctuate semi . map one where
  one (a, b, c) = pretty a <+> colon <+> pretty b <+> nest 2 (arrow </> pretty c)

pprCoRow :: Pretty a => (Text, Coercion a) -> Doc
pprCoRow (a, b) = text a <+> equals <+> pretty b

braces' :: Doc -> Doc
braces' = enclose (lbrace <> linebreak) (linebreak <> rbrace)

instance Pretty a => Pretty (Pattern a) where
  pretty (Capture v ty) = pretty v <> scomment (string ":[" <> pretty ty <> string "]")
  pretty (Constr v) = pretty v
  pretty (Destr v p) = parens (pretty v <+> pretty p)
  pretty (PatExtend p rs) = braces $ pretty p <+> pipe <+> prettyRows rs where
    prettyRows = hsep . punctuate comma . map (\(x, v) ->
      text x <+> equals <+> pretty v)
  pretty (PatLit l) = pretty l

instance Pretty a => Pretty (Type a) where
  pretty (ConTy v) = stypeCon (pretty v)
  pretty (VarTy v) = stypeVar (squote <> pretty v)
  pretty (ForallTy (Relevant vs) c v)
    = skeyword (char '∀') <+> parens (stypeVar (pretty vs) <+> colon <+> pretty c) <> dot <+> pretty v

  pretty (ForallTy Irrelevant x e)
    | ForallTy{} <- x = parens (pretty x) <+> arrow <+> pretty e
    | otherwise = pretty x <+> arrow <+> pretty e

  pretty (RowsTy p rows) = braces $ pretty p <+> pipe <+> prettyRows rows where
    prettyRows = hsep . punctuate comma . map (\(x, t) -> text x <+> colon <+> pretty t)

  pretty (ExactRowsTy rows) = braces $ prettyRows rows where
    prettyRows = hsep . punctuate comma . map (\(x, t) -> text x <+> colon <+> pretty t)

  pretty (AppTy e x) = pretty e <+> k x (pretty x) where
    k AppTy{} = parens
    k ForallTy{} = parens
    k _ = id
  pretty StarTy = prod

instance Pretty Literal where
  pretty LitFalse = sliteral (string "false")
  pretty LitTrue = sliteral (string "true")
  pretty Unit = sliteral (string "unit")
  pretty RecNil = sliteral (braces empty)
  pretty (Float l) = sliteral (double l)
  pretty (Int l) = sliteral (integer l)
  pretty (Str s) = sstring (dquotes (text s))

instance Pretty a => Pretty (Stmt a) where
  pretty (Foreign v t _) = pretty v <+> colon <+> pretty t <+> equals <+> keyword "foreign"
  pretty (StmtLet vs) = keyword "let" <+> pprLet vs
  pretty (Type v cs) = keyword "type" <+> pretty v <+> pprBegin (map pprCons cs) where
    pprCons (x, t) = pretty x <+> colon <+> pretty t

instance Pretty a => Pretty [Stmt a] where
  pretty = vcat . map pretty

freeInAtom :: IsVar a => AnnAtom b a -> VarSet.Set
freeInAtom (Ref v _) = VarSet.singleton (VarSet.toVar v)
freeInAtom (Lam (TermArgument v _) e) = VarSet.delete (VarSet.toVar v) (freeIn e)
freeInAtom (Lam TypeArgument{} e) = freeIn e
freeInAtom (Lit _) = mempty

freeIn :: IsVar a => AnnTerm b a -> VarSet.Set
freeIn (AnnAtom _ a) = freeInAtom a
freeIn (AnnApp _ f x) = freeInAtom f <> freeInAtom x
freeIn (AnnLet _ (One v) e) = VarSet.difference (freeIn e <> freeIn (thd3 v)) (VarSet.singleton (toVar (fst3 v)))
freeIn (AnnLet _ (Many vs) e) = VarSet.difference (freeIn e <> foldMap (freeIn . thd3) vs) (VarSet.fromList (map (VarSet.toVar . fst3) vs))
freeIn (AnnMatch _ e bs) = freeInAtom e <> foldMap freeInBranch bs where
  freeInBranch (b, _, e) = VarSet.difference (freeIn e) (patternVars b)
freeIn (AnnExtend _ c rs) = freeInAtom c <> foldMap (freeInAtom . thd3) rs
freeIn (AnnTyApp _ f _) = freeInAtom f
freeIn (AnnCast _ f _) = freeInAtom f

freeInTy :: Ord a => Type a -> Set.Set a
freeInTy (VarTy v) = Set.singleton v
freeInTy (ForallTy (Relevant v) a b) = freeInTy a <> (v `Set.delete` freeInTy b)
freeInTy (ForallTy Irrelevant a b) = freeInTy a <> freeInTy b
freeInTy (AppTy a b) = freeInTy a <> freeInTy b
freeInTy (RowsTy c rs) = foldMap (freeInTy . snd) rs <> freeInTy c
freeInTy (ExactRowsTy rs) = foldMap (freeInTy . snd) rs
freeInTy ConTy{} = mempty
freeInTy StarTy = mempty

occursInAtom :: IsVar a => a -> Atom a -> Bool
occursInAtom v (Ref v' _) = toVar v == toVar v'
occursInAtom _ (Lit _) = False
occursInAtom v (Lam _ b) = occursInTerm v b

occursInTerm :: IsVar a => a -> Term a -> Bool
occursInTerm v (Atom a) = occursInAtom v a
occursInTerm v (App f x) = occursInAtom v f || occursInAtom v x
occursInTerm v (TyApp f _) = occursInAtom v f
occursInTerm v (Cast f _) = occursInAtom v f
occursInTerm v (Let (One va) e) = occursInTerm v (thd3 va) || occursInTerm v e
occursInTerm v (Let (Many vs) e) = any (occursInTerm v . thd3) vs || occursInTerm v e
occursInTerm v (Match e bs) = occursInAtom v e || any (occursInTerm v . thd3) bs
occursInTerm v (Extend e fs) = occursInAtom v e || any (occursInAtom v . thd3) fs

occursInTy :: IsVar a => a -> Type a -> Bool
occursInTy _ (ConTy _) = False
occursInTy v (VarTy v') = toVar v == toVar v'
occursInTy v (ForallTy b k t)
  | Relevant v /= b = occursInTy v k || occursInTy v t
  | otherwise = occursInTy v k
occursInTy v (AppTy a b) = occursInTy v a || occursInTy v b
occursInTy v (RowsTy t rs) = occursInTy v t || any (occursInTy v . snd) rs
occursInTy v (ExactRowsTy rs) = any (occursInTy v . snd) rs
occursInTy _ StarTy = False

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

relates :: Show a => Coercion a -> Maybe (Type a, Type a)
relates (SameRepr a b) = Just (a, b)
relates (CoercionVar _) = Nothing
relates (Application f x) = do
  (f, g) <- relates f
  (x, y) <- relates x
  pure (AppTy f x, AppTy g y)

relates (ExactRecord rs) = do
  let go (t, c) = do
        (a, b) <- relates c
        pure ((t, a), (t, b))
  (a, b) <- unzip <$> traverse go rs
  pure (ExactRowsTy a, ExactRowsTy b)

relates (Record c rs) = do
  (p, p') <- relates c
  let go (t, c) = do
        (a, b) <- relates c
        pure ((t, a), (t, b))
  (a, b) <- unzip <$> traverse go rs
  pure (RowsTy p a, RowsTy p' b)

relates (Symmetry x) = do
  (a, b) <- relates x
  pure (b, a)
relates (Domain x) = do
  (ForallTy _ a _, ForallTy _ b _) <- relates x
  pure (a, b)
relates (Codomain x) = do
  (ForallTy _ _ a, ForallTy _ _ b) <- relates x
  pure (a, b)
relates (Quantified v co c) = do
  (a, b) <- relates c
  (c, d) <- relates co
  pure (ForallTy v c a, ForallTy v d b)

extractAnn :: AnnTerm b a -> b
extractAnn (AnnAtom b _)     = b
extractAnn (AnnApp b _ _)    = b
extractAnn (AnnLet b _ _)    = b
extractAnn (AnnMatch b _ _)  = b
extractAnn (AnnExtend b _ _) = b
extractAnn (AnnTyApp b _ _)  = b
extractAnn (AnnCast b _ _)   = b

instance Plated (AnnAtom b a) where
  plate = gplate

instance Plated (AnnTerm b a) where
  plate = gplate

instance Plated (AnnBinding b a) where
  plate = gplate

instance Plated (Pattern a) where
  plate = gplate

instance Plated (Coercion a) where
  plate = gplate

instance Plated (Type a) where
  plate = gplate

instance Plated (AnnStmt b a) where
  plate = gplate
