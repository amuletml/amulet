{-# LANGUAGE FlexibleInstances, FlexibleContexts, ScopedTypeVariables,
   DeriveFunctor, DeriveGeneric, PatternSynonyms, TemplateHaskell,
   DeriveAnyClass #-}
-- | Amulet's explicitly-typed intermediate representation
module Core.Core where

import Text.Pretty.Annotation
import Text.Pretty.Semantic

import qualified Data.VarSet as VarSet
import Data.Hashable
import Data.Function
import Data.Triple
import Data.Maybe
import Data.Text (Text)
import Data.List
import Core.Var

import Control.Lens

import GHC.Generics

-- | Atoms.
data Atom a
  = Ref a (Type a) -- ^ A reference to a variable, with an explicit type
  | Lit Literal -- ^ A literal.
  deriving (Eq, Show, Ord, Functor, Generic, Hashable)

-- | The domain of a lambda
data Argument a
  = TermArgument a (Type a) -- ^ Computationally-relevant domain
  | TypeArgument a (Type a) -- ^ Type abstraction, erased at Emit-time.
  deriving (Eq, Show, Ord, Functor, Generic, Hashable)

-- | Terms.
data AnnTerm b a
  = AnnAtom b (Atom a) -- ^ Embed an 'Atom' into a 'Term'
  | AnnApp b (Atom a) (Atom a) -- ^ Eliminate a 'Lam' expecting a 'TermArgument'
  | AnnLam b (Argument a) (AnnTerm b a) -- ^ A lambda abstraction, with explicit domain, encompassing a 'Term'.

  | AnnLet b (AnnBinding b a) (AnnTerm b a) -- ^ Bind some variables within the scope of some 'Term'
  | AnnMatch b (Atom a) [AnnArm b a] -- ^ Pattern matching

  | AnnExtend b (Atom a) [(Text, Type a, Atom a)] -- ^ Record extension
  | AnnValues b [Atom a] -- ^ Unboxed tuple

  | AnnTyApp b (Atom a) (Type a) -- ^ Eliminate a 'Lam' expecting a 'TypeArgument'
  | AnnCast b (Atom a) (Coercion a) -- ^ Cast an 'Atom' using some 'Coercion'.
  deriving (Eq, Show, Ord, Functor, Generic, Hashable)

-- | An 'AnnTerm' with '()' annotations.
type Term = AnnTerm ()

{-# COMPLETE Atom, App, Lam, Let, Match, Extend, Values, TyApp, Cast #-}

-- | Match an 'Atom' with '()' annotation
pattern Atom :: Atom a -> Term a
pattern Atom a = AnnAtom () a

-- | Match an 'App' with '()' annotation
pattern App :: Atom a -> Atom a -> Term a
pattern App f x = AnnApp () f x

-- | Match an 'App' with '()' annotation
pattern Lam :: Argument a -> Term a -> Term a
pattern Lam f x = AnnLam () f x

-- | Match a 'Let' with '()' annotation
pattern Let :: Binding a -> Term a -> Term a
pattern Let b r = AnnLet () b r

-- | Match a 'Match' with '()' annotation
pattern Match :: Atom a -> [Arm a] -> Term a
pattern Match t b = AnnMatch () t b

-- | Match an 'Extend' with '()' annotation
pattern Extend :: Atom a -> [(Text, Type a, Atom a)] -> Term a
pattern Extend f fs = AnnExtend () f fs

-- | Match an 'Extend' with '()' annotation
pattern Values :: [Atom a] -> Term a
pattern Values xs = AnnValues () xs

-- | Match a 'TyApp' with '()' annotation
pattern TyApp :: Atom a -> Type a -> Term a
pattern TyApp f x = AnnTyApp () f x

-- | Match a 'Cast' with '()' annotation
pattern Cast :: Atom a -> Coercion a -> Term a
pattern Cast a ty = AnnCast () a ty

-- | A binding group
data AnnBinding b a
  = One (a, Type a, AnnTerm b a) -- ^ Acyclic binding group (no recursion)
  | Many [(a, Type a, AnnTerm b a)] -- ^ Cyclic, possibly mutually recursive binding groups
  deriving (Eq, Show, Ord, Functor, Generic, Hashable)

-- | An 'AnnBinding' with '()' annotations.
type Binding = AnnBinding ()

-- | An 'Arm' of a pattern matching expression.
data AnnArm b a = Arm
  { _armPtrn :: Pattern a -- ^ The pattern
  , _armTy   :: Type a -- ^ The type of the scrutinee
  , _armBody :: AnnTerm b a -- ^ The body of the arm
  , _armVars :: [(a, Type a)] -- ^ Bound value variables
  , _armTyvars :: [(a, Type a)] -- ^ Existential type variables
  }
  deriving (Eq, Show, Ord, Functor, Generic, Hashable)

-- | An 'Arm' with '()' annotations
type Arm = AnnArm ()

data Pattern a
  = Constr a
  | Destr a (Capture a)
  | PatExtend (Capture a) [(Text, Capture a)]
  | PatValues [Capture a]
  | PatLit Literal
  | PatWildcard
  deriving (Eq, Show, Ord, Functor, Generic, Hashable)

data Capture a = Capture a (Type a)
  deriving (Eq, Show, Ord, Functor, Generic, Hashable)

data Coercion a
  = SameRepr (Type a) (Type a)
  | Symmetry (Coercion a)

  | Application (Coercion a) (Coercion a)
  | Quantified (BoundTv a) (Coercion a) (Coercion a)
  | ExactRecord [(Text, Coercion a)]
  | Record (Coercion a) [(Text, Coercion a)]
  | Projection [(Text, Coercion a)] [(Text, Coercion a)]

  | CoercionVar a

  | Domain (Coercion a)
  | Codomain (Coercion a)
  deriving (Eq, Show, Ord, Functor, Generic, Hashable)

data Literal
  = Int Integer
  | Str Text
  | Float Double
  | LitTrue | LitFalse
  | Unit | RecNil
  deriving (Eq, Show, Ord, Generic, Hashable)

data Type a
  = ConTy a -- A type constructor
  | VarTy a -- A type variable
  | ForallTy (BoundTv a) (Type a) (Type a) -- ^ A function abstraction
  | AppTy (Type a) (Type a) -- ^ A @tycon x@ type application
  | RowsTy (Type a) [(Text, Type a)] -- ^ The type of record extensions
  | ValuesTy [Type a] -- ^ The type of unboxed tuples
  | NilTy -- ^ The type of empty records
  | StarTy -- ^ The type of types
  deriving (Eq, Show, Ord, Functor, Generic, Hashable)

pattern ExactRowsTy :: [(Text, Type a)] -> Type a
pattern ExactRowsTy ts = RowsTy NilTy ts

data BoundTv a = Irrelevant | Relevant a
  deriving (Eq, Show, Ord, Functor, Generic, Hashable)

data AnnStmt b a
  = Foreign a (Type a) Text
  | StmtLet (AnnBinding b a)
  | Type a [(a, Type a)]
  deriving (Eq, Show, Ord, Functor, Generic, Hashable)

makeLenses ''AnnArm
makePrisms ''AnnStmt
makePrisms ''Atom
makePrisms ''AnnTerm
makePrisms ''Type
makePrisms ''Literal
makePrisms ''Coercion

type Stmt = AnnStmt ()

instance Pretty a => Pretty (Atom a) where
  pretty (Ref v ty) = pretty v <> scomment (string ":[" <> pretty ty <> string "]")
  pretty (Lit l) = pretty l

instance (Annotation b, Pretty a) => Pretty (AnnTerm b a) where
  pretty (AnnAtom an a) = annotated an $ pretty a
  pretty (AnnApp an f x) = annotated an $ pretty f <+> pretty x
  pretty (AnnTyApp an f t) = annotated an $ pretty f <+> braces (pretty t)

  pretty (AnnLam an (TypeArgument v t) c)
    = annotated an $ soperator (char 'Λ') <+> parens (pretty v <+> colon <+> pretty t) <> nest 2 (dot </> pretty c)
  pretty (AnnLam an (TermArgument v t) c)
    = annotated an $ soperator (char 'λ') <+> parens (pretty v <+> colon <+> pretty t) <> nest 2 (dot </> pretty c)

  pretty (AnnLet an (One x) e) = annotated an $ keyword "let" <+> pprLet1 x <#> keyword "in" <+> pretty e
  pretty (AnnLet an (Many xs) e) = annotated an $ fill 20 (keyword "let rec") <#> indent 2 (pprLet xs) <#> (keyword "in" <+> pretty e)
  pretty (AnnMatch an e ps) = annotated an $ keyword "match" <+> pretty e <+> pprArms ps
  pretty (AnnExtend an x rs) = annotated an $ braces $ pretty x <+> pipe <+> prettyRows rs where
    prettyRows :: [(Text, Type a, Atom a)] -> Doc
    prettyRows = hsep . punctuate comma . map (\(x, t, v) -> text x <+> colon <+> pretty t <+> equals <+> pretty v)
  pretty (AnnValues an xs) = annotated an $ soperator (string "(|") <+> (hsep . punctuate comma . map pretty $ xs) <+> soperator (string "|)")
  pretty (AnnCast an a phi) = annotated an $ parens $ pretty a <+> soperator (string "|>") <+> pretty phi

instance Pretty a => Pretty (Coercion a) where
  pretty (SameRepr a b) = pretty a <+> soperator (char '~') <+> pretty b
  pretty (Application c c') = pretty c <+> pretty c'
  pretty (Record r s) = enclose (lbrace <> space) (space <> rbrace) (pretty r <+> hsep (punctuate comma (map pprCoRow s)))
  pretty (ExactRecord r) = enclose (lbrace <> space) (space <> rbrace) (hsep (punctuate comma (map pprCoRow r)))
  pretty (Projection rs rs') = enclose (lbrace <> space) (space <> rbrace) (hsep (punctuate comma (map pprCoRow rs)) <+> keyword "with" <+> hsep (punctuate comma (map pprCoRow rs')))
  pretty (Domain f) = keyword "dom" <+> parens (pretty f)
  pretty (Codomain f) = keyword "cod" <+> parens (pretty f)
  pretty (Symmetry f) = keyword "sym" <+> parens (pretty f)
  pretty (Quantified (Relevant v) dom c) = keyword "∀" <> (pretty v <+> colon <+> pretty dom) <> dot <+> pretty c
  pretty (Quantified Irrelevant dom c) = pretty dom <+> arrow <+> pretty c
  pretty (CoercionVar x) = pretty x

pprLet :: (Annotation b, Pretty a) => [(a, Type a, AnnTerm b a)] -> Doc
pprLet = vsep . punctuate semi . map pprLet1

pprLet1 :: (Annotation b, Pretty a) => (a, Type a, AnnTerm b a) -> Doc
pprLet1 (a, b, c) = pretty a <+> colon <+> pretty b <+> nest 2 (equals </> pretty c)

pprBegin :: [Doc] -> Doc
pprBegin = braces' . vsep . map (indent 2) . punctuate semi

pprArms :: (Annotation b, Pretty a) => [AnnArm b a] -> Doc
pprArms = braces' . vsep . map (indent 2) . punctuate semi . map one where
  one (Arm a b c _ ts) = pretty a <+> brackets (hsep (punctuate comma (map pprTv ts))) <+> colon <+> pretty b <+> nest 2 (arrow </> pretty c)
  pprTv (a, t) = stypeVar (squote <> pretty a) <+> colon <+> pretty t

pprCoRow :: Pretty a => (Text, Coercion a) -> Doc
pprCoRow (a, b) = text a <+> equals <+> pretty b

braces' :: Doc -> Doc
braces' = enclose (lbrace <> linebreak) (linebreak <> rbrace)

instance Pretty a => Pretty (Pattern a) where
  pretty (Constr v) = pretty v
  pretty (Destr v p) = parens (pretty v <+> pretty p)
  pretty (PatExtend p rs) = braces $ pretty p <+> pipe <+> prettyRows rs where
    prettyRows :: [(Text, Capture a)] -> Doc
    prettyRows = hsep . punctuate comma . map (\(x, v) ->
      text x <+> equals <+> pretty v)
  pretty (PatValues xs) = soperator (string "(|") <+> (hsep . punctuate comma . map pretty $ xs) <+> soperator (string "|)")
  pretty (PatLit l) = pretty l
  pretty PatWildcard = string "_"

instance Pretty a => Pretty (Capture a) where
  pretty (Capture v ty) = pretty v <> scomment (string ":[" <> pretty ty <> string "]")

instance Pretty a => Pretty (Type a) where
  pretty (ConTy v) = stypeCon (pretty v)
  pretty (VarTy v) = stypeVar (squote <> pretty v)
  pretty (ForallTy (Relevant vs) c v)
    = skeyword (char '∀') <+> parens (stypeVar (pretty vs) <+> colon <+> pretty c) <> dot <+> pretty v

  pretty (ForallTy Irrelevant x e)
    | ForallTy{} <- x = parens (pretty x) <+> arrow <+> pretty e
    | otherwise = pretty x <+> arrow <+> pretty e

  pretty (ExactRowsTy rows) = braces $ prettyRows rows where
    prettyRows :: [(Text, Type a)] -> Doc
    prettyRows = hsep . punctuate comma . map (\(x, t) -> text x <+> colon <+> pretty t)

  pretty (RowsTy p rows) = braces $ pretty p <+> pipe <+> prettyRows rows where
    prettyRows :: [(Text, Type a)] -> Doc
    prettyRows = hsep . punctuate comma . map (\(x, t) -> text x <+> colon <+> pretty t)
  pretty NilTy = braces empty

  pretty (AppTy e x) = pretty e <+> k x (pretty x) where
    k AppTy{} = parens
    k ForallTy{} = parens
    k _ = id

  pretty (ValuesTy xs) = soperator (string "(|") <+> (hsep . punctuate comma . map pretty $ xs) <+> soperator (string "|)")

  pretty StarTy = prod

instance Pretty Literal where
  pretty LitFalse = sliteral (string "false")
  pretty LitTrue = sliteral (string "true")
  pretty Unit = sliteral (string "unit")
  pretty RecNil = sliteral (braces empty)
  pretty (Float l) = sliteral (double l)
  pretty (Int l) = sliteral (integer l)
  pretty (Str s) = sstring (dquotes (text s))

instance (Annotation b, Pretty a) => Pretty (AnnStmt b a) where
  pretty (Foreign v t _) = pretty v <+> colon <+> pretty t <+> equals <+> keyword "foreign"
  pretty (StmtLet (One x)) = keyword "let" <+> pprLet1 x
  pretty (StmtLet (Many xs)) = keyword "let rec" <+> pprLet xs
  pretty (Type v cs) = keyword "type" <+> pretty v <+> pprBegin (map pprCons cs) where
    pprCons (x, t) = pretty x <+> colon <+> pretty t

instance Pretty a => Pretty [Stmt a] where
  pretty = vcat . map pretty

freeInAtom :: IsVar a => Atom a -> VarSet.Set
freeInAtom (Ref v _) = VarSet.singleton (toVar v)
freeInAtom (Lit _) = mempty

freeIn :: IsVar a => AnnTerm b a -> VarSet.Set
freeIn (AnnAtom _ a) = freeInAtom a
freeIn (AnnApp _ f x) = freeInAtom f <> freeInAtom x
freeIn (AnnLam _ (TermArgument v _) e) = VarSet.delete (toVar v) (freeIn e)
freeIn (AnnLam _ TypeArgument{} e) = freeIn e
freeIn (AnnLet _ (One v) e) = VarSet.difference (freeIn e <> freeIn (thd3 v)) (VarSet.singleton (toVar (fst3 v)))
freeIn (AnnLet _ (Many vs) e) = VarSet.difference (freeIn e <> foldMap (freeIn . thd3) vs) (VarSet.fromList (map (toVar . fst3) vs))
freeIn (AnnMatch _ e bs) = freeInAtom e <> foldMap freeInBranch bs where
  freeInBranch x = foldr (VarSet.delete . toVar . fst) (freeIn (x ^. armBody)) (x ^. armVars)
freeIn (AnnExtend _ c rs) = freeInAtom c <> foldMap (freeInAtom . thd3) rs
freeIn (AnnValues _ xs) = foldMap freeInAtom xs
freeIn (AnnTyApp _ f _) = freeInAtom f
freeIn (AnnCast _ f _) = freeInAtom f

freeInTy :: IsVar a => Type a -> VarSet.Set
freeInTy (VarTy v) = VarSet.singleton (toVar v)
freeInTy (ForallTy (Relevant v) a b) = freeInTy a <> (toVar v `VarSet.delete` freeInTy b)
freeInTy (ForallTy Irrelevant a b) = freeInTy a <> freeInTy b
freeInTy (AppTy a b) = freeInTy a <> freeInTy b
freeInTy (RowsTy c rs) = foldMap (freeInTy . snd) rs <> freeInTy c
freeInTy (ExactRowsTy rs) = foldMap (freeInTy . snd) rs
freeInTy (ValuesTy xs) = foldMap freeInTy xs
freeInTy ConTy{} = mempty
freeInTy StarTy = mempty
freeInTy NilTy = mempty

occursInAtom :: IsVar a => a -> Atom a -> Bool
occursInAtom v (Ref v' _) = toVar v == toVar v'
occursInAtom _ (Lit _) = False

occursInTerm :: IsVar a => a -> Term a -> Bool
occursInTerm v (Atom a) = occursInAtom v a
occursInTerm v (App f x) = occursInAtom v f || occursInAtom v x
occursInTerm v (Lam _ b) = occursInTerm v b
occursInTerm v (TyApp f _) = occursInAtom v f
occursInTerm v (Cast f _) = occursInAtom v f
occursInTerm v (Let (One va) e) = occursInTerm v (thd3 va) || occursInTerm v e
occursInTerm v (Let (Many vs) e) = any (occursInTerm v . thd3) vs || occursInTerm v e
occursInTerm v (Match e bs) = occursInAtom v e || any (occursInTerm v . view armBody) bs
occursInTerm v (Extend e fs) = occursInAtom v e || any (occursInAtom v . thd3) fs
occursInTerm v (Values xs) = any (occursInAtom v) xs

occursInTy :: IsVar a => a -> Type a -> Bool
occursInTy _ (ConTy _) = False
occursInTy v (VarTy v') = toVar v == toVar v'
occursInTy v (ForallTy b k t)
  | Relevant v /= b = occursInTy v k || occursInTy v t
  | otherwise = occursInTy v k
occursInTy v (AppTy a b) = occursInTy v a || occursInTy v b
occursInTy v (RowsTy t rs) = occursInTy v t || any (occursInTy v . snd) rs
occursInTy v (ExactRowsTy rs) = any (occursInTy v . snd) rs
occursInTy v (ValuesTy xs) = any (occursInTy v) xs
occursInTy _ StarTy = False
occursInTy _ NilTy = False

relates :: Coercion a -> Maybe (Type a, Type a)
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

relates (Projection rs rs') = do
  let go (t, c) = do
        (a, b) <- relates c
        pure ((t, a), (t, b))
  (as, bs) <- unzip <$> traverse go rs
  (ss, ts) <- unzip <$> traverse go rs'
  let first = unionBy ((==) `on` fst) as ss

  pure (ExactRowsTy first, RowsTy (ExactRowsTy bs) ts)

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
extractAnn (AnnLam b _ _)    = b
extractAnn (AnnLet b _ _)    = b
extractAnn (AnnMatch b _ _)  = b
extractAnn (AnnExtend b _ _) = b
extractAnn (AnnValues b _)  = b
extractAnn (AnnTyApp b _ _)  = b
extractAnn (AnnCast b _ _)   = b

instance Plated (Atom a) where
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

squishCoercion :: Coercion a -> Coercion a
squishCoercion (Application c d)
  | SameRepr t s <- squishCoercion c, SameRepr x y <- squishCoercion d =
    SameRepr (AppTy t x) (AppTy s y)
squishCoercion (ExactRecord rs)
  | all (isJust . (^? _SameRepr) . view _2) rs =
    let co (t, SameRepr a b) = ((t, a), (t, b))
        co (t, _) = error ("impossible coercion for " ++ show t ++ " because of guard")
        (as, bs) = unzip (map co rs)
     in SameRepr (ExactRowsTy as) (ExactRowsTy bs)
squishCoercion x = x
