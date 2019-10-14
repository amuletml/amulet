{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, PatternSynonyms
  , StandaloneDeriving, TemplateHaskell, TypeFamilies
  , UndecidableInstances, ViewPatterns #-}

-- | The core types to represent types within Amulet's syntax.
module Syntax.Type where

import Control.Lens

import Data.Maybe
import Data.Text (Text)
import Data.Typeable
import Data.Span
import Data.List
import Data.Data

import Text.Pretty.Semantic

import Syntax.Var

type family Ann a :: * where
  Ann Parsed = Span
  Ann Resolved = Span
  Ann Desugared = Span
  Ann Typed = (Span, Type Typed)

data Type p
  = TyCon (Var p)
  | TyVar (Var p)
  | TyPromotedCon (Var p)
  | TyApp (Type p) (Type p)
  | TyPi (TyBinder p) (Type p) -- ^ arrow, pi or forall
  | TyRows (Type p) [(Text, Type p)]  -- { α | foo : int, bar : string }
  | TyExactRows [(Text, Type p)] -- { foo : int, bar : string }
  | TyTuple (Type p) (Type p) -- (see note [1])
  | TyOperator (Type p) (Var p) (Type p)
  | TyWildcard (Maybe (Type p))
  | TyParens (Type p) -- ^ @(xyz)@, just useful for resetting precedence. Removed after the resolver.

  -- Used internally:
  | TySkol (Skolem p)
  | TyWithConstraints [(Type p, Type p)] (Type p)

  | TyType -- yeah, type : type, fight me
  | TyLit Lit

data TyBinder p
  = Anon { _tyBinderType :: Type p } -- ^ A function type
  | Implicit { _tyBinderType :: Type p } -- ^ A type with class obligations
  | Invisible
    { _tyBinderVar :: Var p
    , _tyBinderArg :: Maybe (Type p)
    , _tyVisFlag :: Specificity } -- ^ A forall. type

data Specificity = Infer | Spec | Req
  deriving (Eq, Show, Ord, Data)

deriving instance (Show (Var p), Show (Ann p)) => Show (TyBinder p)
deriving instance (Data p, Typeable p, Data (Var p), Data (Ann p)) => Data (TyBinder p)
deriving instance Eq (Var p) => Eq (TyBinder p)
deriving instance Ord (Var p) => Ord (TyBinder p)

data Skolem p
  = Skolem { _skolIdent :: Var p -- ^ The constant itself
           , _skolVar :: Var p -- ^ What variable this skolemises
           , _skolScope :: Type p -- ^ The type this was generated for
           , _skolMotive :: SkolemMotive p
           }

deriving instance (Show (Var p), Show (Ann p)) => Show (Skolem p)
deriving instance (Data p, Typeable p, Data (Var p), Data (Ann p)) => Data (Skolem p)

data SkolemMotive p
  = ByAscription (Ann Desugared) (Type p) -- what r phases?
  | BySubsumption (Type p) (Type p)
  | ByExistential (Var p) (Type p)
  | ByInstanceHead (Type p) (Ann Desugared)
  | ByConstraint (Type p)
  | ByTyFunLhs (Type p) (Ann Desugared)

deriving instance (Eq (Var p), Eq (Ann p)) => Eq (SkolemMotive p)
deriving instance (Show (Var p), Show (Ann p)) => Show (SkolemMotive p)
deriving instance (Ord (Var p), Ord (Ann p)) => Ord (SkolemMotive p)
deriving instance (Data p, Typeable p, Data (Var p), Data (Ann p)) => Data (SkolemMotive p)

instance Eq (Var p) => Eq (Skolem p) where
  Skolem v _ _ _ == Skolem v' _ _ _ = v == v'

instance Ord (Var p) => Ord (Skolem p) where
  Skolem v _ _ _ `compare` Skolem v' _ _ _ = v `compare` v'

deriving instance (Show (Var p), Show (Ann p)) => Show (Type p)
deriving instance (Data p, Typeable p, Data (Var p), Data (Ann p)) => Data (Type p)
deriving instance Ord (Var p) => Ord (Type p)
deriving instance Eq (Var p) => Eq (Type p)
instance (Data p, Typeable p, Data (Var p), Data (Ann p)) => Plated (Type p)

data Coercion p
  = VarCo (Var p) -- coercion variable
  | MvCo (Var p) -- coercion metavariable
  | ReflCo (Type p) -- <T> : T ~ T
  | SymCo (Coercion p) -- sym (X : T ~ S) : S ~ T
  | AppCo (Coercion p) (Coercion p) -- (f : B ~ D) (x : A ~ C) : B A ~ D C
  | ArrCo (Coercion p) (Coercion p) -- (x : S ~ T) -> (y : S' ~ T') : (S -> S') ~ (T -> T')
  | ProdCo (Coercion p) (Coercion p) -- (x : S ~ T, y : S' ~ T') : (S, S') ~ (T, T')
  | ExactRowsCo [(Text, Coercion p)] -- { x : A ~ B } : { x : A } ~ { x : B }
  | RowsCo (Coercion p) [(Text, Coercion p)] -- { x : A ~ B | f : S ~ T } : { A | f : S } ~ { B | f : T }
  | ProjCo [(Text, Type p)] [(Text, Coercion p)]
    -- { x : A ~ B | y : S ~ T } : { x : A, y : S } ~ { x : B | y : T }
  | AssumedCo (Type p) (Type p) -- <A, B> : A ~ B
  | ForallCo (Var p) (Coercion p) (Coercion p)
    -- (forall (v : x : c ~ d). phi : a ~ b) : forall (v : c). a ~ forall (v : d). b
  | P1 (Var p) -- { _1 : a ~ b, _2 : c ~ d }.1 : a ~ b
  | P2 (Var p) -- { _1 : a ~ b, _2 : c ~ d }.2 : a ~ b
  | InstCo (Var p) [Coercion p]

deriving instance (Eq (Var p), Eq (Ann p)) => Eq (Coercion p)
deriving instance (Show (Var p), Show (Ann p)) => Show (Coercion p)
deriving instance (Ord (Var p), Ord (Ann p)) => Ord (Coercion p)
deriving instance (Data p, Typeable p, Data (Var p), Data (Ann p)) => Data (Coercion p)

pattern TyArr :: Type p -> Type p -> Type p
pattern TyArr t t' <- TyPi (Anon t) t' where
  TyArr t ty = TyPi (Anon t) ty

pattern (:->) :: Type p -> Type p -> Type p
pattern t :-> t' <- TyPi (Anon t) t' where
  t :-> ty = TyPi (Anon t) ty

pattern TyForall :: Var p -> Maybe (Type p) -> Type p -> Type p
pattern TyForall v k t' <- TyPi (Invisible v k _) t' where
  TyForall v k ty = TyPi (Invisible v k Infer) ty

-- | A type variable, with an optional type annotation.
data TyConArg p
  = TyVarArg (Var p)
  | TyAnnArg (Var p) (Type p) -- ( 'a : k )

deriving instance (Eq (Var p), Eq (Ann p)) => Eq (TyConArg p)
deriving instance (Show (Var p), Show (Ann p)) => Show (TyConArg p)
deriving instance (Ord (Var p), Ord (Ann p)) => Ord (TyConArg p)
deriving instance (Data p, Typeable p, Data (Var p), Data (Ann p)) => Data (TyConArg p)

makePrisms ''TyBinder
makePrisms ''Type

makeLenses ''Skolem
makeLenses ''TyBinder

instance Pretty (Var p) => Pretty (Coercion p) where
  pretty (VarCo x) = stypeSkol (pretty x)
  pretty (MvCo x) = stypeSkol (pretty x)
  pretty (ReflCo t) = enclose (char '<') (char '>') (pretty t)
  pretty (AssumedCo a b) = enclose (char '<') (char '>') (pretty a <> comma <+> pretty b)
  pretty (SymCo x) = keyword "sym" <+> pretty x
  pretty (AppCo f x) = pretty f <+> pretty x
  pretty (ArrCo f x) = pretty f <+> arrow <+> pretty x
  pretty (ProdCo f x) = pretty f <+> prod <+> pretty x
  pretty (ExactRowsCo rs) = record (map (\(n, v) -> text n <+> colon <+> pretty v) rs)
  pretty (RowsCo c rs) =
    enclose (lbrace <> space) (space <> rbrace)
      (pretty c <+> pipe <+> hsep (punctuate comma (map (\(n, v) -> text n <+> colon <+> pretty v) rs)))
  pretty (ProjCo _ rs') =
    enclose (lbrace <> space) (space <> rbrace) $ keyword "proj" <+> pprRow rs'
    where pprRow xs = hsep (punctuate comma (map (\(n, v) -> text n <+> colon <+> pretty v) xs))
  pretty (ForallCo v c cs) = keyword "∀" <> parens (pretty v <+> colon <+> pretty c) <> dot <+> pretty cs
  pretty (P1 c) = pretty c <> keyword ".1"
  pretty (P2 c) = pretty c <> keyword ".2"
  pretty (InstCo ax t) = pretty ax <+> hsep (map (parens . pretty) t)

record :: [Doc] -> Doc
record = enclose (lbrace <> space) (space <> rbrace) . hsep . punctuate comma

prettyRows :: Pretty x => Doc -> [(Text, x)] -> [Doc]
prettyRows sep = map (\(n, v) -> text n <+> sep <+> pretty v) . sortOn fst

instance (Pretty (Var p)) => Pretty (Type p) where
  pretty (TyCon v) = stypeCon (pretty v)
  pretty (TyPromotedCon v) = stypeCon (pretty v)
  pretty (TyVar v) = stypeVar (squote <> pretty v)
  pretty (TySkol v) = stypeSkol (pretty (v ^. skolIdent) <> squote <> pretty (v ^. skolVar))

  pretty (TyPi x e) = pretty x <+> pretty e
  pretty (TyWildcard (Just t)) = soperator (string "'_") <> pretty t
  pretty TyWildcard{} = skeyword (char '_')

  pretty (TyRows p rows) =
    enclose (lbrace <> space) (space <> rbrace) $
      pretty p <+> soperator pipe <+> hsep (punctuate comma (prettyRows colon rows))
  pretty (TyExactRows rows) = record (prettyRows colon rows)

  pretty (TyApp x e) = pretty x <+> parenTyArg e (pretty e) where
    parenTyArg TyApp{} = parens
    parenTyArg TyPi{} = parens
    parenTyArg TyTuple{} = parens
    parenTyArg _ = id

  pretty (TyTuple a b)
    | TyTuple{} <- a
    = parens (pretty a) <+> prod <+> pretty b
    | otherwise
    = pretty a <+> prod <+> pretty b

  pretty (TyOperator l o r) = pretty l <+> pretty o <+> pretty r

  pretty (TyParens t) = parens $ pretty t

  pretty (TyWithConstraints a b) =
    parens (hsep (punctuate comma (map prettyEq a))) <+> soperator (char '⊃') <+> pretty b
    where prettyEq (a, b) = pretty a <+> soperator (char '~') <+> pretty b

  pretty TyType = stypeCon (string "type")
  pretty (TyLit v) = pretty v

instance Pretty (Var p) => Pretty (TyBinder p) where
  pretty (Anon t) = k t (pretty t) <+> arrow where
    k TyPi{} = parens
    k TyTuple{} = parens
    k _ = id
  pretty (Implicit t) = k t (pretty t) <+> soperator (string "=>") where
    k TyPi{} = parens
    k TyTuple{} = parens
    k _ = id
  pretty (Invisible v (Just k) r) = shown r <> braces (stypeVar (squote <> pretty v) <+> colon <+> pretty k) <> dot
  pretty (Invisible v Nothing r)  = shown r <> stypeVar (squote <> pretty v) <> dot

instance Pretty (Var p) => Pretty (TyConArg p) where
  pretty (TyVarArg var) = pretty var
  pretty (TyAnnArg v k) = parens (pretty v <+> colon <+> pretty k)

getType :: Data (f Typed) => f Typed -> Type Typed
getType = snd . head . catMaybes . gmapQ get where
  get d = fmap (`asTypeOf` (undefined :: (Span, Type Typed))) (cast d)
  -- FIXME: Point-freeing this definition makes type inference broken.
  -- Thanks, GHC.

_TyArr :: Prism' (Type p) (Type p, Type p)
_TyArr = prism (uncurry (TyPi . Anon)) go where
  go (TyArr a b) = Right (a, b)
  go x = Left x

-- | Determine if this type can be skolemized.
isSkolemisable :: Type Typed -> Bool
isSkolemisable (TyPi Invisible{} _) = True
isSkolemisable (TyPi Implicit{} _) = True
isSkolemisable _ = False

isInstantiatable :: Type Typed -> Bool
isInstantiatable (TyPi (Invisible _ _ r) _) = r /= Req
isInstantiatable (TyPi Implicit{} _) = True
isInstantiatable _ = False

mkWildTy :: Maybe (Type p) -> Type p
mkWildTy (Just x@(TyWildcard _)) = x
mkWildTy t = TyWildcard t

appsView :: Type p -> [Type p]
appsView = reverse . go where
  go (TyApp f x) = x:go f
  go (TyOperator l o r) = [r, l, TyCon o]
  go t = [t]

pattern TyApps :: Type p -> [Type p] -> Type p
pattern TyApps head xs <- (appsView -> (head:xs)) where
  TyApps head xs = foldl TyApp head xs

pattern TyArrs :: [TyBinder p] -> Type p -> Type p
pattern TyArrs quant xs <- (arrsView -> (quant, xs)) where
  TyArrs [] cod = cod
  TyArrs (x:xs) cod = TyPi x (TyArrs xs cod)

arrsView :: Type p -> ([TyBinder p], Type p)
arrsView (TyPi q t) =
  let (qs, t') = arrsView t
   in (q:qs, t')
arrsView t = ([], t)
