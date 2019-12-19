{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances
  , PatternSynonyms, StandaloneDeriving, TemplateHaskell, TypeFamilies
  , UndecidableInstances #-}

-- | The core types to represent top level statements within Amulet's syntax.
module Syntax.Toplevel where

import Control.Lens

import Data.List.NonEmpty(NonEmpty ((:|)))
import Data.Semigroup (sconcat)
import Data.Text (Text)
import Data.Typeable
import Data.Spanned
import Data.Span
import Data.Data

import Text.Pretty.Semantic

import Syntax.Expr.Instances ()
import Syntax.Type
import Syntax.Expr
import Syntax.Var

-- | An accessibility modifier of a top-level declaration
data TopAccess = Public | Private
  deriving (Eq, Ord, Show, Data)

data TargetImport p = TargetImport
  { importBackend :: Text
  , importPath    :: Text
  , importAnn     :: Ann p
  }

deriving instance Eq (Ann p) => Eq (TargetImport p)
deriving instance Show (Ann p) => Show (TargetImport p)
deriving instance Ord (Ann p) => Ord (TargetImport p)
deriving instance (Data p, Typeable p, Data (Var p), Data (Ann p)) => Data (TargetImport p)

data ModuleTerm p
  = ModStruct [Toplevel p] (Ann p)
  | ModRef (Var p) (Ann p)
  | ModImport Text (Ann p)
  | ModTargetImport [TargetImport p] (Ann p)

deriving instance (Eq (Var p), Eq (Ann p)) => Eq (ModuleTerm p)
deriving instance (Show (Var p), Show (Ann p)) => Show (ModuleTerm p)
deriving instance (Ord (Var p), Ord (Ann p)) => Ord (ModuleTerm p)
deriving instance (Data p, Typeable p, Data (Var p), Data (Ann p)) => Data (ModuleTerm p)

data Toplevel p
  = LetStmt RecKind TopAccess [Binding p]
  | ForeignVal TopAccess (Var p) Text (Type p) (Ann p)
  | TypeDecl TopAccess (Var p) [TyConArg p] (Maybe [Constructor p]) (Ann p)
  | TySymDecl TopAccess (Var p) [TyConArg p] (Type p) (Ann p)

  | Module TopAccess (Var p) (ModuleTerm p)
  | Open (ModuleTerm p)
  | Include (ModuleTerm p)

  | Class { className :: Var p
          , classAccess :: TopAccess
          , classCtx :: Maybe (Type p)
          , classParams :: [TyConArg p]
          , classDeps :: [Fundep p]
          , classMethods :: [ClassItem p]
          , ann :: Ann p }

  | Instance { instanceClass :: Var p
             , instanceCtx :: Maybe (Type p)
             , instanceHead :: Type p
             , instanceMethods :: [InstanceItem p]
             , instanceDeriveGenerated :: Bool
             , ann :: Ann p }

  | DeriveInstance { derivingType :: Type p
                   , ann :: Ann p }

  | TypeFunDecl { tyfunAccess :: TopAccess
                , tyfunName :: Var p
                , tyfunArgs :: [TyConArg p]
                , tyfunKindSig :: Maybe (Type p)
                , tyfunEqs :: [TyFunClause p]
                , ann :: Ann p
                }

deriving instance (Eq (Var p), Eq (Ann p)) => Eq (Toplevel p)
deriving instance (Show (Var p), Show (Ann p)) => Show (Toplevel p)
deriving instance (Ord (Var p), Ord (Ann p)) => Ord (Toplevel p)
deriving instance (Data p, Typeable p, Data (Var p), Data (Ann p)) => Data (Toplevel p)

data TyFunClause p = TyFunClause (Type p) (Type p) (Ann p)

deriving instance (Eq (Var p), Eq (Ann p)) => Eq (TyFunClause p)
deriving instance (Show (Var p), Show (Ann p)) => Show (TyFunClause p)
deriving instance (Ord (Var p), Ord (Ann p)) => Ord (TyFunClause p)
deriving instance (Data p, Typeable p, Data (Var p), Data (Ann p)) => Data (TyFunClause p)

data ClassItem p
  = MethodSig { _methName :: Var p
              , _methTy :: Type p
              , _methAnn :: Ann p
              }
  | DefaultMethod { _methodBinding :: Binding p
                  , _methAnn :: Ann p
                  }
  | AssocType { _methName :: Var p
              , _methArgs :: [TyConArg p]
              , _methKind :: Type p
              , _methAnn  :: Ann p
              }

deriving instance (Eq (Var p), Eq (Ann p)) => Eq (ClassItem p)
deriving instance (Show (Var p), Show (Ann p)) => Show (ClassItem p)
deriving instance (Ord (Var p), Ord (Ann p)) => Ord (ClassItem p)
deriving instance (Data p, Typeable p, Data (Var p), Data (Ann p)) => Data (ClassItem p)

data InstanceItem p
  = MethodImpl (Binding p)
  | TypeImpl { _typeName :: Var p
             , _typeArgs :: [TyConArg p]
             , _typeInst :: Type p
             , _typeAnn :: Ann p
             }

deriving instance (Eq (Var p), Eq (Ann p)) => Eq (InstanceItem p)
deriving instance (Show (Var p), Show (Ann p)) => Show (InstanceItem p)
deriving instance (Ord (Var p), Ord (Ann p)) => Ord (InstanceItem p)
deriving instance (Data p, Typeable p, Data (Var p), Data (Ann p)) => Data (InstanceItem p)

data Fundep p =
  Fundep { _fdFrom :: [Var p]
         , _fdTo :: [Var p]
         , _fdAnn :: Ann p
         }

deriving instance (Eq (Var p), Eq (Ann p)) => Eq (Fundep p)
deriving instance (Show (Var p), Show (Ann p)) => Show (Fundep p)
deriving instance (Ord (Var p), Ord (Ann p)) => Ord (Fundep p)
deriving instance (Data p, Typeable p, Data (Var p), Data (Ann p)) => Data (Fundep p)

data Constructor p
  = UnitCon TopAccess (Var p) (Ann p)
  -- In ArgCon, the Type p is the type of the (sole) argument
  | ArgCon TopAccess (Var p) (Type p) (Ann p)
  -- In GadtCon, the Type p is the type of the overall thing
  | GadtCon TopAccess (Var p) (Type p) (Ann p)

deriving instance (Eq (Var p), Eq (Ann p)) => Eq (Constructor p)
deriving instance (Show (Var p), Show (Ann p)) => Show (Constructor p)
deriving instance (Ord (Var p), Ord (Ann p)) => Ord (Constructor p)
deriving instance (Data p, Typeable p, Data (Var p), Data (Ann p)) => Data (Constructor p)
instance (Data (Var p), Data (Ann p), Data p) => Spanned (Constructor p)

makePrisms ''Toplevel
makePrisms ''Constructor

makeLenses ''ClassItem
makeLenses ''Fundep

instance Spanned (Ann p) => Spanned (TargetImport p) where
  annotation = annotation . importAnn

instance Spanned (Ann p) => Spanned (ModuleTerm p) where
  annotation (ModStruct _ a) = annotation a
  annotation (ModRef _ a) = annotation a
  annotation (ModImport _ a) = annotation a
  annotation (ModTargetImport _ a) = annotation a

instance (Spanned (Constructor p), Spanned (Ann p)) => Spanned (Toplevel p) where
  annotation (LetStmt _ _ []) = internal
  annotation (LetStmt _ _ [b]) = annotation b
  annotation (LetStmt _ _ (b:vs)) = sconcat (annotation b :| map annotation vs)
  annotation (TypeDecl _ _ _ (Just cs) x) = sconcat (annotation x :| map annotation cs)
  annotation (TypeDecl _ _ _ Nothing x) = annotation x
  annotation (TySymDecl _ _ _ _ x) = annotation x
  annotation (ForeignVal _ _ _ _ x) = annotation x
  annotation (Class _ _ _ _ _ _ x) = annotation x
  annotation (Instance _ _ _ _ _ x) = annotation x
  annotation (DeriveInstance _ x) = annotation x
  annotation x@TypeFunDecl{} = annotation (ann x)
  annotation (Module _ _ m) = annotation m
  annotation (Open m) = annotation m
  annotation (Include m) = annotation m

instance Spanned (Ann p) => Spanned (Fundep p) where
  annotation = annotation . view fdAnn

instance Spanned (Ann p) => Spanned (TyFunClause p) where
  annotation (TyFunClause _ _ x) = annotation x

instance Spanned (Ann p) => Spanned (ClassItem p) where
  annotation = annotation . view methAnn

instance Spanned (Ann p) => Spanned (InstanceItem p) where
  annotation (TypeImpl _ _ _ x) = annotation x
  annotation (MethodImpl b) = annotation b

instance Pretty (Var p) => Pretty (ClassItem p) where
  pretty (MethodSig v t _) = keyword "val" <+> pretty v <+> colon <+> pretty t
  pretty (DefaultMethod b _) = keyword "let" <+> pretty b
  pretty (AssocType v as k _) = keyword "type" <+> pretty v <+> hsep (map pretty as) <+> colon <+> pretty k

instance Pretty (Var p) => Pretty (Fundep p) where
  pretty (Fundep from to _) = hsep (punctuate comma (map k from))
                          <+> arrow
                          <+> hsep (punctuate comma (map k to))
    where k x = stypeVar $ squote <> pretty x

instance Pretty TopAccess where
  pretty Public = keyword "public"
  pretty Private = keyword "private"

prettyAcc :: TopAccess -> Doc
prettyAcc Public = empty
prettyAcc x = pretty x <+> empty

prettyRec :: RecKind -> Doc
prettyRec Recursive = keyword "rec" <> space
prettyRec NonRecursive = mempty

instance Pretty (TargetImport p) where
  pretty (TargetImport backend path _) = text backend <+> equals <+> sstring (dquotes (text path))

instance Pretty (Var p) => Pretty (ModuleTerm p) where
  pretty (ModStruct bod _) =
    vsep [ keyword "begin"
         , indent 2 (align (pretty bod))
         , keyword "end"
         ]
  pretty (ModRef v _) = pretty v
  pretty (ModImport t _) = keyword "import" <+> sstring (dquotes (text t))
  pretty (ModTargetImport ts _) = keyword "import" <+> braces (hsep . punctuate comma . map pretty $ ts)

instance Pretty (Var p) => Pretty (Toplevel p) where
  pretty (LetStmt _ _ []) = string "empty let?"
  pretty (LetStmt re m (x:xs)) =
    let prettyBind x = keyword "and" <+> pretty x
     in keyword "let" <+> prettyRec re <> prettyAcc m <> pretty x
             <> case xs of
                  [] -> empty
                  _ -> line <> vsep (map prettyBind xs)
  pretty (ForeignVal m v d ty _) =
    keyword "foreign" <+> prettyAcc m <> keyword "val" <+> pretty v <+> colon <+> pretty ty <+> equals <+> dquotes (text d)
  pretty (TypeDecl m ty args ctors _) =
    let ct = case ctors of
          Nothing -> mempty
          Just [] -> equals <+> pipe
          Just cs -> equals <#> indent 2 (vsep (map ((pipe <+>) . pretty) cs))
    in keyword "type" <+> prettyAcc m <> pretty ty <+> hsep (map ((squote <>) . pretty) args) <+> ct
  pretty (Open m) = keyword "open" <+> pretty m
  pretty (Include m) = keyword "include" <+> pretty m

  pretty (TySymDecl m ty args exp _) =
    prettyAcc m <+> keyword "type" <> pretty ty <+> hsep (map ((squote <>) . pretty) args) <+> pretty exp

  pretty (Module am m bod) =
    keyword "module" <+> prettyAcc am <> pretty m <+> equals <+> pretty bod

  pretty (Class v am c h fd m _) =
    vsep [ keyword "class" <+> prettyAcc am <> maybe (parens mempty) pretty c
            <+> soperator (string "=>") <+> pretty v <+> hsep (map pretty h) <+> fds
            <+> keyword "begin"
         , indent 2 (align (vsep (map pretty m)))
         , keyword "end"
         ]
    where fds = case fd of { [] -> empty; _ -> pipe <+> hsep (map pretty fd) }

  pretty (Instance _ c h m _ _) =
    vsep [ keyword "instance" <+> maybe (parens mempty) pretty c
            <+> soperator (string "=>") <+> pretty h <+> keyword "begin"
         , indent 2 (align (pretty m))
         , keyword "end"
         ]

  pretty (DeriveInstance t _) = keyword "deriving instance" <+> pretty t

  pretty (TypeFunDecl ac name args kindsig equations _) =
    prettyAcc ac <+> keyword "type function" <+> pretty name <+> hsep (map ((squote <>) . pretty) args) <> ks <+> keyword "begin"
      <#> indent 2 (align (vsep (map pretty equations)))
      <#> keyword "end"
    where ks = foldMap ((colon <+>) . pretty) kindsig

instance Pretty (Var p) => Pretty (TyFunClause p) where
  pretty (TyFunClause lhs rhs _) = pretty lhs <+> equals <+> pretty rhs

instance (Pretty (Var p)) => Pretty [Toplevel p] where
  pretty = vcat . map pretty

instance (Pretty (Var p)) => Pretty (Constructor p) where
  pretty (UnitCon a p _) = prettyAccess a $ pretty p
  pretty (ArgCon a p t _) = prettyAccess a $ pretty p <+> keyword "of" <+> pretty t
  pretty (GadtCon a p t _) = prettyAccess a $ pretty p <+> colon <+> pretty t

instance (Pretty (Var p)) => Pretty (InstanceItem p) where
  pretty (MethodImpl b) = pretty b
  pretty (TypeImpl v as t _) = keyword "type" <+> pretty v <+> hsep (map pretty as) <+> equals <+> pretty t

prettyAccess :: TopAccess -> Doc -> Doc
prettyAccess Public x = x
prettyAccess Private x = keyword "private" <+> x
