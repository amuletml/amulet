{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances
  , PatternSynonyms, StandaloneDeriving, TemplateHaskell, TypeFamilies
  , UndecidableInstances, DerivingVia #-}

-- | The core types to represent top level statements within Amulet's syntax.
module Syntax.Toplevel where

import Control.Lens

import Data.Text (Text)
import Data.Spanned
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
  , importAnn     :: RawAnn p
  }

deriving instance EqPhrase p => Eq (TargetImport p)
deriving instance ShowPhrase p => Show (TargetImport p)
deriving instance OrdPhrase p => Ord (TargetImport p)
deriving instance DataPhrase p => Data (TargetImport p)

data ModuleTerm p
  = ModStruct [Toplevel p] (RawAnn p)
  | ModRef (Var p) (RawAnn p)
  | ModImport Text (RawAnn p)
  | ModTargetImport [TargetImport p] (RawAnn p)

deriving instance EqPhrase p => Eq (ModuleTerm p)
deriving instance ShowPhrase p => Show (ModuleTerm p)
deriving instance OrdPhrase p => Ord (ModuleTerm p)
deriving instance DataPhrase p => Data (ModuleTerm p)

data Toplevel p
  = LetStmt RecKind TopAccess [Binding p] (RawAnn p)
  | ForeignVal TopAccess (Var p) Text (Type p) (RawAnn p)
  | TypeDecl TopAccess (Var p) [TyConArg p] (Maybe [Constructor p]) (RawAnn p)
  | TySymDecl TopAccess (Var p) [TyConArg p] (Type p) (RawAnn p)

  | Module TopAccess (Var p) (ModuleTerm p)
  | Open (ModuleTerm p)
  | Include (ModuleTerm p)

  | Class { className :: Var p
          , classAccess :: TopAccess
          , classCtx :: Maybe (Type p)
          , classParams :: [TyConArg p]
          , classDeps :: [Fundep p]
          , classMethods :: [ClassItem p]
          , ann :: RawAnn p }

  | Instance { instanceClass :: Var p
             , instanceCtx :: Maybe (Type p)
             , instanceHead :: Type p
             , instanceMethods :: [InstanceItem p]
             , instanceDeriveGenerated :: Bool
             , ann :: RawAnn p }

  | DeriveInstance { derivingType :: Type p
                   , ann :: RawAnn p }

  | TypeFunDecl { tyfunAccess :: TopAccess
                , tyfunName :: Var p
                , tyfunArgs :: [TyConArg p]
                , tyfunKindSig :: Maybe (Type p)
                , tyfunEqs :: [TyFunClause p]
                , ann :: RawAnn p
                }

deriving instance EqPhrase p => Eq (Toplevel p)
deriving instance ShowPhrase p => Show (Toplevel p)
deriving instance OrdPhrase p => Ord (Toplevel p)
deriving instance DataPhrase p => Data (Toplevel p)

data TyFunClause p = TyFunClause (Type p) (Type p) (Ann p)

deriving instance EqPhrase p => Eq (TyFunClause p)
deriving instance ShowPhrase p => Show (TyFunClause p)
deriving instance OrdPhrase p => Ord (TyFunClause p)
deriving instance DataPhrase p => Data (TyFunClause p)

data ClassItem p
  = MethodSig { _methName :: Var p
              , _methTy :: Type p
              , _methAnn :: RawAnn p
              }
  | DefaultMethod { _methodBinding :: Binding p
                  , _methAnn :: RawAnn p
                  }
  | AssocType { _methName :: Var p
              , _methArgs :: [TyConArg p]
              , _methKind :: Type p
              , _methAnn  :: RawAnn p
              }

deriving instance EqPhrase p => Eq (ClassItem p)
deriving instance ShowPhrase p => Show (ClassItem p)
deriving instance OrdPhrase p => Ord (ClassItem p)
deriving instance DataPhrase p => Data (ClassItem p)

data InstanceItem p
  = MethodImpl (Binding p)
  | TypeImpl { _typeName :: Var p
             , _typeArgs :: [TyConArg p]
             , _typeInst :: Type p
             , _typeAnn :: Ann p
             }

deriving instance EqPhrase p => Eq (InstanceItem p)
deriving instance ShowPhrase p => Show (InstanceItem p)
deriving instance OrdPhrase p => Ord (InstanceItem p)
deriving instance DataPhrase p => Data (InstanceItem p)

data Fundep p =
  Fundep { _fdFrom :: [Var p]
         , _fdTo :: [Var p]
         , _fdAnn :: RawAnn p
         }

deriving instance EqPhrase p => Eq (Fundep p)
deriving instance ShowPhrase p => Show (Fundep p)
deriving instance OrdPhrase p => Ord (Fundep p)
deriving instance DataPhrase p => Data (Fundep p)

data Constructor p
  = UnitCon TopAccess (Var p) (Ann p)
  -- In ArgCon, the Type p is the type of the (sole) argument
  | ArgCon TopAccess (Var p) (Type p) (Ann p)
  -- In GadtCon, the Type p is the type of the overall thing
  | GadtCon TopAccess (Var p) (Type p) (Ann p)

deriving instance EqPhrase p => Eq (Constructor p)
deriving instance ShowPhrase p => Show (Constructor p)
deriving instance OrdPhrase p => Ord (Constructor p)
deriving instance DataPhrase p => Data (Constructor p)

makePrisms ''Toplevel
makePrisms ''Constructor

makeLenses ''ClassItem
makeLenses ''Fundep

instance Annotated (Constructor p) where
  type Annotation (Constructor p) = Ann p
  annotation (UnitCon _ _ a) = a
  annotation (ArgCon _ _ _ a) = a
  annotation (GadtCon _ _ _ a) = a

instance Annotated (TargetImport p) where
  type Annotation (TargetImport p) = RawAnn p
  annotation = importAnn

instance Annotated (ModuleTerm p) where
  type Annotation (ModuleTerm p) = RawAnn p
  annotation (ModStruct _ a) = a
  annotation (ModRef _ a) = a
  annotation (ModImport _ a) = a
  annotation (ModTargetImport _ a) = a

instance Annotated (Toplevel p) where
  type Annotation (Toplevel p) = RawAnn p
  annotation (LetStmt _ _ _ x) = x
  annotation (TypeDecl _ _ _ _ x) = x
  annotation (TySymDecl _ _ _ _ x) = x
  annotation (ForeignVal _ _ _ _ x) = x
  annotation (Class _ _ _ _ _ _ x) = x
  annotation (Instance _ _ _ _ _ x) = x
  annotation (DeriveInstance _ x) = x
  annotation x@TypeFunDecl{} = ann x
  annotation (Module _ _ m) = annotation m
  annotation (Open m) = annotation m
  annotation (Include m) = annotation m

instance Annotated (Fundep p) where
  type Annotation (Fundep p) = RawAnn p
  annotation = view fdAnn

instance Annotated (TyFunClause p) where
  type Annotation (TyFunClause p) = Ann p
  annotation (TyFunClause _ _ x) = x

instance Annotated (ClassItem p) where
  type Annotation (ClassItem p) = RawAnn p
  annotation = view methAnn

instance Annotated (InstanceItem p) where
  type Annotation (InstanceItem p) = Ann p
  annotation (TypeImpl _ _ _ x) = x
  annotation (MethodImpl b) = annotation b

deriving via AnnotatedVia (Constructor p)  (Ann p)    instance Spanned (Ann p)    => Spanned (Constructor p)
deriving via AnnotatedVia (TargetImport p) (RawAnn p) instance Spanned (RawAnn p) => Spanned (TargetImport p)
deriving via AnnotatedVia (ModuleTerm p)   (RawAnn p) instance Spanned (RawAnn p) => Spanned (ModuleTerm p)
deriving via AnnotatedVia (Toplevel p)     (RawAnn p) instance Spanned (RawAnn p) => Spanned (Toplevel p)
deriving via AnnotatedVia (Fundep p)       (RawAnn p) instance Spanned (RawAnn p) => Spanned (Fundep p)
deriving via AnnotatedVia (TyFunClause p)  (Ann p)    instance Spanned (Ann p)    => Spanned (TyFunClause p)
deriving via AnnotatedVia (ClassItem p)    (RawAnn p) instance Spanned (RawAnn p) => Spanned (ClassItem p)
deriving via AnnotatedVia (InstanceItem p) (Ann p)    instance Spanned (Ann p)    => Spanned (InstanceItem p)


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
  pretty (LetStmt _ _ [] _) = string "empty let?"
  pretty (LetStmt re m (x:xs) _) =
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
