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

data Toplevel p
  = LetStmt TopAccess [Binding p]
  | ForeignVal TopAccess (Var p) Text (Type p) (Ann p)
  | TypeDecl TopAccess (Var p) [TyConArg p] (Maybe [Constructor p]) (Ann p)
  | TySymDecl TopAccess (Var p) [TyConArg p] (Type p) (Ann p)
  | Module TopAccess (Var p) [Toplevel p]
  | Open { openName :: Var p
         , openAs :: Maybe Text }
  | Class { className :: Var p
          , classAccess :: TopAccess
          , classCtx :: Maybe (Type p)
          , classParams :: [TyConArg p]
          , classMethods :: [ClassItem p]
          , ann :: Ann p }
  | Instance { instanceClass :: Var p
             , instanceCtx :: Maybe (Type p)
             , instanceHead :: Type p
             , instanceMethods :: [Binding p]
             , ann :: Ann p }

deriving instance (Eq (Var p), Eq (Ann p)) => Eq (Toplevel p)
deriving instance (Show (Var p), Show (Ann p)) => Show (Toplevel p)
deriving instance (Ord (Var p), Ord (Ann p)) => Ord (Toplevel p)
deriving instance (Data p, Typeable p, Data (Var p), Data (Ann p)) => Data (Toplevel p)

data ClassItem p
  = MethodSig { _methName :: Var p
              , _methTy :: Type p
              , _methAnn :: Ann p
              }
  | DefaultMethod { _methodBinding :: Binding p
                  , _methAnn :: Ann p
                  }

deriving instance (Eq (Var p), Eq (Ann p)) => Eq (ClassItem p)
deriving instance (Show (Var p), Show (Ann p)) => Show (ClassItem p)
deriving instance (Ord (Var p), Ord (Ann p)) => Ord (ClassItem p)
deriving instance (Data p, Typeable p, Data (Var p), Data (Ann p)) => Data (ClassItem p)

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

instance (Spanned (Constructor p), Spanned (Ann p)) => Spanned (Toplevel p) where
  annotation (LetStmt _ [b]) = annotation b
  annotation (LetStmt _ (b:vs)) = sconcat (annotation b :| map annotation vs)
  annotation (TypeDecl _ _ _ (Just cs) x) = sconcat (annotation x :| map annotation cs)
  annotation (TypeDecl _ _ _ Nothing x) = annotation x
  annotation (ForeignVal _ _ _ _ x) = annotation x
  annotation (Class _ _ _ _ _ x) = annotation x
  annotation (Instance _ _ _ _ x) = annotation x
  annotation _ = internal

instance Spanned (Ann p) => Spanned (ClassItem p) where
  annotation = annotation . view methAnn

instance Pretty (Var p) => Pretty (ClassItem p) where
  pretty (MethodSig v t _) = keyword "val" <+> pretty v <+> colon <+> pretty t
  pretty (DefaultMethod b _) = keyword "let" <+> pretty b

instance Pretty TopAccess where
  pretty Public = keyword "public"
  pretty Private = keyword "private"

prettyAcc :: TopAccess -> Doc
prettyAcc Public = empty
prettyAcc x = pretty x <+> empty

instance Pretty (Var p) => Pretty (Toplevel p) where
  pretty (LetStmt _ []) = string "empty let?"
  pretty (LetStmt m (x:xs)) =
    let prettyBind x = keyword "and" <+> pretty x
     in keyword "let" <+> prettyAcc m <> pretty x
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
  pretty (Open m Nothing) = keyword "open" <+> pretty m
  pretty (Open m (Just a)) = keyword "open" <+> pretty m <+> keyword "as" <+> text a

  pretty (TySymDecl m ty args exp _) =
    keyword "type" <+> prettyAcc m <> pretty ty <+> hsep (map ((squote <>) . pretty) args) <+> pretty exp

  pretty (Module am m bod) =
    vsep [ keyword "module" <+> prettyAcc am <> pretty m <+> equals <+> keyword "begin"
         , indent 2 (align (pretty bod))
         , keyword "end"
         ]

  pretty (Class v am c h m _) =
    vsep [ keyword "class" <+> prettyAcc am <> maybe (parens mempty) pretty c
            <+> soperator (string "=>") <+> pretty v <+> hsep (map pretty h) <+> keyword "begin"
         , indent 2 (align (vsep (map pretty m)))
         , keyword "end"
         ]

  pretty (Instance _ c h m _) =
    vsep [ keyword "instance" <+> maybe (parens mempty) pretty c
            <+> soperator (string "=>") <+> pretty h <+> keyword "begin"
         , indent 2 (align (pretty m))
         , keyword "end"
         ]

instance (Pretty (Var p)) => Pretty [Toplevel p] where
  pretty = vcat . map pretty

instance (Pretty (Var p)) => Pretty (Constructor p) where
  pretty (UnitCon a p _) = prettyAccess a $ pretty p
  pretty (ArgCon a p t _) = prettyAccess a $ pretty p <+> keyword "of" <+> pretty t
  pretty (GadtCon a p t _) = prettyAccess a $ pretty p <+> colon <+> pretty t

prettyAccess :: TopAccess -> Doc -> Doc
prettyAccess Public x = x
prettyAccess Private x = keyword "private" <+> x
