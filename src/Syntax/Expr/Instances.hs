{-# LANGUAGE FlexibleContexts, OverloadedStrings, TypeFamilies, UndecidableInstances, MultiParamTypeClasses, StandaloneDeriving, DerivingVia #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Syntax.Expr.Instances () where

import Control.Lens (view)

import Text.Pretty.Semantic

import Data.Bifunctor
import Data.Spanned
import Data.List
import Data.Char

import Syntax.Expr
import Syntax.Type
import Syntax.Var

instance Annotated (Binding p) where
  type Annotation (Binding p) = Ann p
  annotation = _bindAnn

instance Annotated (Expr p) where
  type Annotation (Expr p) = Ann p
  annotation (VarRef _ a) = a
  annotation (Let _ _ _ a) = a
  annotation (If _ _ _ a) = a
  annotation (App _ _ a) = a
  annotation (Fun _ _ a) = a
  annotation (Begin _ a) = a
  annotation (Literal _ a) = a
  annotation (Match _ _ _ a) = a
  annotation (Function _ _ a) = a
  annotation (BinOp _ _ _ a) = a
  annotation (Hole _ a) = a
  annotation (Ascription _ _ a) = a

  annotation (Record _ a) = a
  annotation (RecordExt _ _ a) = a
  annotation (Access _ _ a) = a

  annotation (LeftSection _ _ a) = a
  annotation (RightSection _ _ a) = a
  annotation (BothSection _ a) = a
  annotation (AccessSection _ a) = a
  annotation (Parens _ a) = a

  annotation (Tuple _ a) = a
  annotation (TupleSection _ a) = a

  annotation (OpenIn _ _ a) = a
  annotation (Lazy _ a) = a
  annotation (Vta _ _ a) = a
  annotation (ListExp _ a) = a
  annotation (ListComp _ _ a) = a
  annotation (DoExpr _ _ a) = a
  annotation (Idiom _ _ _ a) = a

  annotation (ListFrom _ _ a) = a
  annotation (ListFromTo _ _ _ a) = a
  annotation (ListFromThen _ _ _ a) = a
  annotation (ListFromThenTo _ _ _ _ a) = a

  annotation (ExprWrapper _ _ a) = a

instance Annotated (Pattern p) where
  type Annotation (Pattern p) = Ann p
  annotation (Wildcard a) = a
  annotation (Capture _ a) = a
  annotation (Destructure _ _ a) = a
  annotation (PAs _ _ a) = a
  annotation (PType _ _ a) = a
  annotation (PRecord _ a) = a
  annotation (PTuple _ a) = a
  annotation (PLiteral _ a) = a
  annotation (PGadtCon _ _ _ _ a) = a
  annotation (PList _ a) = a

instance Annotated (Arm p) where
  type Annotation (Arm p) = RawAnn p
  annotation = armAnn

instance Annotated (Parameter p) where
  type Annotation (Parameter p) = Ann p
  annotation = annotation . view paramPat

instance Annotated (CompStmt p) where
  type Annotation (CompStmt p) = Ann p
  annotation (CompGuard ex) = annotation ex
  annotation (CompLet _ a) = a
  annotation (CompGen _ _ a) = a

deriving via AnnotatedVia (Binding p)   (Ann p)    instance Spanned (Ann p)    => Spanned (Binding p)
deriving via AnnotatedVia (Expr p)      (Ann p)    instance Spanned (Ann p)    => Spanned (Expr p)
deriving via AnnotatedVia (Pattern p)   (Ann p)    instance Spanned (Ann p)    => Spanned (Pattern p)
deriving via AnnotatedVia (Arm p)       (RawAnn p) instance Spanned (RawAnn p) => Spanned (Arm p)
deriving via AnnotatedVia (Parameter p) (Ann p)    instance Spanned (Ann p)    => Spanned (Parameter p)
deriving via AnnotatedVia (CompStmt p)  (Ann p)    instance Spanned (Ann p)    => Spanned (CompStmt p)

parenFun :: Pretty (Var p) => Expr p -> Doc
parenFun f = case f of
  Fun{} -> parens (pretty f)
  Let{} -> parens (pretty f)
  Match{} -> parens (pretty f)
  _ -> pretty f

parenArg :: Pretty (Var p) => Expr p -> Doc
parenArg f = case f of
  ExprWrapper w ex _ -> case w of
    IdWrap -> parenArg ex
    _ -> parens (pretty f)
  App{} -> parens (pretty f)
  _ -> parenFun f

letRec :: RecKind -> Doc
letRec Recursive = keyword "rec" <> space
letRec NonRecursive = mempty

instance Pretty (Var p) => Pretty (Expr p) where
  pretty (VarRef v _) =
    let x = show (pretty v)
     in if isUpper (head x)
           then stypeCon (pretty v)
           else pretty v
  pretty (Let r [] _ _) = keyword "let" <+> letRec r <> braces mempty
  pretty (Let r (x:xs) e _) =
    let prettyBind x = keyword "and" <+> pretty x
     in keyword "let" <+> letRec r <> pretty x
            <#> case xs of
              [] -> keyword "in" <+> pretty e
              _ -> vsep (map prettyBind xs) <#> keyword "in" <+> pretty e
  pretty (If c t e _) = keyword "if" <+> pretty c
                    <#> indent 2 (vsep [ keyword "then" <+> pretty t
                                       , keyword "else" <+> pretty e
                                       ])
  pretty (App f x _) = parenFun f <+> parenArg x
  pretty f@Fun{} = keyword "fun" <+> funs f where
    funs (Fun v e _) = pretty v <+> funs e
    funs e = arrow <+> pretty e
  pretty (Begin e _) =
    vsep [ keyword "begin", indent 2 (vsep (punctuate semi (map pretty e))), keyword "end" ]
  pretty (DoExpr _ e _) =
    vsep [ keyword "begin", indent 2 (vsep (punctuate semi (map pretty e))), keyword "end" ]
  pretty (Literal l _) = pretty l
  pretty (BinOp l o r _) = parens (pretty l <+> pretty o <+> pretty r)
  pretty (Match t bs _ _) = vsep ((keyword "match" <+> pretty t <+> keyword "with"):map pretty bs)
  pretty (Function [] _ _) = keyword "function" <+> parens mempty
  pretty (Function bs _ _) = vsep (keyword "function":map pretty bs)
  pretty (Hole v _) = "_" <> pretty v -- A typed hole
  pretty (Ascription e t _) = parens $ pretty e <+> colon <+> pretty t
  pretty (Record [] _) = braces empty
  pretty (Record rows _) = record (map (\(Field n v _) -> text n <+> equals <+> pretty v) rows)
  pretty (RecordExt var rows _) =
    enclose (char '{' <> space)
      (space <> char '}') $
      pretty var <+> keyword "with" <+> hsep (punctuate comma (prettyRows' equals rows))
  pretty (Access e f _) = parenArg e <> dot <> text f

  pretty (LeftSection op vl _) = parens $ pretty op <+> pretty vl
  pretty (RightSection op vl _) = parens $ pretty vl <+> pretty op
  pretty (BothSection op _) = parens $ pretty op
  pretty (AccessSection k _) = parens $ dot <> text k
  pretty (Parens e _) = parens $ pretty e

  pretty (Tuple es _) = parens (hsep (punctuate comma (map pretty es)))
  pretty (TupleSection es _) = parens (hsep (punctuate comma (map (maybe (string "") pretty) es)))

  pretty (OpenIn v e _) = pretty v <> string "." <> parens (pretty e)
  pretty (Lazy e _) = keyword "lazy" <+> parenArg e
  pretty (Vta e t _) = parenFun e <+> keyword "as" <+> pretty t

  pretty (ListExp es _) = brackets (hsep (punctuate comma (map pretty es)))
  pretty (ListComp e qs _) =
    brackets (pretty e <+> pipe <+> hsep (punctuate comma (map pretty qs)))
  pretty (ListFrom _ x _) =
    brackets (pretty x <+> soperator (string ".."))
  pretty (ListFromTo _ x y _) =
    brackets (pretty x <+> soperator (string "..") <+> pretty y)
  pretty (ListFromThen _ x y _) =
    brackets (pretty x <> comma <> pretty y <+> soperator (string ".."))
  pretty (ListFromThenTo _ x y z _) =
    brackets (pretty x <> comma <> pretty y <+> soperator (string "..") <+> pretty z)

  pretty (Idiom _ _ xs _) = "(|" <+> pretty xs <+> "|)"

  pretty (ExprWrapper wrap ex an) = go wrap ex where
    go (TypeLam v t) ex =
      keyword "fun" <+> braces (pretty (TySkol v) <+> colon <+> pretty t) <> dot <+> pretty ex
    go (Cast c) ex = parens (pretty ex <+> soperator (string "|>") <+> pretty c)
    go (TypeApp t) ex = pretty ex <+> braces (pretty t)
    go (ExprApp t) ex = pretty (App ex t undefined)
    go (TypeAsc _) ex = pretty ex
    go (wr :> wi) ex = go wr (ExprWrapper wi ex an)
    go (WrapVar v) ex = pretty ex <+> soperator (char '_') <> pretty v
    go (WrapFn f) ex = pretty (runWrapper f ex)
    go IdWrap ex = pretty ex

instance Pretty (Var p) => Pretty (CompStmt p) where
  pretty (CompGen p e _) = pretty p <+> soperator (string "<-") <+> pretty e
  pretty (CompLet [] _) = keyword "let" <> braces mempty
  pretty (CompLet (x:xs) _) =
    let prettyBind x = keyword "and" <+> pretty x
     in keyword "let" <+> pretty x
            <+> case xs of
              [] -> empty
              _ -> hsep (map prettyBind xs)
  pretty (CompGuard e) = pretty e

instance Pretty (Var p) => Pretty (Arm p) where
  pretty (Arm p g b _) = pipe <+> nest 4 (pretty p <+> prettyGuard g <> arrow </> pretty b) where
    prettyGuard Nothing = mempty
    prettyGuard (Just g) = keyword "when" <+> pretty g <+> mempty

instance (Pretty (Var p)) => Pretty (Pattern p) where
  pretty Wildcard{} = skeyword (char '_')
  pretty (Capture x _) = pretty x
  pretty (Destructure x Nothing   _) = stypeCon (pretty x)
  pretty (Destructure x (Just xs) _) = parens $ stypeCon (pretty x) <+> pretty xs
  pretty (PAs p v _) = pretty p <+> keyword "as" <+> pretty v
  pretty (PType p x _) = parens $ pretty p <+> colon <+> pretty x
  pretty (PRecord rows _) = record (prettyRows equals rows)
  pretty (PTuple ps _) = parens (hsep (punctuate comma (map pretty ps)))
  pretty (PList ps _) = brackets (hsep (punctuate comma (map pretty ps)))
  pretty (PLiteral l _) = pretty l
  pretty (PGadtCon v _ vs p _) = parens $ pretty v <+> hsep (map (pretty . fst) vs) <> spc <> foldMap pretty p where
    spc = foldMap (const (char ' ')) p

instance Pretty (Var p) => Pretty (Binding p) where
  pretty (Binding n v _ _) = hsep (pretty n:map pretty args) <> sig <+> nest 2 (equals </> pretty rest') where
    (args, rest) = takeLambdas v
    (sig, rest') = case rest of
      Ascription e t _ -> (space <> colon <+> pretty t, e)
      _ -> (empty, rest)

    takeLambdas (Fun p x _) = first (p:) . takeLambdas $ x
    takeLambdas x = ([], x)
  pretty (Matching p e _) = pretty p <+> nest 2 (equals </> pretty e)
  pretty (TypedMatching p e _ _) = pretty p <+> equals <+> pretty e


instance Pretty (Var p) => Pretty (Parameter p) where
  pretty = pretty . view paramPat

prettyRows' :: Pretty (Var p) => Doc -> [Field p] -> [Doc]
prettyRows' sep = map (\(Field n v _) -> text n <+> sep <+> pretty v) . sortOn (view fName)
