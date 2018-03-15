{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances, FlexibleInstances #-}
module Syntax.Pretty
  ( module Syntax
  , tidyPrettyType
  ) where

import Control.Arrow (first, second)
import Control.Lens

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Span

import Syntax.Subst
import Syntax
import Pretty

parenFun :: Pretty (Var p) => Expr p -> Doc
parenFun f = case f of
  Fun{} -> parens (pretty f)
  Let{} -> parens (pretty f)
  Match{} -> parens (pretty f)
  _ -> pretty f

parenArg :: Pretty (Var p) => Expr p -> Doc
parenArg f = case f of
  TypeApp{} -> parens (pretty f)
  App{} -> parens (pretty f)
  _ -> parenFun f

instance (Pretty (Var p)) => Pretty (Expr p) where
  pretty (VarRef v _) = pretty v
  pretty (Let [] _ _) = error "absurd: never parsed"
  pretty (Let ((n, v, _):xs) e _) =
    let prettyBind (n, v, _) = keyword "and" <+> prettyOneBinding n v
     in align $ keyword "let" <+> prettyOneBinding n v
            <#> case xs of
              [] -> keyword "in" <+> pretty e
              _ -> vsep (map prettyBind xs) <#> keyword "in" <+> pretty e
  pretty (If c t e _) = keyword "if" <+> pretty c
                    <#> indent 2 (vsep [ keyword "then" <+> pretty t
                                       , keyword "else" <+> pretty e
                                       ])
  pretty (App f x _) = parenFun f <+> parenArg x
  pretty (Fun v e _) = keyword "fun" <+> pretty v <+> nest 2 (arrow </> pretty e)
  pretty (Begin e _) =
    vsep [ keyword "begin", indent 2 (vsep (punctuate semi (map pretty e))), keyword "end" ]
  pretty (Literal l _) = pretty l
  pretty (BinOp l o r _) = parens (pretty l <+> pretty o <+> pretty r)
  pretty (Match t bs _) = vsep ((keyword "match" <+> pretty t <+> keyword "with"):prettyMatches bs)
  pretty (Hole v _) = pretty v -- A typed hole
  pretty (Ascription e t _) = parens $ pretty e <+> colon <+> pretty t
  pretty (Record rows _) = record (map (\(n, v) -> text n <+> equals <+> pretty v) rows)
  pretty (RecordExt var rows _) = braces $ pretty var <> keyword "with" <> hsep (punctuate comma (prettyRows equals rows))
  pretty (Access x@VarRef{} f _) = pretty x <> dot <> text f
  pretty (Access e f _) = parens (pretty e) <> dot <> text f

  pretty (LeftSection op vl _) = parens $ pretty op <+> pretty vl
  pretty (RightSection op vl _) = parens $ pretty vl <+> pretty op
  pretty (BothSection op _) = parens $ pretty op
  pretty (AccessSection k _) = parens $ dot <> text k

  pretty (Tuple es _) = parens (hsep (punctuate comma (map pretty es)))
  pretty (TupleSection es _) = parens (hsep (punctuate comma (map (maybe (string "") pretty) es)))
  pretty (TypeApp f x _) = parenFun f <+> soperator (char '@') <> pretty x

prettyMatches :: (Pretty (Var p)) => [(Pattern p, Expr p)] -> [Doc]
prettyMatches = map (\(a, b) -> pipe <+> nest 4 (pretty a <+> arrow </> pretty b))

prettyRows :: Pretty x => Doc -> [(Text, x)] -> [Doc]
prettyRows sep = map (\(n, v) -> text n <+> sep <+> pretty v)

instance (Pretty (Var p)) => Pretty (Pattern p) where
  pretty Wildcard{} = skeyword (char '_')
  pretty (Capture x _) = pretty x
  pretty (Destructure x Nothing   _) = stypeCon (pretty x)
  pretty (Destructure x (Just xs) _) = parens $ stypeCon (pretty x) <+> pretty xs
  pretty (PType p x _) = parens $ pretty p <+> colon <+> pretty x
  pretty (PRecord rows _) = record (prettyRows equals rows)
  pretty (PTuple ps _) = parens (hsep (punctuate comma (map pretty ps)))

instance Pretty Lit where
  pretty (LiStr s) = sstring (dquotes (text s))
  pretty (LiInt s) = sliteral (integer s)
  pretty (LiFloat s) = sliteral (double s)
  pretty (LiBool True) = sliteral (string "true")
  pretty (LiBool False) = sliteral (string "false")
  pretty LiUnit = sliteral (parens empty)

instance (Pretty (Var p)) => Pretty (Type p) where
  pretty (TyCon v) = stypeCon (pretty v)
  pretty (TyVar v) = stypeVar (squote <> pretty v)
  pretty (TySkol v) = stypeSkol (pretty (v ^. skolIdent))
  pretty (TyForall vs v)
    = keyword "forall" <+> hsep (map (stypeVar . (squote <>) . pretty) vs) <> dot <+> pretty v

  pretty (TyArr x e)
    | TyArr{} <- x = parens (pretty x) <+> arrow <+> pretty e
    | TyForall{} <- x = parens (pretty x) <+> arrow <+> pretty e
    | TyTuple{} <- x = parens (pretty x) <+> arrow <+> pretty e
    | otherwise = pretty x <+> arrow <+> pretty e

  pretty (TyRows p rows) = enclose (lbrace <> space) (space <> rbrace)  $ pretty p <+> soperator pipe <+> hsep (punctuate comma (prettyRows colon rows)) 
  pretty (TyExactRows rows) = record (prettyRows colon rows)

  pretty (TyApp x e) = pretty x <+> parenTyArg e (pretty e) where
    parenTyArg TyApp{} = parens
    parenTyArg TyForall{} = parens
    parenTyArg TyArr{} = parens
    parenTyArg TyTuple{} = parens
    parenTyArg _ = id

  pretty (TyTuple a b)
    | TyTuple{} <- a
    = parens (pretty a) <+> prod <+> pretty b
    | otherwise
    = pretty a <+> prod <+> pretty b

  pretty (TyWithConstraints a b) = parens (hsep (punctuate comma (map prettyEq a))) <+> soperator (char '⊃') <+> pretty b where
    prettyEq (a, b) = pretty a <+> soperator (char '~') <+> pretty b

instance Pretty (Var p) => Pretty (Kind p) where
  pretty KiStar = stypeCon (string "Type")
  pretty (KiArr a b)
    | KiArr{} <- a = parens (pretty a) <+> arrow <+> pretty b
    | otherwise = pretty a <+> arrow <+> pretty b

  pretty (KiVar v) = squote <> pretty v
  pretty (KiForall vs v)
    = keyword "forall" <+> hsep (map ((squote <>) . pretty) vs) <> dot <+> pretty v

instance (Pretty (Var p)) => Pretty (Toplevel p) where
  pretty (LetStmt []) = error "absurd!"
  pretty (LetStmt ((n, v, _):xs)) =
    let prettyBind (n, v, _) = keyword "and" <+> prettyOneBinding n v
     in align $ keyword "let" <+> prettyOneBinding n v
             <> case xs of
                  [] -> empty
                  _ -> line <> vsep (map prettyBind xs)
  pretty (ForeignVal v d ty _) = keyword "foreign val" <+> pretty v <+> colon <+> pretty ty <+> equals <+> dquotes (text d)
  pretty (TypeDecl ty args []) = keyword "type" <+> pretty ty <+> hsep (map ((squote <>) . pretty) args)
  pretty (TypeDecl ty args ctors) = keyword "type" <+> pretty ty
                                <+> hsep (map ((squote <>) . pretty) args)
                                <+> equals
                                <#> vsep (map ((pipe <+>) . pretty) ctors)

  pretty (Open m Nothing) = keyword "open" <+> pretty m
  pretty (Open m (Just a)) = keyword "open" <+> pretty m <+> keyword "as" <+> text a

  pretty (Module m bod) =
    vsep [ keyword "module" <+> pretty m <+> equals <+> keyword "begin"
         , indent 2 (align (pretty bod))
         , keyword "end"
         ]

instance (Pretty (Var p)) => Pretty [Toplevel p] where
  pretty = vcat . map pretty

instance (Pretty (Var p)) => Pretty (Constructor p) where
  pretty (UnitCon p _) = pretty p
  pretty (ArgCon p t _) = pretty p <+> keyword "of" <+> pretty t
  pretty (GeneralisedCon p t _) = pretty p <+> colon <+> pretty t

instance Pretty (Var Parsed) where
  pretty (Name v) = text v
  pretty (InModule t v) = text t <> dot <> pretty v

instance Pretty (Var Resolved) where
  pretty (TgName v i) = text v <> scomment (string "#" <> string (show i))
  pretty (TgInternal v) = text v

instance Pretty (Var Typed) where
  pretty (TvName v) = pretty v
  -- pretty (TvName v t)
    -- | t == internalTyVar = pretty v
    -- | otherwise = parens $ v <+> opClr " : " <+> t
    --
instance Pretty (Span, Type Typed) where
  pretty (x, _) = pretty x

record :: [Doc] -> Doc
record = enclose (lbrace <> space) (space <> rbrace) . hsep . punctuate comma

prettyOneBinding :: Pretty (Var p) => Var p -> Expr p -> Doc
prettyOneBinding n v = hsep (pretty n:map pretty args) <> sig <+> nest 2 (equals </> pretty rest') where
  (args, rest) = takeLambdas v
  (sig, rest') = case rest of
    Ascription e t _ -> (space <> colon <+> pretty t, e)
    _ -> (empty, rest)

  takeLambdas (Fun p x _) = first (p:) . takeLambdas $ x
  takeLambdas x = ([], x)

tidyPrettyType :: (Pretty (Var p), Ord (Var p)) => Type p -> Doc
tidyPrettyType = pretty . applyCons

applyCons :: Ord (Var p) => Type p -> Type p
applyCons x@TyCon{} = x
applyCons x@TyVar{} = x
applyCons x@TySkol{} = x
applyCons (TyForall vs t) = TyForall vs (applyCons t)
applyCons (TyArr a b) = TyArr (applyCons a) (applyCons b)
applyCons (TyApp a b) = TyApp (applyCons a) (applyCons b)
applyCons (TyRows r rs) = TyRows (applyCons r) (map (second applyCons) rs)
applyCons (TyExactRows rs) = TyExactRows (map (second applyCons) rs)
applyCons (TyTuple a b) = TyTuple (applyCons a) (applyCons b)
applyCons (TyWithConstraints cs a) =
  let eq (TyVar a, t) = Map.singleton a t
      eq _ = Map.empty
      eqs = foldMap eq cs
   in apply eqs a
