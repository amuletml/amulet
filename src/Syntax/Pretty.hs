{-# LANGUAGE FlexibleContexts, UndecidableInstances, FlexibleInstances, OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Syntax.Pretty
  ( module Syntax
  , module Syntax.Var
  , displayType, applyCons
  ) where

import Control.Arrow (first, second)
import Control.Lens hiding (Lazy)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import Data.List

import Syntax.Subst
import Syntax.Var
import Syntax

import Text.Pretty.Semantic

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

instance (Pretty (Var p)) => Pretty (Expr p) where
  pretty (VarRef v _) = pretty v
  pretty (Let [] _ _) = error "absurd: never parsed"
  pretty (Let ((n, v, _):xs) e _) =
    let prettyBind (n, v, _) = keyword "and" <+> prettyOneBinding n v
     in keyword "let" <+> prettyOneBinding n v
            <#> case xs of
              [] -> keyword "in" <+> pretty e
              _ -> vsep (map prettyBind xs) <#> keyword "in" <+> pretty e
  pretty (If c t e _) = keyword "if" <+> pretty c
                    <#> indent 2 (vsep [ keyword "then" <+> pretty t
                                       , keyword "else" <+> pretty e
                                       ])
  pretty (App f x _) = parenFun f <+> parenArg x
  pretty (InstType t _) = soperator (string "@{") <+> pretty t <+> soperator (char '}')
  pretty (Fun v e _) = keyword "fun" <+> pretty v <+> arrow <+> pretty e
  pretty (Begin e _) =
    vsep [ keyword "begin", indent 2 (vsep (punctuate semi (map pretty e))), keyword "end" ]
  pretty (Literal l _) = pretty l
  pretty (BinOp l o r _) = parens (pretty l <+> pretty o <+> pretty r)
  pretty (Match t bs _) = vsep ((keyword "match" <+> pretty t <+> keyword "with"):prettyMatches bs)
  pretty (Function bs _) = vsep (keyword "function":prettyMatches bs)
  pretty (Hole v _) = "_" <> pretty v -- A typed hole
  pretty (Ascription e t _) = parens $ pretty e <+> colon <+> pretty t
  pretty (Record [] _) = braces empty
  pretty (Record rows _) = record (map (\(n, v) -> text n <+> equals <+> pretty v) rows)
  pretty (RecordExt var rows _) = enclose (char '{' <> space) (space <> char '}') $ pretty var <+> keyword "with" <+> hsep (punctuate comma (prettyRows equals rows))
  pretty (Access x@VarRef{} f _) = pretty x <> dot <> text f
  pretty (Access e f _) = parens (pretty e) <> dot <> text f

  pretty (LeftSection op vl _) = parens $ pretty op <+> pretty vl
  pretty (RightSection op vl _) = parens $ pretty vl <+> pretty op
  pretty (BothSection op _) = parens $ pretty op
  pretty (AccessSection k _) = parens $ dot <> text k
  pretty (Parens e _) = parens $ pretty e
  pretty (InstHole _) = keyword "?"

  pretty (Tuple es _) = parens (hsep (punctuate comma (map pretty es)))
  pretty (TupleSection es _) = parens (hsep (punctuate comma (map (maybe (string "") pretty) es)))

  pretty (OpenIn v e _) = pretty v <+> string "." <+> parens (pretty e)
  pretty (Lazy e _) = keyword "lazy" <+> parenArg e

  pretty (ExprWrapper wrap ex _) = go wrap ex where
    go (TypeLam v t) ex = keyword "fun" <+> braces (pretty (TySkol v) <+> colon <+> pretty t) <> dot <+> pretty ex
    go (Cast c) ex = parens (pretty ex <+> soperator (string "|>") <+> pretty c)
    go (TypeApp t) ex = pretty ex <+> braces (pretty t)
    go (wr Syntax.:> wi) ex = go wr (ExprWrapper wi ex undefined)
    go (WrapVar v) ex = pretty ex <+> soperator (char '_') <> pretty v
    go (WrapFn f) ex = pretty (runWrapper f ex)
    go IdWrap ex = pretty ex

instance Pretty (Var p) => Pretty (Coercion p) where
  pretty (VarCo x) = stypeSkol (pretty x)
  pretty (ReflCo t) = enclose (char '<') (char '>') (pretty t)
  pretty (AssumedCo a b) = enclose (char '<') (char '>') (pretty a <> comma <+> pretty b)
  pretty (SymCo x) = keyword "sym" <+> pretty x
  pretty (AppCo f x) = pretty f <+> pretty x
  pretty (ArrCo f x) = pretty f <+> arrow <+> pretty x
  pretty (ProdCo f x) = pretty f <+> prod <+> pretty x
  pretty (ExactRowsCo rs) = record (map (\(n, v) -> text n <+> colon <+> pretty v) rs)
  pretty (RowsCo c rs) = enclose (lbrace <> space) (space <> rbrace) (pretty c <+> pipe <+> hsep (punctuate comma (map (\(n, v) -> text n <+> colon <+> pretty v) rs)))
  pretty (ProjCo rs rs') = enclose (lbrace <> space) (space <> rbrace) $ pprRow rs <+> keyword "with" <+> pprRow rs' where
    pprRow xs = hsep (punctuate comma (map (\(n, v) -> text n <+> colon <+> pretty v) xs))
  pretty (ForallCo v c cs) = keyword "∀" <> parens (pretty v <+> colon <+> pretty c) <> dot <+> pretty cs

prettyMatches :: (Pretty (Var p)) => [(Pattern p, Expr p)] -> [Doc]
prettyMatches = map (\(a, b) -> pipe <+> nest 4 (pretty a <+> arrow </> pretty b))

prettyRows :: Pretty x => Doc -> [(Text, x)] -> [Doc]
prettyRows sep = map (\(n, v) -> text n <+> sep <+> pretty v) . sortOn fst

instance (Pretty (Var p)) => Pretty (Pattern p) where
  pretty Wildcard{} = skeyword (char '_')
  pretty (Capture x _) = pretty x
  pretty (Destructure x Nothing   _) = stypeCon (pretty x)
  pretty (Destructure x (Just xs) _) = parens $ stypeCon (pretty x) <+> pretty xs
  pretty (PType p x _) = parens $ pretty p <+> colon <+> pretty x
  pretty (PRecord rows _) = record (prettyRows equals rows)
  pretty (PTuple ps _) = parens (hsep (punctuate comma (map pretty ps)))
  pretty (PLiteral l _) = pretty l
  pretty (PWrapper _ p _) = pretty p

instance Pretty Lit where
  pretty (LiStr s) = sstring (dquotes (text s))
  pretty (LiInt s) = sliteral (integer s)
  pretty (LiFloat s) = sliteral (double s)
  pretty (LiBool True) = sliteral (string "true")
  pretty (LiBool False) = sliteral (string "false")
  pretty LiUnit = sliteral (parens empty)

instance (Pretty (Var p)) => Pretty (Type p) where
  pretty (TyCon v) = stypeCon (pretty v)
  pretty (TyPromotedCon v) = stypeCon (pretty v)
  pretty (TyVar v) = stypeVar (squote <> pretty v)
  pretty (TySkol v) = stypeSkol (pretty (v ^. skolIdent) <> text "." <> pretty (v ^. skolVar))

  pretty (TyPi x e) = pretty x <+> pretty e

  pretty (TyRows p rows) = enclose (lbrace <> space) (space <> rbrace)  $ pretty p <+> soperator pipe <+> hsep (punctuate comma (prettyRows colon rows)) 
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

  pretty (TyWithConstraints a b) = parens (hsep (punctuate comma (map prettyEq a))) <+> soperator (char '⊃') <+> pretty b where
    prettyEq (a, b) = pretty a <+> soperator (char '~') <+> pretty b

  pretty TyType = stypeCon (string "type")

instance Pretty (Var p) => Pretty (TyBinder p) where
  pretty (Anon t) = k t (pretty t) <+> arrow where
    k TyPi{} = parens
    k TyTuple{} = parens
    k _ = id
  pretty (Implicit v (Just k)) = braces (stypeVar (squote <> pretty v) <+> colon <+> pretty k) <> dot
  pretty (Implicit v Nothing)  = stypeVar (squote <> pretty v) <> dot

  pretty (Explicit v k) = parens (stypeVar (squote <> pretty v) <+> colon <+> pretty k) <+> arrow

instance (Pretty (Var p)) => Pretty (Toplevel p) where
  pretty (LetStmt []) = error "absurd!"
  pretty (LetStmt ((n, v, _):xs)) =
    let prettyBind (n, v, _) = keyword "and" <+> prettyOneBinding n v
     in keyword "let" <+> prettyOneBinding n v
             <> case xs of
                  [] -> empty
                  _ -> line <> vsep (map prettyBind xs)
  pretty (ForeignVal v d ty _) = keyword "foreign val" <+> pretty v <+> colon <+> pretty ty <+> equals <+> dquotes (text d)
  pretty (TypeDecl ty args ann []) = keyword "type" <+> pretty ty <+> hsep (map ((squote <>) . pretty) args) <> prettyAnnot ann
  pretty (TypeDecl ty args ann ctors) = keyword "type" <+> pretty ty
                                        <+> hsep (map ((squote <>) . pretty) args)
                                        <>  prettyAnnot ann
                                        <+> equals
                                        <#> indent 2 (vsep (map ((pipe <+>) . pretty) ctors))

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

applyCons :: Ord (Var p) => Type p -> Type p
applyCons x@TyCon{} = x
applyCons x@TyVar{} = x
applyCons x@TySkol{} = x
applyCons x@TyType{} = x
applyCons x@TyPromotedCon{} = x
applyCons (TyPi a b) = TyPi (go a) (applyCons b) where
  go (Anon t) = Anon (applyCons t)
  go (Implicit t k) = Implicit t (fmap applyCons k)
  go (Explicit t k) = Explicit t (applyCons k)
applyCons (TyApp a b) = TyApp (applyCons a) (applyCons b)
applyCons (TyRows r rs) = TyRows (applyCons r) (map (second applyCons) rs)
applyCons (TyExactRows rs) = TyExactRows (map (second applyCons) rs)
applyCons (TyTuple a b) = TyTuple (applyCons a) (applyCons b)
applyCons (TyWithConstraints cs a) =
  let eq (TyVar a, t) = Map.singleton a t
      eq _ = Map.empty
      eqs = foldMap eq cs
   in apply eqs a

-- Pretty-print a type, with syntax as close to source Amulet as
-- possible

displayType :: forall p. (Pretty (Var p), Ord (Var p)) => Type p -> Doc
displayType = prettyType . dropKindVars mempty where
  dropKindVars :: Subst p -> Type p -> Type p
  dropKindVars sub (TyPi x@(Implicit v (Just TyType)) t)
    | v `kindVarIn` t, v `Set.member` ftv t = dropKindVars (Map.insert v TyType sub) t
    | otherwise = TyPi x (dropKindVars sub t)

  dropKindVars s (TyPi q ty) = TyPi (apply s q) (dropKindVars s ty)
  dropKindVars _ x@TyPromotedCon{} = x
  dropKindVars s x@TyVar{} = apply s x
  dropKindVars _ x@TyCon{} = x
  dropKindVars _ x@TySkol{} = x
  dropKindVars s (TyApp t y) = TyApp (dropKindVars s t) (dropKindVars s y)
  dropKindVars s (TyRows t rs) = TyRows (dropKindVars s t) (map (second (dropKindVars s)) rs)
  dropKindVars s (TyExactRows rs) = TyExactRows (map (second (dropKindVars s)) rs)
  dropKindVars m (TyTuple s t) = TyTuple (dropKindVars m s) (dropKindVars m t)
  dropKindVars s (TyWithConstraints cs t) = TyWithConstraints cs (dropKindVars s t)
  dropKindVars _ TyType = TyType

  kindVarIn :: Var p -> Type p -> Bool
  kindVarIn v (TyPi (Explicit _ k) t) = v `Set.member` ftv k || kindVarIn v t
  kindVarIn v (TyPi (Implicit _ k) t) = v `Set.member` maybe mempty ftv k || kindVarIn v t
  kindVarIn v (TyPi (Anon a) b) = kindVarIn v a && kindVarIn v b
  kindVarIn _ TyPromotedCon{} = True
  kindVarIn v (TyVar x) = x /= v
  kindVarIn _ TyCon{} = True
  kindVarIn _ TySkol{} = True
  kindVarIn v (TyApp t y) = kindVarIn v t && kindVarIn v y
  kindVarIn v (TyRows t rs) = kindVarIn v t && all (kindVarIn v . snd) rs
  kindVarIn v (TyExactRows rs) = all (kindVarIn v . snd) rs
  kindVarIn v (TyTuple t y) = kindVarIn v t && kindVarIn v y
  kindVarIn v (TyWithConstraints _ t) = kindVarIn v t
  kindVarIn _ TyType = True

prettyType :: forall p. (Pretty (Var p), Ord (Var p)) => Type p -> Doc
prettyType x@TyPromotedCon{} = pretty x
prettyType x@TyVar{} = pretty x
prettyType x@TyCon{} = pretty x
prettyType (TySkol v) = stypeSkol (pretty (v ^. skolVar))
prettyType (TyPi x t) = uncurry prettyQuantifiers . second reverse $ unwind t [x] where
  unwind (TyPi x t) xs = unwind t (x:xs)
  unwind t xs = (t, xs)

  prettyQuantifiers :: Type p -> [TyBinder p] -> Doc
  prettyQuantifiers t [] = prettyType t
  prettyQuantifiers inner (q:qs) =
    let (these, those) = span (sameAs q) qs
        these, those :: [TyBinder p]
     in case q:these of
       Implicit{}:_ -> keyword "forall" <+> hsep (map (stypeVar . (char '\'' <>) . pretty . (^?! tyBinderVar)) (q:these)) <> dot <+> prettyQuantifiers inner those
       Explicit{}:_ -> keyword "forall"
                   <+> let ppr (Explicit v k) = parens (stypeVar (char '\'' <> pretty v) <+> colon <+> prettyType k)
                           ppr _ = undefined
                        in hsep (map ppr (q:these)) <+> arrow
                   <+> prettyQuantifiers inner those
       Anon{}:_ ->
         let arg x = parenTuple x (prettyType x)
          in hsep (punctuate (space <> arrow) (map (arg . (^?! _Anon)) (q:these))) <+> arrow <+> prettyQuantifiers inner those
       [] -> error "what?"

  sameAs Implicit{} Implicit{} = True
  sameAs Explicit{} Explicit{} = True
  sameAs Anon{} Anon{} = True
  sameAs _ _ = False

prettyType (TyApp x e) = parenTyFun x (displayType x) <+> parenTyArg e (displayType e)
prettyType (TyRows p rows) = enclose (lbrace <> space) (space <> rbrace) $
  pretty p <+> soperator pipe <+> hsep (punctuate comma (prettyRows colon rows))
prettyType (TyExactRows rows) = record (prettyRows colon rows)
prettyType (TyTuple t s) = parenTyFun t (displayType t) <+> soperator (char '*') <+> parenTuple s (displayType s)
prettyType t@TyWithConstraints{} = displayType (applyCons t)
prettyType TyType = keyword "type"

prettyAnnot :: Pretty (Var p) => Maybe (Type p) -> Doc
prettyAnnot Nothing = mempty
prettyAnnot (Just ty) = space <> colon <+> pretty ty

parenTyFun, parenTyArg, parenTuple :: Type p -> Doc -> Doc
parenTyArg TyApp{} = parens
parenTyArg x = parenTyFun x

parenTyFun TyPi{} = parens
parenTyFun TyTuple{} = parens
parenTyFun _ = id

parenTuple TyPi{} = parens
parenTuple _ = id
