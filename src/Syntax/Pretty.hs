{-# LANGUAGE FlexibleContexts, UndecidableInstances
  , FlexibleInstances, OverloadedStrings, ScopedTypeVariables #-}
module Syntax.Pretty (displayType, applyCons, prettyMotive) where

import Control.Lens hiding (Lazy)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Bifunctor
import Data.Text (Text)
import Data.List

import Syntax.Subst
import Syntax.Type
import Syntax.Var

import Text.Pretty.Semantic

applyCons :: Ord (Var p) => Type p -> Type p
applyCons x@TyCon{} = x
applyCons x@TyVar{} = x
applyCons x@TySkol{} = x
applyCons x@TyType{} = x
applyCons x@TyPromotedCon{} = x
applyCons x@TyWildcard{} = x
applyCons (TyPi a b) = TyPi (go a) (applyCons b) where
  go (Anon t) = Anon (applyCons t)
  go (Implicit t) = Implicit (applyCons t)
  go (Invisible t k spec) = Invisible t (fmap applyCons k) spec
applyCons (TyApp a b) = TyApp (applyCons a) (applyCons b)
applyCons (TyRows r rs) = TyRows (applyCons r) (map (second applyCons) rs)
applyCons (TyExactRows rs) = TyExactRows (map (second applyCons) rs)
applyCons (TyTuple a b) = TyTuple (applyCons a) (applyCons b)
applyCons (TyParens t) = TyParens (applyCons t)
applyCons (TyOperator l o r) = TyOperator (applyCons l) o (applyCons r)
applyCons (TyWithConstraints cs a) =
  let eq (TyVar a, t) = Map.singleton a t
      eq _ = Map.empty
      eqs = foldMap eq cs
   in apply eqs a

-- | Pretty-print a type, with syntax as close to source Amulet as
-- possible
displayType :: forall p. (Pretty (Var p), Ord (Var p)) => Type p -> Doc
displayType = prettyType . dropKindVars mempty where
  dropKindVars :: Subst p -> Type p -> Type p
  dropKindVars sub (TyPi x@(Invisible v (Just TyType) _) t)
    | v `kindVarIn` t, v `Set.member` ftv t = dropKindVars (Map.insert v TyType sub) t
    | otherwise = TyPi x (dropKindVars sub t)

  dropKindVars s (TyPi q ty) = TyPi (apply s q) (dropKindVars s ty)
  dropKindVars _ x@TyPromotedCon{} = x
  dropKindVars s x@TyVar{} = apply s x
  dropKindVars s x@TyWildcard{} = apply s x
  dropKindVars _ x@TyCon{} = x
  dropKindVars _ x@TySkol{} = x
  dropKindVars s (TyApp t y) = TyApp (dropKindVars s t) (dropKindVars s y)
  dropKindVars s (TyRows t rs) = TyRows (dropKindVars s t) (map (second (dropKindVars s)) rs)
  dropKindVars s (TyExactRows rs) = TyExactRows (map (second (dropKindVars s)) rs)
  dropKindVars m (TyTuple s t) = TyTuple (dropKindVars m s) (dropKindVars m t)
  dropKindVars s (TyWithConstraints cs t) = TyWithConstraints cs (dropKindVars s t)
  dropKindVars s (TyParens t) = TyParens (dropKindVars s t)
  dropKindVars s (TyOperator l o r) = TyOperator (dropKindVars s l) o (dropKindVars s r)
  dropKindVars _ TyType = TyType

  kindVarIn :: Var p -> Type p -> Bool
  kindVarIn v (TyPi (Invisible _ k _) t) = v `Set.member` foldMap ftv k || kindVarIn v t
  kindVarIn v (TyPi (Anon a) b) = kindVarIn v a && kindVarIn v b
  kindVarIn v (TyPi (Implicit a) b) = kindVarIn v a && kindVarIn v b
  kindVarIn _ TyPromotedCon{} = True
  kindVarIn v (TyVar x) = x /= v
  kindVarIn v (TyWildcard x) = case x of
    Just x -> kindVarIn v x
    Nothing -> True
  kindVarIn _ TyCon{} = True
  kindVarIn _ TyOperator{} = True
  kindVarIn v (TySkol (Skolem _ x _ _)) = x /= v
  kindVarIn v (TyApp t y) = kindVarIn v t && kindVarIn v y
  kindVarIn v (TyRows t rs) = kindVarIn v t && all (kindVarIn v . snd) rs
  kindVarIn v (TyExactRows rs) = all (kindVarIn v . snd) rs
  kindVarIn v (TyTuple t y) = kindVarIn v t && kindVarIn v y
  kindVarIn v (TyWithConstraints _ t) = kindVarIn v t
  kindVarIn v (TyParens t) = kindVarIn v t
  kindVarIn _ TyType = True

prettyType :: forall p. (Pretty (Var p), Ord (Var p)) => Type p -> Doc
prettyType x@TyPromotedCon{} = pretty x
prettyType TyWildcard{} = skeyword (char '_')
prettyType x@TyVar{} = pretty x
prettyType x@TyCon{} = pretty x
prettyType (TySkol v) = stypeSkol (squote <> pretty (v ^. skolVar))
prettyType (TyPi x t) = uncurry prettyQuantifiers . second reverse $ unwind t [x] where
  unwind (TyPi x t) xs = unwind t (x:xs)
  unwind t xs = (t, xs)

  prettyQuantifiers :: Type p -> [TyBinder p] -> Doc
  prettyQuantifiers t [] = prettyType t
  prettyQuantifiers inner (q:qs) =
    let (these, those) = span (sameAs q) qs
        these, those :: [TyBinder p]
     in case q:these of
       Invisible _ _ Spec:_ ->
         keyword "forall"
         <+> hsep (map (stypeVar . (char '\'' <>) . pretty . (^?! tyBinderVar)) (q:these))
         <> dot <+> prettyQuantifiers inner those
       Invisible _ _ Infer:_ -> prettyQuantifiers inner those
       Invisible _ _ Req:_ ->
         keyword "forall"
         <+> hsep (map (stypeVar . (char '\'' <>) . pretty . (^?! tyBinderVar)) (q:these))
         <+> arrow <+> prettyQuantifiers inner those
       Anon{}:_ ->
         let arg x = parenTuple x (prettyType x)
          in hsep (punctuate (space <> arrow) (map (arg . (^?! _Anon)) (q:these)))
             <+> arrow <+> prettyQuantifiers inner those
       Implicit{}:_ ->
         let arg x = parenTuple x (prettyType x)
             arrow = soperator (string "=>")
             star = soperator (char '*')
          in hsep (punctuate (space <> star) (map (arg . (^?! _Implicit)) (q:these)))
             <+> arrow <+> prettyQuantifiers inner those
       [] -> error "what?"

  sameAs (Invisible _ _ x) (Invisible _ _ y) = x == y
  sameAs Anon{} Anon{} = True
  sameAs Implicit{} Implicit{} = True
  sameAs _ _ = False
prettyType (TyApp x e) = parenTyFun x (displayType x) <+> parenTyArg e (displayType e)
prettyType (TyRows p rows) = enclose (lbrace <> space) (space <> rbrace) $
  pretty p <+> soperator pipe <+> hsep (punctuate comma (displayRows rows))
prettyType (TyExactRows rows) = record (displayRows rows)
prettyType (TyTuple t s) =
  parenTyFun t (displayType t) <+> soperator (char '*') <+> parenTuple s (displayType s)
prettyType t@TyWithConstraints{} = displayType (applyCons t)
prettyType (TyParens t) = parens $ prettyType t
prettyType (TyOperator l o r) = prettyType l <+> pretty o <+> prettyType r
prettyType TyType = keyword "type"

displayRows :: (Ord (Var p), Pretty (Var p)) => [(Text, Type p)] -> [Doc]
displayRows = map (\(n, v) -> text n <+> colon <+> displayType v) . sortOn fst

parenTyFun, parenTyArg, parenTuple :: Type p -> Doc -> Doc
parenTyArg TyApp{} = parens
parenTyArg x = parenTyFun x

parenTyFun TyPi{} = parens
parenTyFun TyTuple{} = parens
parenTyFun _ = id

parenTuple TyPi{} = parens
parenTuple _ = id

prettyMotive :: SkolemMotive Typed -> Doc
prettyMotive (ByAscription _ t) = string "of the context, the type" <#> displayType t
prettyMotive (BySubsumption t1 t2) =
  string "of a requirement that" <+> displayType t1 <#> string "be as polymorphic as" <+> displayType t2
prettyMotive (ByExistential v t) =
  string "it is an existential" <> comma
  <#> string "bound by the type of" <+> pretty v <> comma <+> displayType t
prettyMotive (ByInstanceHead head _) = string "it is bound in an instance head, namely" <#> displayType head
prettyMotive (ByConstraint con) =
  string "it is mentioned in a type class constraint, namely" <#> displayType con
