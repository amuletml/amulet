{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
module Repl.Display where

import Control.Arrow (second)
import Control.Lens

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)

import Text.Pretty.Semantic
import Syntax.Pretty
import Syntax.Subst

-- Pretty-print a type, with syntax as close to source Amulet as
-- possible

displayType :: forall p. (Pretty (Var p), Ord (Var p)) => Type p -> Doc
displayType = prettyType . dropKindVars mempty where
  dropKindVars :: Subst p -> Type p -> Type p
  dropKindVars sub (TyPi x@(Implicit v (Just TyType)) t)
    | v `kindVarIn` t, v `Set.member` (ftv t) = dropKindVars (Map.insert v TyType sub) t
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
prettyType TySkol{} = error "skolem in displayType is a TC bug"
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
       Anon{}:_ -> hsep (punctuate arrow (map (prettyType . (^?! _Anon)) (q:these))) <+> arrow <+> prettyQuantifiers inner those
       [] -> error "what?"

  sameAs Implicit{} Implicit{} = True
  sameAs Explicit{} Explicit{} = True
  sameAs Anon{} Anon{} = True
  sameAs _ _ = False
prettyType (TyApp x e) = parenTyFun x (displayType e) <+> parenTyArg e (displayType e)
prettyType (TyRows p rows) = enclose (lbrace <> space) (space <> rbrace) $
  pretty p <+> soperator pipe <+> hsep (punctuate comma (prettyRows rows)) 
prettyType (TyExactRows rows) = record (prettyRows rows)
prettyType (TyTuple t s) = parenTyFun t (displayType t) <+> soperator (char '*') <+> parenTyFun s (displayType s)
prettyType t@TyWithConstraints{} = displayType (applyCons t)
prettyType TyType = keyword "type"

record :: [Doc] -> Doc
record = enclose (lbrace <> space) (space <> rbrace) . hsep . punctuate comma

prettyRows :: (Pretty (Var p), Ord (Var p)) => [(Text, Type p)] -> [Doc]
prettyRows = map (\(n, v) -> text n <+> colon <+> displayType v)

parenTyFun, parenTyArg :: Type p -> Doc -> Doc
parenTyArg TyApp{} = parens
parenTyArg x = parenTyFun x

parenTyFun TyPi{} = parens
parenTyFun TyTuple{} = parens
parenTyFun _ = id
