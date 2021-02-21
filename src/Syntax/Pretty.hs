{-# LANGUAGE FlexibleContexts, UndecidableInstances
  , FlexibleInstances, OverloadedStrings, ScopedTypeVariables, GADTs, ViewPatterns #-}
module Syntax.Pretty (displayType, applyCons, prettyMotive, displayTypeTyped) where

import Control.Lens hiding (Lazy)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Bifunctor
import Data.Text (Text)
import Data.Char
import Data.List

import Syntax.Builtin
import Syntax.Subst
import Syntax.Type
import Syntax.Var

import Text.Pretty.Semantic

applyCons :: Ord (Var p) => Type p -> Type p
applyCons x@TyCon{} = x
applyCons x@TyLit{} = x
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
applyCons (TyTupleL a b) = TyTupleL (applyCons a) (applyCons b)
applyCons (TyParens t) = TyParens (applyCons t)
applyCons (TyOperator l o r) = TyOperator (applyCons l) o (applyCons r)
applyCons (TyWithConstraints cs a) =
  let eq (TyVar a _, t) = Map.singleton a t
      eq _ = Map.empty
      eqs = foldMap eq cs
   in apply eqs a

-- | Pretty-print a type, with syntax as close to source Amulet as
-- possible
displayType :: forall p. (Pretty (Var p), Ord (Var p)) => Type p -> Doc
displayType = prettyType . undependentify . dropKindVars mempty

displayTypeTyped :: Var p ~ Var Resolved => Type p -> Doc
displayTypeTyped = prettyTypeTyped . undependentify . dropKindVars mempty

dropKindVars :: forall p. (Pretty (Var p), Ord (Var p)) => Subst p -> Type p -> Type p
dropKindVars sub (TyPi x@(Invisible v (Just TyType) r) t)
  | r == Infer, v `kindVarIn` t, v `Set.member` ftv t = dropKindVars (Map.insert v TyType sub) t
  | otherwise = TyPi x (dropKindVars sub t)

dropKindVars s (TyPi q ty) = TyPi (apply s q) (dropKindVars s ty)
dropKindVars _ x@TyPromotedCon{} = x
dropKindVars s x@TyVar{} = apply s x
dropKindVars s x@TyWildcard{} = apply s x
dropKindVars _ x@TyCon{} = x
dropKindVars _ x@TySkol{} = x
dropKindVars _ x@TyLit{} = x
dropKindVars s (TyApp t y) = TyApp (dropKindVars s t) (dropKindVars s y)
dropKindVars s (TyRows t rs) = TyRows (dropKindVars s t) (map (second (dropKindVars s)) rs)
dropKindVars s (TyExactRows rs) = TyExactRows (map (second (dropKindVars s)) rs)
dropKindVars m (TyTuple s t) = TyTuple (dropKindVars m s) (dropKindVars m t)
dropKindVars m (TyTupleL s t) = TyTupleL (dropKindVars m s) (dropKindVars m t)
dropKindVars s (TyWithConstraints cs t) = TyWithConstraints cs (dropKindVars s t)
dropKindVars s (TyParens t) = TyParens (dropKindVars s t)
dropKindVars s (TyOperator l o r) = TyOperator (dropKindVars s l) o (dropKindVars s r)
dropKindVars _ TyType = TyType

kindVarIn :: forall p. Ord (Var p) => Var p -> Type p -> Bool
kindVarIn v (TyPi (Invisible _ k _) t) = v `Set.member` foldMap ftv k || kindVarIn v t
kindVarIn v (TyPi (Anon a) b) = kindVarIn v a && kindVarIn v b
kindVarIn v (TyPi (Implicit a) b) = kindVarIn v a && kindVarIn v b
kindVarIn _ TyPromotedCon{} = True
kindVarIn v (TyVar x _) = x /= v
kindVarIn v (TyWildcard x) = case x of
  Just x -> kindVarIn v x
  Nothing -> True
kindVarIn _ TyCon{} = True
kindVarIn _ TyOperator{} = True
kindVarIn _ TyLit{} = True
kindVarIn v (TySkol (Skolem _ x _ _)) = x /= v
kindVarIn v (TyApp t y) = kindVarIn v t && kindVarIn v y
kindVarIn v (TyRows t rs) = kindVarIn v t && all (kindVarIn v . snd) rs
kindVarIn v (TyExactRows rs) = all (kindVarIn v . snd) rs
kindVarIn v (TyTupleL t y) = kindVarIn v t && kindVarIn v y
kindVarIn v (TyTuple t y) = kindVarIn v t && kindVarIn v y
kindVarIn v (TyWithConstraints _ t) = kindVarIn v t
kindVarIn v (TyParens t) = kindVarIn v t
kindVarIn _ TyType = True

prettyType :: forall p. (Pretty (Var p), Ord (Var p)) => Type p -> Doc
prettyType x@TyPromotedCon{} = pretty x
prettyType (TyWildcard x) = case x of
  Just ty -> displayType ty
  Nothing -> skeyword (char '_')
prettyType x@TyVar{} = pretty x
prettyType x@TyCon{} = pretty x
prettyType (TySkol v) = stypeSkol (squote <> pretty (v ^. skolVar))
prettyType (TyPi x t) = uncurry (prettyQuantifiers prettyType) . second reverse $ unwind t [x] where
  unwind (TyPi x t) xs = unwind t (x:xs)
  unwind t xs = (t, xs)

prettyType (TyApp (TyApp v l) r) | isOpName v = prettyType (TyOperator l v r)
-- Hack for the guarded unification magic type: \/
prettyType (TyApp (TyCon v _) x) | show (pretty v) == mempty = displayType x
-- This is really gross

prettyType (TyApp x e) = parenTyFun x (displayType x) <+> parenTyArg e (displayType e)

prettyType (flatRows -> Just (Just p, rows)) = enclose (lbrace <> space) (space <> rbrace) $
  pretty p <+> soperator pipe <+> hsep (punctuate comma (displayRows rows))
prettyType (flatRows -> Just (Nothing, rows)) = record (displayRows rows)
prettyType TyRows{} = error "unmatched flatRows"
prettyType TyExactRows{} = error "unmatched flatRows"

prettyType (TyTupleL x y) = parens $ prettyType x <> comma <+> prettyType y
prettyType (TyTuple t s) =
  parenTyFun t (displayType t) <+> soperator (char '*') <+> parenTuple s (displayType s)
prettyType t@TyWithConstraints{} = displayType (applyCons t)
prettyType (TyParens t) = parens $ prettyType t
prettyType (TyOperator l o r) = prettyType l <+> pretty o <+> prettyType r
prettyType TyType = keyword "type"
prettyType (TyLit l) = pretty l

prettyTypeTyped :: forall p. Var p ~ Var Resolved => Type p -> Doc
prettyTypeTyped x | Just l <- listType x = brackets (hsep (punctuate comma (map prettyTypeTyped l)))
prettyTypeTyped x@TyVar{} = pretty x
prettyTypeTyped x@TyCon{} = pretty x
prettyTypeTyped t@TyWithConstraints{} = displayTypeTyped (applyCons t)
prettyTypeTyped x@TyPromotedCon{} = pretty x

prettyTypeTyped (TyWildcard x) = case x of
  Just ty -> displayTypeTyped ty
  Nothing -> skeyword (char '_')
prettyTypeTyped (TySkol v) = stypeSkol (squote <> pretty (v ^. skolVar))
prettyTypeTyped (TyPi x t) = uncurry (prettyQuantifiers prettyTypeTyped) . second reverse $ unwind t [x] where
  unwind (TyPi x t) xs = unwind t (x:xs)
  unwind t xs = (t, xs)

prettyTypeTyped (TyApp (TyApp v l) r) | isOpName v = prettyTypeTyped (TyOperator l v r)
-- Hack for the guarded unification magic type: \/
prettyTypeTyped (TyApp (TyCon v _) x) | show (pretty v) == mempty = displayTypeTyped x
-- This is really gross

prettyTypeTyped (TyApp x e) = parenTyFun x (displayType x) <+> parenTyArg' e (displayTypeTyped e) where
  parenTyArg' e d | Just _ <- listType e = d
  parenTyArg' e d = parenTyArg e d
prettyTypeTyped (TyTupleL x y) = parens $ prettyTypeTyped x <> comma <+> prettyTypeTyped y
prettyTypeTyped (TyTuple t s) =
  parenTyFun t (displayTypeTyped t) <+> soperator (char '*') <+> parenTuple s (displayTypeTyped s)
prettyTypeTyped (TyParens t) = parens $ prettyTypeTyped t
prettyTypeTyped (TyOperator l o r) = prettyTypeTyped l <+> pretty o <+> prettyTypeTyped r
prettyTypeTyped TyType = keyword "type"
prettyTypeTyped (TyLit l) = pretty l
prettyTypeTyped (flatRows -> Just (Just p, rows)) = enclose (lbrace <> space) (space <> rbrace) $
  pretty p <+> soperator pipe <+> hsep (punctuate comma (displayRows rows))
prettyTypeTyped (flatRows -> Just (Nothing, rows)) = record (displayRowsTyped rows)
prettyTypeTyped TyRows{} = error "unmatched flatRows"
prettyTypeTyped TyExactRows{} = error "unmatched flatRows"

flatRows :: Type p -> Maybe (Maybe (Type p), [(Text, Type p)])
flatRows (TyExactRows rs) = Just (Nothing, rs)
flatRows (TyRows t rs) =
  case flatRows t of
    Nothing -> Just (Just t, rs)
    Just (t', rs') -> Just (t', rs ++ rs')
flatRows _ = Nothing

displayRows :: (Ord (Var p), Pretty (Var p)) => [(Text, Type p)] -> [Doc]
displayRows = map (\(n, v) -> text n <+> colon <+> displayType v) . sortOn fst

displayRowsTyped :: Var p ~ Var Resolved => [(Text, Type p)] -> [Doc]
displayRowsTyped = map (\(n, v) -> text n <+> colon <+> displayTypeTyped v) . sortOn fst

prettyQuantifiers :: forall p. (Pretty (Var p), Ord (Var p)) => (Type p -> Doc) -> Type p -> [TyBinder p] -> Doc
prettyQuantifiers prettyType t [] = prettyType t
prettyQuantifiers prettyType inner (q:qs) =
  let (these, those) = span (sameAs q) qs
      these, those :: [TyBinder p]
   in case q:these of
     Invisible _ _ Spec:_ ->
       keyword "forall"
       <+> hsep (map (stypeVar . (char '\'' <>) . pretty . (^?! tyBinderVar)) (q:these))
       <> dot <+> prettyQuantifiers prettyType inner those
     Invisible _ _ Infer:_ -> prettyQuantifiers prettyType inner those
     Invisible _ _ Req:_ ->
       let b (Invisible v k _) =
             case k of
               Nothing -> stypeVar (char '\'' <> pretty v)
               Just k -> parens $ stypeVar (char '\'' <> pretty v) <+> colon <+> prettyType k
           b _ = undefined
        in keyword "forall"
       <+> hsep (map b (q:these))
       <+> arrow <+> prettyQuantifiers prettyType inner those
     Anon{}:_ ->
       let arg x = parenTuple x (prettyType x)
        in hsep (punctuate (space <> arrow) (map (arg . (^?! _Anon)) (q:these)))
           <+> arrow <+> prettyQuantifiers prettyType inner those
     Implicit{}:_ ->
       let arg x = parenTuple x (prettyType x)
           arrow = soperator (string "=>")
           star = soperator (char '*')
        in hsep (punctuate (space <> star) (map (arg . (^?! _Implicit)) (q:these)))
           <+> arrow <+> prettyQuantifiers prettyType inner those


sameAs :: TyBinder p -> TyBinder p -> Bool
sameAs (Invisible _ _ x) (Invisible _ _ y) = x == y
sameAs Anon{} Anon{} = True
sameAs Implicit{} Implicit{} = True
sameAs _ _ = False

parenTyFun, parenTyArg, parenTuple :: Type p -> Doc -> Doc
parenTyArg TyApp{} = parens
parenTyArg x = parenTyFun x

parenTyFun TyPi{} = parens
parenTyFun TyTuple{} = parens
parenTyFun _ = id

parenTuple TyPi{} = parens
parenTuple _ = id

isOpName :: Pretty (Var p) => Type p -> Bool
isOpName (TyCon v _) = not . isAlphaNum . T.head . display . renderCompact . pretty $ v
isOpName _ = False

listType :: Var p ~ Var Resolved => Type p -> Maybe [Type p]
listType (TyPromotedCon v _)
  | v == nILName = pure []
  | otherwise = Nothing
listType (TyApp (TyPromotedCon v _) (TyTupleL hd tl))
  | v == cONSName = (hd:) <$> listType tl
  | otherwise = Nothing
listType _ = Nothing

undependentify :: Ord (Var p) => Type p -> Type p
undependentify (TyPi (Invisible v (Just k) Req) rest)
  | v `Set.notMember` ftv rest = TyArr k (undependentify rest)
  | otherwise = TyPi (Invisible v (Just k) Req) (undependentify rest)
undependentify t = t

prettyMotive :: SkolemMotive Typed -> Doc
prettyMotive (ByAscription _ t) = string "of the context, the type" <#> displayType t
prettyMotive (BySubsumption t1 t2) =
  string "of a requirement that" <+> displayType t1 <#> string "be as polymorphic as" <+> displayType t2
prettyMotive (ByExistential v t) =
  string "it is an existential" <> comma
  <#> string "bound by the type of" <+> pretty v <> comma <+> displayType t
prettyMotive (ByInstanceHead head _) = string "it is bound in an instance head, namely" <#> displayType head
prettyMotive (ByTyFunLhs head _) = string "it is bound by the LHS of a type function equation, namely" <#> displayType head
prettyMotive (ByConstraint con) =
  string "it is mentioned in a type class constraint, namely" <#> displayType con
