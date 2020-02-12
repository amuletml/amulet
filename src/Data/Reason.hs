{-# LANGUAGE GADTs
  , ConstraintKinds
  , MultiParamTypeClasses
  , FlexibleInstances
  , FlexibleContexts
  , UndecidableInstances
  , ScopedTypeVariables #-}

-- | Represents a context associated with an error, such as what
-- expression the error occurred in.
module Data.Reason
  ( SomeReason(..), ConcreteReason(..)
  , Reasonable(..)
  , blameOf
  , becauseExp, becausePat
  , Respannable(..)
  , InType(..)
  ) where

import Data.Functor.Const
import Data.Spanned
import Data.Span
import Data.Data

import Syntax

import Text.Pretty.Semantic

-- | The reason for some error message
data SomeReason where
  -- | Blame a specific 'Reasonable' value.
  BecauseOf :: (Reasonable a p) => a p -> SomeReason
  -- | Blame some concrete value
  It'sThis  :: ConcreteReason -> SomeReason

instance Pretty SomeReason where
  pretty (BecauseOf a) = pretty a
  pretty (It'sThis x) = pretty x

instance Spanned SomeReason where
  spanOf (BecauseOf a) = spanOf a
  spanOf (It'sThis a) = spanOf a

instance Show SomeReason where
  show (BecauseOf _) = "reason"
  show (It'sThis x) = show x

instance Eq SomeReason where
  _ == _ = False

data ConcreteReason where
  BecauseOfExpr :: forall p. (Pretty (Var p), Respannable (Ann p)) => Expr p -> String -> ConcreteReason
  BecauseOfPat :: forall p. (Pretty (Var p), Spanned (Ann p), Data (Var p), Data (Ann p), Data p)
               => Pattern p -> ConcreteReason
  BecauseInternal :: String -> ConcreteReason

instance Show ConcreteReason where
  show (BecauseOfExpr _ _) = "expression blame"
  show (BecauseOfPat _) = "pattern blame"
  show (BecauseInternal x) = "internal blame: " ++ x

instance Spanned ConcreteReason where
  spanOf (BecauseOfExpr e _) = spanOf e
  spanOf (BecauseOfPat e) = spanOf e
  spanOf _ = internal

instance Pretty ConcreteReason where
  pretty (BecauseOfExpr e _) = pretty e
  pretty (BecauseOfPat e) = pretty e
  pretty (BecauseInternal x) = keyword x

-- | A type which can be blamed for an error
class (Spanned (f p), Pretty (f p)) => Reasonable f p where
  -- | Convert this blameable value into a pretty-printed document.
  blame :: f p -> Doc
  blame _ = empty

instance (Spanned (Pattern p), Pretty (Var p)) => Reasonable Pattern p where
  blame _ = string "the" <+> highlight "pattern"

instance (Spanned (Expr p), Pretty (Var p)) => Reasonable Expr p where
  blame _ = string "the" <+> highlight "expression"

instance (Spanned (Ann p), Pretty (Var p)) => Reasonable Constructor p where
  blame _ = string "the" <+> highlight "constructor"

instance (Spanned (RawAnn p), Pretty (Var p)) => Reasonable Toplevel p where
  blame _ = string "the" <+> highlight "declaration"

instance (Spanned (RawAnn p), Pretty (Var p)) => Reasonable ModuleTerm p where
  blame _ = string "the" <+> highlight "module"

instance (Spanned (Ann p), Pretty (Var p)) => Reasonable Binding p where
  blame _ = string "the" <+> highlight "binding"

instance (Spanned (RawAnn p), Pretty (Var p)) => Reasonable Arm p where
  blame _ = string "the pattern-matching clause"

instance (Spanned (Ann p), Pretty (Var p)) => Reasonable CompStmt p where
  blame _ = string "the statement"

instance (Spanned (RawAnn p), Pretty (Var p)) => Reasonable ClassItem p where
  blame MethodSig{} = string "the" <+> highlight "method signature"
  blame DefaultMethod{} = string "the" <+> highlight "default method"
  blame AssocType{} = string "the" <+> highlight "associated type"

instance (Spanned (RawAnn p), Pretty (Var p)) => Reasonable Fundep p where
  blame _ = string "the functional dependency"

instance (Spanned (Ann p), Pretty (Var p)) => Reasonable TyFunClause p where
  blame _ = string "the type function clause"

instance (Spanned (Ann p), Pretty (Var p)) => Reasonable InstanceItem p where
  blame MethodImpl{} = string "the" <+> highlight "method implementation"
  blame TypeImpl{} = string "the" <+> highlight "associated type definition"

instance Reasonable (Const SomeReason) p where
  blame = blameOf . getConst

instance Spanned (Const SomeReason a) where
  spanOf = spanOf . getConst

instance Pretty (Const SomeReason a) where
  pretty = pretty . getConst

data InType p = InType (Type p) (TypeAnn p)

instance Spanned (TypeAnn p) => Spanned (InType p) where
  spanOf (InType _ s) = spanOf s

instance Pretty (Type p) => Pretty (InType p) where
  pretty (InType t _) = pretty t

instance (Spanned (TypeAnn p), Pretty (Type p)) => Reasonable InType p where
  blame _ = string "the" <+> highlight "type"

-- | Convert a reason into a pretty-printed document
blameOf :: SomeReason -> Doc
blameOf (BecauseOf (x :: f p)) = blame x
blameOf (It'sThis x) = case x of
  BecauseOfExpr _ s -> string "this" <+> highlight s
  BecauseOfPat e -> blame e
  BecauseInternal{} -> keyword "internal compiler error"

becauseExp :: (Pretty (Var p), Respannable (Ann p)) => Expr p -> SomeReason
becauseExp = It'sThis . flip BecauseOfExpr "expression"

becausePat :: forall p. (Pretty (Var p), Spanned (Ann p), Data (Var p), Data (Ann p), Data p)
           => Pattern p -> SomeReason
becausePat = It'sThis . BecauseOfPat

class Spanned a => Respannable a where
  respan :: (Span -> Span) -> a -> a

instance Respannable (Ann p) => Respannable (Expr p) where
  respan k (VarRef v a) = VarRef v (respan k a)
  respan k (Let re vs r a) = Let re vs r (respan k a)
  respan k (If c t f a) = If c t f (respan k a)
  respan k (App f x a) = App f x (respan k a)
  respan k (Fun p b a) = Fun p b (respan k a)
  respan k (Begin es a) = Begin es (respan k a)
  respan k (Literal l a) = Literal l (respan k a)
  respan k (Match e bs p a) = Match e bs p (respan k a)
  respan k (Function bs p a) = Function bs p (respan k a)
  respan k (BinOp l o r a) = BinOp l o r (respan k a)
  respan k (Hole v a) = Hole v (respan k a)
  respan k (Ascription e t a) = Ascription e t (respan k a)
  respan k (Vta e t a) = Vta e t (respan k a)
  respan k (ListExp e a) = ListExp e (respan k a)
  respan k (ListComp e qs a) = ListComp e qs (respan k a)
  respan k (DoExpr v qs a) = DoExpr v qs (respan k a)
  respan k (Idiom vp va es a) = Idiom vp va es (respan k a)
  respan k (ListFrom v x a) = ListFrom v x (respan k a)
  respan k (ListFromTo v x y a) = ListFromTo v x y (respan k a)
  respan k (ListFromThen v x y a) = ListFromThen v x y (respan k a)
  respan k (ListFromThenTo v x y z a) = ListFromThenTo v x y z (respan k a)

  respan k (Record fs a) = Record fs (respan k a)
  respan k (RecordExt f fs a) = RecordExt f fs (respan k a)
  respan k (Access e f a) = Access e f (respan k a)

  respan k (LeftSection o r a) = LeftSection o r (respan k a)
  respan k (RightSection l o a) = RightSection l o (respan k a)
  respan k (BothSection o a) = BothSection o (respan k a)
  respan k (AccessSection t a) = AccessSection t (respan k a)
  respan k (Parens e a) = Parens e (respan k a)

  respan k (Tuple es a) = Tuple es (respan k a)
  respan k (TupleSection es a) = TupleSection es (respan k a)

  respan k (OpenIn n e a) = OpenIn n e (respan k a)

  respan k (Lazy e a) = Lazy e (respan k a)

  respan k (ExprWrapper w e a) = ExprWrapper w e (respan k a)

instance Respannable Span where
  respan = id

instance Respannable a => Respannable (a, b) where
  respan f (a, b) = (respan f a, b)
