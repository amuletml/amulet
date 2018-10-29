{-# LANGUAGE GADTs, ConstraintKinds, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances, ScopedTypeVariables #-}

-- | Represents a context associated with an error, such as what
-- expression the error occurred in.
module Data.Reason
  ( SomeReason(..), ConcreteReason(..)
  , Reasonable(..)
  , blameOf
  , becauseExp, becausePat
  , Respannable(..)
  ) where

import Data.Functor.Const
import Data.Spanned
import Data.Span
import Data.Data

import Syntax.Pretty

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
  annotation (BecauseOf a) = annotation a
  annotation (It'sThis a) = annotation a

instance Show SomeReason where
  show (BecauseOf _) = "reason"
  show (It'sThis x) = show x

instance Eq SomeReason where
  _ == _ = False

data ConcreteReason where
  BecauseOfExpr :: forall p. (Pretty (Var p), Respannable p) => Expr p -> String -> ConcreteReason
  BecauseOfPat :: forall p. (Pretty (Var p), Spanned (Ann p), Data (Var p), Data (Ann p), Data p) => Pattern p -> ConcreteReason

instance Show ConcreteReason where
  show (BecauseOfExpr _ _) = "expression blame"
  show (BecauseOfPat _) = "pattern blame"

instance Spanned ConcreteReason where
  annotation (BecauseOfExpr e _) = annotation e
  annotation (BecauseOfPat e) = annotation e

instance Pretty ConcreteReason where
  pretty (BecauseOfExpr e _) = pretty e
  pretty (BecauseOfPat e) = pretty e

-- | A type which can be blamed for an error
class (Spanned (f p), Pretty (f p)) => Reasonable f p where
  -- | Convert this blameable value into a pretty-printed document.
  blame :: f p -> Doc
  blame _ = empty

instance (Spanned (Pattern p), Pretty (Var p)) => Reasonable Pattern p where
  blame _ = string "the" <+> highlight "pattern"

instance (Spanned (Expr p), Pretty (Var p)) => Reasonable Expr p where
  blame _ = string "the" <+> highlight "expression"

instance (Data p, Data (Ann p), Data (Var p), Pretty (Var p)) => Reasonable Constructor p where
  blame _ = string "the" <+> highlight "constructor"

instance (Spanned (Ann p), Data p, Data (Ann p), Data (Var p), Pretty (Var p)) => Reasonable Toplevel p where
  blame _ = string "the" <+> highlight "declaration"

instance (Spanned (Ann p), Pretty (Var p)) => Reasonable Binding p where
  blame _ = string "the" <+> highlight "binding"

instance (Pretty (Var p), Reasonable Pattern p, Reasonable Expr p) => Reasonable Arm p where
  blame _ = string "the pattern-matching clause"

instance (Spanned (Ann p), Pretty (Var p)) => Reasonable ClassItem p where
  blame MethodSig{} = string "the" <+> highlight "method signature"
  blame DefaultMethod{} = string "the" <+> highlight "default method"

instance Reasonable (Const SomeReason) p where
  blame = blameOf . getConst

instance Spanned (Const SomeReason a) where
  annotation = annotation . getConst

instance Pretty (Const SomeReason a) where
  pretty = pretty . getConst

-- | Convert a reason into a pretty-printed document
blameOf :: SomeReason -> Doc
blameOf (BecauseOf (x :: f p)) = blame x
blameOf (It'sThis x) = case x of
  BecauseOfExpr _ s -> string "the" <+> highlight s
  BecauseOfPat e -> blame e

becauseExp :: (Pretty (Var p), Respannable p) => Expr p -> SomeReason
becauseExp = It'sThis . flip BecauseOfExpr "expression"

becausePat :: forall p. (Pretty (Var p), Spanned (Ann p), Data (Var p), Data (Ann p), Data p) => Pattern p -> SomeReason
becausePat = It'sThis . BecauseOfPat

class Spanned (Ann p) => Respannable p where
  respan :: (Span -> Span) -> Expr p -> Expr p

instance Respannable Parsed where
  respan k (VarRef v a) = VarRef v (k a)
  respan k (Let vs r a) = Let vs r (k a)
  respan k (If c t f a) = If c t f (k a)
  respan k (App f x a) = App f x (k a)
  respan k (Fun p b a) = Fun p b (k a)
  respan k (Begin es a) = Begin es (k a)
  respan k (Literal l a) = Literal l (k a)
  respan k (Match e bs a) = Match e bs (k a)
  respan k (Function bs a) = Function bs (k a)
  respan k (BinOp l o r a) = BinOp l o r (k a)
  respan k (Hole v a) = Hole v (k a)
  respan k (Ascription e t a) = Ascription e t (k a)

  respan k (Record fs a) = Record fs (k a)
  respan k (RecordExt f fs a) = RecordExt f fs (k a)
  respan k (Access e f a) = Access e f (k a)

  respan k (LeftSection o r a) = LeftSection o r (k a)
  respan k (RightSection l o a) = RightSection l o (k a)
  respan k (BothSection o a) = BothSection o (k a)
  respan k (AccessSection t a) = AccessSection t (k a)
  respan k (Parens e a) = Parens e (k a)

  respan k (Tuple es a) = Tuple es (k a)
  respan k (TupleSection es a) = TupleSection es (k a)

  respan k (OpenIn n e a) = OpenIn n e (k a)

  respan k (Lazy e a) = Lazy e (k a)

  respan k (ExprWrapper w e a) = ExprWrapper w e (k a)

instance Respannable Resolved where
  respan k (VarRef v a) = VarRef v (k a)
  respan k (Let vs r a) = Let vs r (k a)
  respan k (If c t f a) = If c t f (k a)
  respan k (App f x a) = App f x (k a)
  respan k (Fun p b a) = Fun p b (k a)
  respan k (Begin es a) = Begin es (k a)
  respan k (Literal l a) = Literal l (k a)
  respan k (Match e bs a) = Match e bs (k a)
  respan k (Function bs a) = Function bs (k a)
  respan k (BinOp l o r a) = BinOp l o r (k a)
  respan k (Hole v a) = Hole v (k a)
  respan k (Ascription e t a) = Ascription e t (k a)

  respan k (Record fs a) = Record fs (k a)
  respan k (RecordExt f fs a) = RecordExt f fs (k a)
  respan k (Access e f a) = Access e f (k a)

  respan k (LeftSection o r a) = LeftSection o r (k a)
  respan k (RightSection l o a) = RightSection l o (k a)
  respan k (BothSection o a) = BothSection o (k a)
  respan k (AccessSection t a) = AccessSection t (k a)
  respan k (Parens e a) = Parens e (k a)

  respan k (Tuple es a) = Tuple es (k a)
  respan k (TupleSection es a) = TupleSection es (k a)

  respan k (OpenIn n e a) = OpenIn n e (k a)

  respan k (Lazy e a) = Lazy e (k a)

  respan k (ExprWrapper w e a) = ExprWrapper w e (k a)

instance Respannable Typed where
  respan k (VarRef v a) = VarRef v (k (fst a), snd a)
  respan k (Let vs r a) = Let vs r (k (fst a), snd a)
  respan k (If c t f a) = If c t f (k (fst a), snd a)
  respan k (App f x a) = App f x (k (fst a), snd a)
  respan k (Fun p b a) = Fun p b (k (fst a), snd a)
  respan k (Begin es a) = Begin es (k (fst a), snd a)
  respan k (Literal l a) = Literal l (k (fst a), snd a)
  respan k (Match e bs a) = Match e bs (k (fst a), snd a)
  respan k (Function bs a) = Function bs (k (fst a), snd a)
  respan k (BinOp l o r a) = BinOp l o r (k (fst a), snd a)
  respan k (Hole v a) = Hole v (k (fst a), snd a)
  respan k (Ascription e t a) = Ascription e t (k (fst a), snd a)

  respan k (Record fs a) = Record fs (k (fst a), snd a)
  respan k (RecordExt f fs a) = RecordExt f fs (k (fst a), snd a)
  respan k (Access e f a) = Access e f (k (fst a), snd a)

  respan k (LeftSection o r a) = LeftSection o r (k (fst a), snd a)
  respan k (RightSection l o a) = RightSection l o (k (fst a), snd a)
  respan k (BothSection o a) = BothSection o (k (fst a), snd a)
  respan k (AccessSection t a) = AccessSection t (k (fst a), snd a)
  respan k (Parens e a) = Parens e (k (fst a), snd a)

  respan k (Tuple es a) = Tuple es (k (fst a), snd a)
  respan k (TupleSection es a) = TupleSection es (k (fst a), snd a)

  respan k (OpenIn n e a) = OpenIn n e (k (fst a), snd a)

  respan k (Lazy e a) = Lazy e (k (fst a), snd a)

  respan k (ExprWrapper w e a) = ExprWrapper w e (k (fst a), snd a)
