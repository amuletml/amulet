{-# LANGUAGE ConstraintKinds, FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Syntax.Verify where

import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Foldable
import Data.Spanned
import Data.Reason

import Control.Monad.Writer

import Text.Pretty.Semantic

import Syntax.Pretty
import Syntax.Let

type MonadVerify m = ( MonadWriter (Seq.Seq VerifyError) m )

data VerifyError
  = NonRecursiveRhs SomeReason (Expr Typed)

runVerify :: Writer (Seq.Seq VerifyError) () -> Either (Seq.Seq VerifyError) ()
runVerify = fixup . runWriter where
  fixup ((), w)
    | Seq.null w = Right ()
    | otherwise = Left w

verifyProgram :: MonadVerify m => [Toplevel Typed] -> m ()
verifyProgram = traverse_ verifyStmt where 
  verifyStmt (LetStmt vs) = verifyBindingGroup vs
  verifyStmt ForeignVal{} = pure ()
  verifyStmt TypeDecl{}   = pure ()
  verifyStmt (Module _ p) = verifyProgram p
  verifyStmt Open{}       = pure ()

verifyBindingGroup :: MonadVerify m => [( Var Typed, Expr Typed, Ann Typed )] -> m ()
verifyBindingGroup = traverse_ verifyBinding where
  verifyBinding b@(v, e, _) = do
    when (v `Set.member` freeIn e) $ isProperlyRecursive (BecauseOf (Binding b)) e
    verifyExpr e

isProperlyRecursive :: MonadVerify m => SomeReason -> Expr Typed -> m ()
isProperlyRecursive _ Fun{} = pure ()
isProperlyRecursive r e = tell (Seq.singleton (NonRecursiveRhs r e))

verifyExpr :: MonadVerify m => Expr Typed -> m ()
verifyExpr VarRef{} = pure ()
verifyExpr (Let vs e _) = do
  verifyBindingGroup vs
  verifyExpr e
verifyExpr (If c t e _) = traverse_ verifyExpr [c, t, e]
verifyExpr (App f x _) = verifyExpr f *> verifyExpr x
verifyExpr (Fun _ x _) = verifyExpr x
verifyExpr (Begin es _) = traverse_ verifyExpr es
verifyExpr Literal{} = pure ()
verifyExpr (Match e bs _) = do
  verifyExpr e
  traverse_ (verifyExpr . snd) bs
verifyExpr (Function bs _) = traverse_ (verifyExpr . snd) bs
verifyExpr (BinOp l o r _) = traverse_ verifyExpr [l, o, r]
verifyExpr Hole{} = pure ()
verifyExpr (Ascription e _ _) = verifyExpr e
verifyExpr (Record rs _) = traverse_ (verifyExpr . snd) rs
verifyExpr (RecordExt e rs _) = verifyExpr e *> traverse_ (verifyExpr . snd) rs
verifyExpr (Access e _ _) = verifyExpr e
verifyExpr (LeftSection l o _) = traverse_ verifyExpr [l, o]
verifyExpr (RightSection o r _) = traverse_ verifyExpr [o, r]
verifyExpr (BothSection e _) = verifyExpr e
verifyExpr AccessSection{} = pure ()
verifyExpr (Parens e _) = verifyExpr e
verifyExpr (Tuple es _) = traverse_ (verifyExpr) es
verifyExpr (TupleSection es _) = traverse_ (traverse_ verifyExpr) es
verifyExpr InstType{} = pure ()
verifyExpr InstHole{} = pure ()
verifyExpr (OpenIn _ e _) = verifyExpr e
verifyExpr (ExprWrapper _ e _) = verifyExpr e

newtype Binding p = Binding (Var p, Expr p, Ann p)

instance Spanned (Ann p) => Spanned (Binding p) where
  annotation (Binding (_, _, x)) = annotation x

instance Pretty (Var p) => Pretty (Binding p) where
  pretty (Binding v) = pretty (LetStmt [v])

instance (Spanned (Ann p), Pretty (Var p)) => Reasonable Binding p where
  blame _ = string "the" <+> highlight "binding"
