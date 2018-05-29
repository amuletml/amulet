{-# LANGUAGE ConstraintKinds, FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings #-}
module Syntax.Verify where

import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Foldable
import Data.Spanned
import Data.Reason
import Data.Triple
import Data.Graph

import Control.Monad.Writer

import Text.Pretty.Semantic
import Text.Pretty.Note

import Syntax.Pretty
import Syntax.Let

import Debug.Trace

type MonadVerify m = ( MonadWriter (Seq.Seq VerifyError) m )

data VerifyError
  = NonRecursiveRhs SomeReason (Expr Typed)

instance Spanned VerifyError where
  annotation (NonRecursiveRhs e _) = annotation e

instance Pretty VerifyError where
  pretty (NonRecursiveRhs re ex) = "Invalid recursive binding" <+> pretty ex
                                   <#> nest 4 ("Arising from use of" <+> blameOf re)

instance Note VerifyError Style where
  diagnosticKind _ = ErrorMessage

  formatNote f x = indent 2 (Right <$> pretty x) <#> f (traceShowId [annotation x])

runVerify :: Writer (Seq.Seq VerifyError) () -> Either (Seq.Seq VerifyError) ()
runVerify = fixup . runWriter where
  fixup ((), w)
    | Seq.null w = Right ()
    | otherwise = Left w

verifyProgram :: MonadVerify m => [Toplevel Typed] -> m ()
verifyProgram = traverse_ verifyStmt where
  verifyStmt st@(LetStmt vs) = verifyBindingGroup (BecauseOf (traceShow (annotation (traceShowId st)) st)) vs
  verifyStmt ForeignVal{} = pure ()
  verifyStmt TypeDecl{}   = pure ()
  verifyStmt (Module _ p) = verifyProgram p
  verifyStmt Open{}       = pure ()

verifyBindingGroup :: MonadVerify m => SomeReason -> [(Var Typed, Expr Typed, Ann Typed)] -> m ()
verifyBindingGroup blame = traverse_ verifyScc . depOrder where
  verifyScc (AcyclicSCC (_, e, _)) = verifyExpr e
  verifyScc (CyclicSCC vs) =
    let vars = Set.fromList (map fst3 vs)
     in undefined

verifyExpr :: MonadVerify m => Expr Typed -> m ()
verifyExpr VarRef{} = pure ()
verifyExpr ex@(Let vs e _) = do
  verifyBindingGroup (BecauseOf ex) vs
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
verifyExpr (Tuple es _) = traverse_ verifyExpr es
verifyExpr (TupleSection es _) = traverse_ (traverse_ verifyExpr) es
verifyExpr InstType{} = pure ()
verifyExpr InstHole{} = pure ()
verifyExpr (Lazy e _) = verifyExpr e
verifyExpr (OpenIn _ e _) = verifyExpr e
verifyExpr (ExprWrapper _ e _) = verifyExpr e

newtype Binding p = Binding (Var p, Expr p, Ann p)

instance Spanned (Ann p) => Spanned (Binding p) where
  annotation (Binding (_, _, x)) = annotation x

instance Pretty (Var p) => Pretty (Binding p) where
  pretty (Binding v) = pretty (LetStmt [v])

instance (Spanned (Ann p), Pretty (Var p)) => Reasonable Binding p where
  blame _ = string "the" <+> highlight "binding"
