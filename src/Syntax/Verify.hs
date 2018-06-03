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

type MonadVerify m = ( MonadWriter (Seq.Seq VerifyError) m )

data VerifyError
  = NonRecursiveRhs { why :: SomeReason
                    , var :: Var Typed
                    , unguarded :: [Var Typed]
                    }

instance Spanned VerifyError where
  annotation (NonRecursiveRhs e _ _) = annotation e

instance Pretty VerifyError where
  pretty (NonRecursiveRhs re ex xs) =
    vsep [ "Invalid recursive right-hand side for variable" <+> skeyword (pretty ex)
         , if length xs == 0
              then empty
              else note <+> "because the variable" <> plural
                        <+> hsep (punctuate comma (map pretty xs)) <+> verb <+> "not under a function"
         , nest 4 ("Arising from use of" <+> blameOf re)
         ]
    where plural | length xs == 1 = empty | otherwise = char 's'
          verb | length xs == 1 = string "is" | otherwise = string "are"

instance Note VerifyError Style where
  diagnosticKind _ = ErrorMessage

  formatNote f x = indent 2 (Right <$> pretty x) <#> f [annotation x]

runVerify :: Writer (Seq.Seq VerifyError) () -> Either (Seq.Seq VerifyError) ()
runVerify = fixup . runWriter where
  fixup ((), w)
    | Seq.null w = Right ()
    | otherwise = Left w

verifyProgram :: MonadVerify m => [Toplevel Typed] -> m ()
verifyProgram = traverse_ verifyStmt where
  verifyStmt st@(LetStmt vs) = verifyBindingGroup (BecauseOf st) vs
  verifyStmt ForeignVal{} = pure ()
  verifyStmt TypeDecl{}   = pure ()
  verifyStmt (Module _ p) = verifyProgram p
  verifyStmt Open{}       = pure ()

verifyBindingGroup :: MonadVerify m => SomeReason -> [(Var Typed, Expr Typed, Ann Typed)] -> m ()
verifyBindingGroup _ = traverse_ verifyScc . depOrder where
  verifyScc (AcyclicSCC (_, e, _)) = verifyExpr e
  verifyScc (CyclicSCC vs) = do
    let vars = Set.fromList (map fst3 vs)
    forM_ vs $ \b@(var, ex, _) -> do
      let naked = unguardedVars ex
          blame = BecauseOf (Binding b)
      verifyExpr ex
      unless (naked `Set.disjoint` vars) $
        tell (Seq.singleton (NonRecursiveRhs blame var (Set.toList (naked `Set.intersection` vars))))

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

unguardedVars :: Expr Typed -> Set.Set (Var Typed)
unguardedVars (Ascription e _ _)   = unguardedVars e
unguardedVars (RecordExt e rs _)   = unguardedVars e <> foldMap (unguardedVars . snd) rs
unguardedVars (BinOp a b c _)      = unguardedVars a <> unguardedVars b <> unguardedVars c
unguardedVars (VarRef v _)         = Set.singleton v
unguardedVars (Begin es _)         = foldMap unguardedVars es
unguardedVars (Let vs b _)         = (unguardedVars b <> foldMap (unguardedVars . snd3) vs) Set.\\ Set.fromList (map fst3 vs)
unguardedVars (App f x _)          = unguardedVars f <> unguardedVars x
unguardedVars Fun{}                = mempty
unguardedVars (Record rs _)        = foldMap (unguardedVars . snd) rs
unguardedVars (Access e _ _)       = unguardedVars e
unguardedVars (Match t ps _)       = unguardedVars t <> foldMap unguardedVarsBranch ps where
  unguardedVarsBranch (p, e)       = unguardedVars e Set.\\ bound p
unguardedVars Literal{}            = mempty
unguardedVars Hole{}               = mempty
unguardedVars (If a b c _)         = unguardedVars a <> unguardedVars b <> unguardedVars c
unguardedVars (Tuple es _)         = foldMap unguardedVars es
unguardedVars (ExprWrapper _ e _)  = unguardedVars e -- wrappers only bind type arguments
unguardedVars (Parens e _)         = unguardedVars e
unguardedVars (LeftSection a b _)  = unguardedVars a <> unguardedVars b
unguardedVars (RightSection a b _) = unguardedVars a <> unguardedVars b
unguardedVars (BothSection b _)    = unguardedVars b
unguardedVars AccessSection{}      = mempty
unguardedVars InstHole{}           = mempty
unguardedVars InstType{}           = mempty
unguardedVars x = error (show x)


