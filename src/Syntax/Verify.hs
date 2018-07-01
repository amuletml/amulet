{-# LANGUAGE ConstraintKinds, FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings #-}
module Syntax.Verify where

import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Foldable
import Data.Spanned
import Data.Reason
import Data.Graph
import Data.Span

import Control.Monad.Writer.Strict
import Control.Monad.State.Strict
import Control.Lens hiding (Lazy)

import Text.Pretty.Semantic
import Text.Pretty.Note

import Syntax.Pretty
import Syntax.Let

type MonadVerify m = ( MonadWriter (Seq.Seq VerifyError) m, MonadState (Set.Set BindingSite) m )

data BindingSite
  = BindingSite { boundVar :: Var Typed
                , boundWhere :: Span
                , boundType :: Type Typed
                }
  deriving Show

data VerifyError
  = NonRecursiveRhs { why :: SomeReason
                    , var :: Var Typed
                    , unguarded :: [Var Typed]
                    }
  | DefinedUnused BindingSite

instance Spanned VerifyError where
  annotation (NonRecursiveRhs e _ _) = annotation e
  annotation (DefinedUnused b) = boundWhere b

instance Pretty VerifyError where
  pretty (NonRecursiveRhs re ex xs) =
    vsep [ "Invalid recursive right-hand side for variable" <+> skeyword (pretty ex)
         , if null xs
              then empty
              else note <+> "because the variable" <> plural
                        <+> hsep (punctuate comma (map pretty xs)) <+> verb <+> "not under a function"
         , nest 4 ("Arising from use of" <+> blameOf re)
         ]
    where plural | length xs == 1 = empty | otherwise = char 's'
          verb | length xs == 1 = string "is" | otherwise = string "are"
  pretty (DefinedUnused (BindingSite v _ _)) =
    string "Bound locally but not used:" <+> squotes (pretty v)

instance Note VerifyError Style where
  diagnosticKind NonRecursiveRhs{} = ErrorMessage
  diagnosticKind DefinedUnused{} = WarningMessage

  formatNote f x = indent 2 (Right <$> pretty x) <#> f [annotation x]

runVerify :: WriterT (Seq.Seq VerifyError) (State (Set.Set BindingSite)) () -> Either (Seq.Seq VerifyError) ()
runVerify = fixup . flip runState mempty . runWriterT where
  fixup (((), w), st) =
    let errs | Seq.null w = Right () | otherwise = Left w
        others = if Set.null st
                    then []
                    else map DefinedUnused (Set.elems st)
        probably [] = Right ()
        probably xs = Left (Seq.fromList xs)
     in case errs of
       Right () -> probably others
       Left es -> Left (es Seq.>< Seq.fromList others)

verifyProgram :: MonadVerify m => [Toplevel Typed] -> m ()
verifyProgram = traverse_ verifyStmt where
  verifyStmt st@(LetStmt vs) = verifyBindingGroup (flip const) (BecauseOf st) vs
  verifyStmt ForeignVal{} = pure ()
  verifyStmt TypeDecl{}   = pure ()
  verifyStmt (Module _ p) = verifyProgram p
  verifyStmt Open{}       = pure ()

verifyBindingGroup :: MonadVerify m
                   => (BindingSite -> Set.Set BindingSite -> Set.Set BindingSite)
                   -> SomeReason -> [Binding Typed] -> m ()
verifyBindingGroup k _ = traverse_ verifyScc . depOrder where
  verifyScc (AcyclicSCC (Binding v e _(s, t))) = do
    modify (k (BindingSite v s t))
    verifyExpr e
  verifyScc (CyclicSCC vs) = do
    let vars = foldMapOf (each . bindVariable) Set.singleton vs
    for_ vs $ \b@(Binding var ex _ (s, ty)) -> do
      let naked = unguardedVars ex
          blame = BecauseOf b
      verifyExpr ex
      modify (k (BindingSite var s ty))
      unless (naked `Set.disjoint` vars) $
        tell (Seq.singleton (NonRecursiveRhs blame var (Set.toList (naked `Set.intersection` vars))))

verifyExpr :: MonadVerify m => Expr Typed -> m ()
verifyExpr (VarRef v (s, t)) = do
  modify $ Set.delete (BindingSite v s t)
  pure ()
verifyExpr ex@(Let vs e _) = do
  verifyBindingGroup Set.insert (BecauseOf ex) vs
  verifyExpr e
verifyExpr (If c t e _) = traverse_ verifyExpr [c, t, e]
verifyExpr (App f x _) = verifyExpr f *> verifyExpr x
verifyExpr (Fun p x _) = do
  modify (Set.union (bindingSites p))
  verifyExpr x
verifyExpr (Begin es _) = traverse_ verifyExpr es
verifyExpr Literal{} = pure ()
verifyExpr (Match e bs _) = do
  verifyExpr e
  for_ bs $ \(pat, body) -> do
    modify (Set.union (bindingSites pat))
    verifyExpr body
verifyExpr (Function bs _) =
  for_ bs $ \(pat, body) -> do
    modify (Set.union (bindingSites pat))
    verifyExpr body
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

unguardedVars :: Expr Typed -> Set.Set (Var Typed)
unguardedVars (Ascription e _ _)   = unguardedVars e
unguardedVars (RecordExt e rs _)   = unguardedVars e <> foldMap (unguardedVars . snd) rs
unguardedVars (BinOp a b c _)      = unguardedVars a <> unguardedVars b <> unguardedVars c
unguardedVars (VarRef v _)         = Set.singleton v
unguardedVars (Begin es _)         = foldMap unguardedVars es
unguardedVars (Let vs b _)         = (unguardedVars b <> foldMap (unguardedVars . view bindBody) vs)
                              Set.\\ foldMapOf (each . bindVariable) Set.singleton vs
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

bindingSites :: Pattern Typed -> Set.Set BindingSite
bindingSites (Capture v (s, t)) = Set.singleton (BindingSite v s t)
bindingSites Wildcard{} = mempty
bindingSites PLiteral{} = mempty
bindingSites (Destructure _ p _) = maybe mempty bindingSites p
bindingSites (PType p _ _) = bindingSites p
bindingSites (PRecord rs _) = foldMap (bindingSites . snd) rs
bindingSites (PTuple ps _) = foldMap bindingSites ps
bindingSites (PWrapper _ p _) = bindingSites p


instance Ord BindingSite where
  BindingSite v _ _ `compare` BindingSite v' _ _ = v `compare` v'

instance Eq BindingSite where
  BindingSite v _ _ == BindingSite v' _ _ = v == v'
