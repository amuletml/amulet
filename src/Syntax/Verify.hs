{-# LANGUAGE ConstraintKinds, FlexibleContexts, FlexibleInstances,
   UndecidableInstances, MultiParamTypeClasses, OverloadedStrings,
   ScopedTypeVariables #-}
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

import Language.Lua.Parser.Wrapper
import Language.Lua.Parser.Parser
import Language.Lua.Parser.Error

import Types.Infer.Builtin (tyUnit)

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
  | ParseErrorInForeign { stmt :: Toplevel Typed
                        , err :: ParseError }
  | NonUnitBegin (Expr Typed) (Type Typed)

instance Spanned VerifyError where
  annotation (NonRecursiveRhs e _ _) = annotation e
  annotation (DefinedUnused b) = boundWhere b
  annotation (ParseErrorInForeign _ e) = annotation e
  annotation (NonUnitBegin e _) = annotation e

instance Pretty VerifyError where
  pretty (NonRecursiveRhs re ex xs) =
    vsep [ "Invalid recursive right-hand side for variable" <+> skeyword (pretty ex)
         , if null xs
              then empty
              else note <+> "because evaluation of the variable" <> plural
                        <+> hsep (punctuate comma (map pretty xs)) <+> "is not delayed"
         , nest 4 ("Arising from use of" <+> blameOf re)
         ]
    where plural | length xs == 1 = empty | otherwise = char 's'
  pretty (DefinedUnused (BindingSite v _ _)) =
    string "Bound locally but not used:" <+> squotes (pretty v)
  pretty (ParseErrorInForeign var err) =
    vsep [ "Invalid syntax in definition of foreign value" <+> pretty var
         , pretty err ]
  pretty (NonUnitBegin ex ty) =
    vsep [ "This statement discards a value of type"
         , indent 2 (displayType ty)
         , empty
         , "Note: use a" <+> keyword "let" <+> "to silence this warning, as in"
         , indent 2 $
             keyword "let" <+> soperator (char '_') <+> equals <+> pretty ex
         ]

instance Note VerifyError Style where
  diagnosticKind NonRecursiveRhs{} = ErrorMessage
  diagnosticKind ParseErrorInForeign{} = WarningMessage
  diagnosticKind DefinedUnused{} = WarningMessage
  diagnosticKind NonUnitBegin{} = WarningMessage

  formatNote f (ParseErrorInForeign (ForeignVal var s _ (span, _)) err) =
    let SourcePos name _ _ = spanStart (annotation err)
        spans = [( name, s )]
     in vsep [ indent 2 "Syntax error in definition of" <+> (Right <$> skeyword (pretty var))
             , f [span]
             , empty
             , format (fileSpans spans) err
             ]
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

verifyProgram :: forall m. MonadVerify m => [Toplevel Typed] -> m ()
verifyProgram = traverse_ verifyStmt where
  verifyStmt :: Toplevel Typed -> m ()
  verifyStmt st@(LetStmt vs) = verifyBindingGroup (flip const) (BecauseOf st) vs
  verifyStmt st@(ForeignVal v d _ (_, _)) =
    case runParser (SourcePos ("definition of " ++ displayS (pretty v)) 1 1) (d ^. lazy) parseExpr of
      Left e -> tell (Seq.singleton (ParseErrorInForeign st e))
      Right _ -> pure ()

  verifyStmt TypeDecl{}   = pure ()
  verifyStmt (Module _ p) = verifyProgram p
  verifyStmt Open{}       = pure ()

verifyBindingGroup :: MonadVerify m
                   => (BindingSite -> Set.Set BindingSite -> Set.Set BindingSite)
                   -> SomeReason -> [Binding Typed] -> m ()
verifyBindingGroup k _ = traverse_ verifyScc . depOrder where
  verifyScc (AcyclicSCC (Binding v e _ (s, t))) = do
    modify (k (BindingSite v s t))
    verifyExpr e
  verifyScc (AcyclicSCC (TypedMatching p e _ _)) = do
    traverse_ (modify . k) $ bindingSites p
    verifyExpr e

  verifyScc (AcyclicSCC ParsedBinding{}) = error "ParsedBinding in *verify*"
  verifyScc (AcyclicSCC Matching{}) = error "Matching after TC"

  verifyScc (CyclicSCC vs) = do
    let vars = foldMapOf (each . bindVariable) Set.singleton vs
    for_ vs $ \(Binding var _ _ (s, ty)) -> modify (k (BindingSite var s ty))
    for_ vs $ \b@(Binding var ex _ (_, _)) -> do
      let naked = unguardedVars ex
          blame = BecauseOf b
      verifyExpr ex
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
  let bindingSites' (PatParam p) = bindingSites p
      bindingSites' (ImplParam p) = bindingSites p
  modify (Set.union (bindingSites' p))
  verifyExpr x
verifyExpr (Begin es _) = do
  for_ (init es) $ \ex -> do
    let ty = getType ex
    verifyExpr ex
    when (ty /= tyUnit) $
      tell (Seq.singleton (NonUnitBegin ex ty))
  verifyExpr (last es)
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
verifyExpr (Record rs _) = traverse_ (verifyExpr . view fExpr) rs
verifyExpr (RecordExt e rs _) = verifyExpr e *> traverse_ (verifyExpr . view fExpr) rs
verifyExpr (Access e _ _) = verifyExpr e
verifyExpr (LeftSection l o _) = traverse_ verifyExpr [l, o]
verifyExpr (RightSection o r _) = traverse_ verifyExpr [o, r]
verifyExpr (BothSection e _) = verifyExpr e
verifyExpr AccessSection{} = pure ()
verifyExpr (Parens e _) = verifyExpr e
verifyExpr (Tuple es _) = traverse_ verifyExpr es
verifyExpr (TupleSection es _) = traverse_ (traverse_ verifyExpr) es
verifyExpr (Lazy e _) = verifyExpr e
verifyExpr (OpenIn _ e _) = verifyExpr e
verifyExpr (ExprWrapper w e _) =
  case w of
    WrapFn (MkWrapCont k _) -> verifyExpr (k e)
    ExprApp a -> do
      verifyExpr a
      verifyExpr e
    _ -> verifyExpr e

unguardedVars :: Expr Typed -> Set.Set (Var Typed)
unguardedVars (Ascription e _ _)   = unguardedVars e
unguardedVars (RecordExt e rs _)   = unguardedVars e <> foldMap (unguardedVars . view fExpr) rs
unguardedVars (BinOp a b c _)      = unguardedVars a <> unguardedVars b <> unguardedVars c
unguardedVars (VarRef v _)         = Set.singleton v
unguardedVars (Begin es _)         = foldMap unguardedVars es
unguardedVars (Let vs b _)         = (unguardedVars b <> foldMap (unguardedVars . view bindBody) vs)
                              Set.\\ foldMapOf (each . bindVariable) Set.singleton vs
unguardedVars (App f x _) =
  case f of
    ExprWrapper _ (VarRef (TvName (TgInternal "force")) _) _ -> freeIn f <> freeIn x
    _ -> freeIn f <> unguardedVars x
unguardedVars Fun{}                = mempty
unguardedVars (Record rs _)        = foldMap (unguardedVars . view fExpr) rs
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
unguardedVars x = error (show x)

bindingSites :: Pattern Typed -> Set.Set BindingSite
bindingSites (Capture v (s, t)) = Set.singleton (BindingSite v s t)
bindingSites (PAs p v (s, t)) = Set.insert (BindingSite v s t) (bindingSites p)
bindingSites Wildcard{} = mempty
bindingSites PLiteral{} = mempty
bindingSites (Destructure _ p _) = foldMap bindingSites p
bindingSites (PType p _ _) = bindingSites p
bindingSites (PRecord rs _) = foldMap (bindingSites . snd) rs
bindingSites (PTuple ps _) = foldMap bindingSites ps
bindingSites (PWrapper _ p _) = bindingSites p
bindingSites (PSkolem p _ _) = bindingSites p

instance Ord BindingSite where
  BindingSite v _ _ `compare` BindingSite v' _ _ = v `compare` v'

instance Eq BindingSite where
  BindingSite v _ _ == BindingSite v' _ _ = v == v'
