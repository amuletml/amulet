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
import Syntax.Subst
import Syntax.Let

import Language.Lua.Parser

import Types.Infer.Builtin (tyUnit, tyLazy, spine, getHead)

type MonadVerify m =
  ( MonadWriter (Seq.Seq VerifyError) m
  , MonadState (Set.Set BindingSite) m )

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
  | NonParametricForeign { stmt :: Toplevel Typed
                         , typeOf :: Type Typed
                         , var :: Var Typed }
  | NonUnitBegin (Expr Typed) (Type Typed)
  | LazyLet (Expr Typed) (Type Typed)

instance Spanned VerifyError where
  annotation (NonRecursiveRhs e _ _) = annotation e
  annotation (DefinedUnused b) = boundWhere b
  annotation (ParseErrorInForeign _ e) = annotation e
  annotation (NonParametricForeign s _ _) = annotation s
  annotation (NonUnitBegin e _) = annotation e
  annotation (LazyLet e _) = annotation e

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
         , bullet "Note: use a" <+> keyword "let" <+> "to silence this warning, as in"
         , indent 2 $
             keyword "let" <+> soperator (char '_') <+> equals <+> pretty ex
         ]
  pretty (LazyLet _ _) =
    vsep [ "Automatic thunking of" <+> keyword "let" <> "s does not cover bindings"
         ]
  pretty (NonParametricForeign _ ty var) =
    vsep [ "Foreign value has implied non-parametric type"
         , bullet "Note: the compiler could assume all functions returning otherwise"
         , indent 8 "unused type variables are non-terminating for optimisation"
         , empty
         , bullet "Note: in this type, no terms of type" <+> stypeVar (pretty var) <+> "are inputs"
         , indent 6 $ displayType ty
         , indent 2 "and so no value of that type could be returned."
         ]

instance Note VerifyError Style where
  diagnosticKind NonRecursiveRhs{} = ErrorMessage
  diagnosticKind ParseErrorInForeign{} = WarningMessage
  diagnosticKind NonParametricForeign{} = WarningMessage
  diagnosticKind DefinedUnused{} = WarningMessage
  diagnosticKind NonUnitBegin{} = WarningMessage
  diagnosticKind LazyLet{} = WarningMessage

  formatNote f (ParseErrorInForeign (ForeignVal var s _ (span, _)) err) =
    let SourcePos name _ _ = spanStart (annotation err)
        spans = [( name, s )]
     in vsep [ indent 2 "Syntax error in definition of" <+> (Right <$> skeyword (pretty var))
             , f [span]
             , empty
             , format (fileSpans spans highlightLua) err
             ]

  formatNote f (LazyLet (Let bs ex _) _) =
    vsep
      [ indent 2 "Automatic thunking of" <+> (Right <$> keyword "let") <> "s does not cover bindings"
      , empty
      , indent 2 $ bullet "Note: the expression"
      , f [annotation ex]
      , indent 2 "will be evaluated lazily, but" <+> (if length bs == 1 then "this" else "these")
          <+> "binding" <> if length bs == 1 then "" else "s"
      , f (fmap annotation bs)
      , indent 2 "are" <+> (Right <$> highlight "strict.")
      , indent 2 $ bullet "Note: if this is what you want, use" <+> (Right <$> keyword "lazy") <+> "explicitly"
      , indent 6 "to silence this warning."
      ]
  formatNote _ LazyLet{} = error "impossible"
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
  verifyStmt Class{} = pure ()
  verifyStmt st@(ForeignVal v d t (_, _)) = do
    case parseExpr (SourcePos ("definition of " ++ displayS (pretty v)) 1 1) (d ^. lazy) of
      Left e -> tell (Seq.singleton (ParseErrorInForeign st e))
      Right _ -> pure ()
    parametricity st t

  verifyStmt TypeDecl{}   = pure ()
  verifyStmt (Module _ p) = verifyProgram p
  verifyStmt Open{}       = pure ()

verifyBindingGroup :: MonadVerify m
                   => (BindingSite -> Set.Set BindingSite -> Set.Set BindingSite)
                   -> SomeReason -> [Binding Typed] -> m ()
verifyBindingGroup k _ = traverse_ verifyScc . depOrder where
  verifyScc (AcyclicSCC (Binding v e (s, t))) = do
    modify (k (BindingSite v s t))
    verifyExpr e
  verifyScc (AcyclicSCC (TypedMatching p e _ _)) = do
    traverse_ (modify . k) $ bindingSites p
    verifyExpr e

  verifyScc (AcyclicSCC Matching{}) = error "Matching after TC"

  verifyScc (CyclicSCC vs) = do
    let vars = foldMapOf (each . bindVariable) Set.singleton vs
    for_ vs $ \(Binding var _ (s, ty)) -> modify (k (BindingSite var s ty))
    for_ vs $ \b@(Binding var ex (_, _)) -> do
      let naked = unguardedVars ex
          blame = BecauseOf b
      verifyExpr ex
      unless (naked `Set.disjoint` vars) $
        tell (Seq.singleton (NonRecursiveRhs blame var (Set.toList (naked `Set.intersection` vars))))

verifyExpr :: MonadVerify m => Expr Typed -> m ()
verifyExpr (VarRef v (s, t)) = do
  modify $ Set.delete (BindingSite v s t)
  pure ()
verifyExpr ex@(Let vs e (_, ty)) = do
  when (isLazy ty && isWrappedThunk e && any nonTrivialRhs vs) $
    tell (Seq.singleton (LazyLet ex ty))
  verifyBindingGroup Set.insert (BecauseOf ex) vs
  verifyExpr e
verifyExpr (If c t e _) = traverse_ verifyExpr [c, t, e]
verifyExpr (App f x _) = verifyExpr f *> verifyExpr x
verifyExpr (Fun p x _) = do
  let bindingSites' (PatParam p) = bindingSites p
      bindingSites' _ = mempty
  modify (Set.union (bindingSites' p))
  verifyExpr x
verifyExpr (Begin es _) = do
  let unitish TyVar{} = True
      unitish TyWildcard{} = True
      unitish x = x == tyUnit
  for_ (init es) $ \ex -> do
    let ty = getType ex
    verifyExpr ex
    unless (unitish ty) $
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

isLazy :: Type Typed -> Bool
isLazy ty = tyLazy == head (spine (getHead ty))

isWrappedThunk :: Expr Typed -> Bool
isWrappedThunk (ExprWrapper (WrapFn (MkWrapCont _ x)) _ _) =
  x == "automatic thunking"
isWrappedThunk _ = False

nonTrivialRhs :: Binding Typed -> Bool
nonTrivialRhs (TypedMatching _ e _ _) = nonTrivial e
nonTrivialRhs (Binding _ e _) = nonTrivial e
nonTrivialRhs _ = error "nonTrivialRHS pre-TC Bindings"

nonTrivial :: Expr Typed -> Bool
nonTrivial App{} = True
nonTrivial BinOp{} = True
nonTrivial (If c t e _) = nonTrivial c || nonTrivial t || nonTrivial e
nonTrivial (Let vs e _) = nonTrivial e || any nonTrivialRhs vs
nonTrivial (Begin es _) = any nonTrivial es
nonTrivial (Match e cs _) = nonTrivial e || any (nonTrivial . snd) cs
nonTrivial VarRef{} = False
nonTrivial Fun{} = False
nonTrivial Literal{} = False
nonTrivial Function{} = False
nonTrivial Hole{} = False
nonTrivial (Ascription e _ _) = nonTrivial e
nonTrivial (Record rs _) = any (nonTrivial . view fExpr) rs
nonTrivial (RecordExt e rs _) = nonTrivial e || any (nonTrivial . view fExpr) rs
nonTrivial (Access e _ _) = nonTrivial e
nonTrivial LeftSection{} = False
nonTrivial AccessSection{} = False
nonTrivial RightSection{} = False
nonTrivial BothSection{} = False
nonTrivial (Parens e _) = nonTrivial e
nonTrivial (Tuple es _) = any nonTrivial es
nonTrivial TupleSection{} = False
nonTrivial (OpenIn _ e _) = nonTrivial e
nonTrivial Lazy{} = False
nonTrivial (ExprWrapper w e _) =
  case w of
    WrapFn (MkWrapCont k _) -> nonTrivial (k e)
    _ -> nonTrivial e

parametricity :: forall m. MonadVerify m => Toplevel Typed -> Type Typed -> m ()
parametricity stmt overall = go mempty overall where
  go :: Set.Set (Var Typed) -> Type Typed -> m ()
  go set (TyVar v)
    | v `Set.member` set = tell (pure (NonParametricForeign stmt overall v))
    | otherwise = pure ()

  go set (TyPi binder cont) =
    case binder of
      Invisible v kind -> do
        set <- case kind of
          Just kind -> goArg set kind
          _ -> pure set
        go (Set.insert v set) cont
      Anon v -> do
        set <- goArg set v
        go set cont
      Implicit v -> do
        set <- goArg set v
        go set cont

  go _ _ = pure ()

  goArg set (TyPi _ cont) = goArg set cont
  goArg set t = pure (set `Set.difference` ftv t)

