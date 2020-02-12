{-# LANGUAGE ConstraintKinds, FlexibleContexts, FlexibleInstances,
   UndecidableInstances, MultiParamTypeClasses, OverloadedStrings,
   ScopedTypeVariables, TupleSections, GADTs #-}
module Syntax.Verify
  ( VerifyError(..)
  , BindingSite(..)
  , runVerify
  , verifyProgram
  ) where

import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Set as Set
import Data.Foldable
import Data.Position
import Data.Spanned
import Data.Reason
import Data.Graph
import Data.Span

import Control.Monad.Writer.Strict
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Lens hiding (Lazy, (:>))

import Text.Pretty.Semantic
import Text.Pretty.Note

import Syntax.Verify.Pattern
import Syntax.Verify.Error

import Syntax.Builtin (tyLazy, forceName, tyRefName)
import Syntax.Transform
import Syntax.Types
import Syntax.Let
import Syntax

import qualified CompileTarget as CT

import Types.Infer.Builtin (getHead, expandTypeWith)

data VerifyScope = VerifyScope
  { env        :: Env
  , patternAbs :: AbsState
  , target     :: CT.Target
  }
type MonadVerify m =
  ( MonadWriter (Seq.Seq VerifyError) m
  , MonadReader VerifyScope m
  , MonadState (Set.Set BindingSite) m )

runVerify :: Env -> CT.Target
          -> Var Resolved
          -> WriterT (Seq.Seq VerifyError) (StateT (Set.Set BindingSite) (Reader VerifyScope)) ()
          -> (Bool, Seq.Seq VerifyError)
runVerify env target var = fixup
                  . flip runReader (VerifyScope env (emptyAbsState var) target)
                  . flip runStateT mempty
                  . runWriterT where
  fixup (((), w), st) =
    let errs = w <> Seq.fromList (map DefinedUnused (Set.elems st))
    in (all nonFatal errs, errs)

  nonFatal x = diagnosticKind x /= ErrorMessage

verifyProgram :: forall m. MonadVerify m => [Toplevel Typed] -> m ()
verifyProgram = traverse_ verifyStmt where
  verifyStmt :: Toplevel Typed -> m ()
  verifyStmt st@(LetStmt _ am vs _) = verifyBindingGroup addBind (BecauseOf st) vs where
    addBind :: BindingSite -> m ()
    addBind b@(BindingSite _ _ tau) = do
      when (am == Public) $
        case tau of
          TyApps (TyCon v _) [_] | v == tyRefName -> tell (Seq.singleton (ToplevelRefBinding b))
          _ -> pure ()
      case am of
        Private -> modify (Set.insert b)
        Public -> pure ()

  verifyStmt st@(ForeignVal _ v d _ _) = do
    target <- asks target
    case CT.parse target (SourcePos ("definition of " <> displayT (pretty v)) 1 1) (d ^. lazy) of
      Left e -> tell (Seq.singleton (ParseErrorInForeign st e target))
      Right _ -> pure ()

  verifyStmt Class{} = pure ()
  verifyStmt Instance{} = pure ()
  verifyStmt TySymDecl{} = pure ()

  verifyStmt TypeDecl{} = pure ()
  verifyStmt DeriveInstance{} = pure ()
  verifyStmt TypeFunDecl{} = pure ()

  -- Here we do the opposite of TC: Since we're working with the final
  -- 'Env' (where everything is fully qualified), when entering a module
  -- definition, remove the prefix.
  verifyStmt (Module _ nm m) = do
    let prefix = case nm of
          TgName n _ -> n <> T.singleton '.'
          TgInternal v -> v <> T.singleton '.'
        ext s = s { env = env s & names %~ mapScope id (unqualifyWrt prefix)
                                & types %~ fmap (Set.mapMonotonic (unqualifyVarWrt prefix)) }

    local ext $ verifyModule m

  verifyStmt (Open m) = verifyModule m
  verifyStmt (Include m) = verifyModule m

-- | Verify a recursive definition is well-formed
verifyBindingGroup :: MonadVerify m
                   => (BindingSite -> m ())
                   -> SomeReason -> [Binding Typed] -> m ()
verifyBindingGroup k _ = traverse_ verifyScc . depOrder where
  verifyScc (AcyclicSCC (Binding v e c (s, t))) =
    when c $ do
      k (BindingSite v s t)
      verifyExpr e

  verifyScc (AcyclicSCC (TypedMatching p e _ _)) = do
    traverse_ k (bindingSites p)
    verifyExpr e

  verifyScc (AcyclicSCC Matching{}) = error "Matching after TC"

  verifyScc (CyclicSCC vs) = do
    let vars = foldMapOf (each . bindVariable) Set.singleton vs
    for_ vs $ \(Binding var _ c (s, ty)) ->
      when c $ k (BindingSite var s ty)

    for_ vs $ \b@(Binding var ex c (_, _)) -> when c $ do
      let naked = unguardedVars ex
          blame = BecauseOf b
      verifyExpr ex

      unless (naked `Set.disjoint` vars) $
        tell (Seq.singleton (MalformedRecursiveRhs blame var (Set.toList (naked `Set.intersection` vars))))

verifyModule :: MonadVerify m => ModuleTerm Typed -> m ()
verifyModule (ModStruct m _) = verifyProgram m
verifyModule ModRef{} = pure ()
verifyModule ModImport{} = pure ()
verifyModule ModTargetImport{} = pure ()

verifyExpr :: MonadVerify m => Expr Typed -> m ()
verifyExpr (VarRef v (s, t)) = do
  modify $ Set.delete (BindingSite v s t)
  pure ()
verifyExpr ex@(Let _ vs e (_, ty)) = do
  when (isLazy ty && isWrappedThunk e && any nonTrivialRhs vs) $
    tell (Seq.singleton (LazyLet ex ty))
  verifyBindingGroup (modify . Set.insert) (BecauseOf ex) vs
  verifyExpr e
verifyExpr (If c t e _) = traverse_ verifyExpr [c, t, e]
verifyExpr (App f x _) = verifyExpr f *> verifyExpr x
verifyExpr m@(Fun (PatParam p) x _) = verifyMatch (const $ pure ()) (spanOf m) (getType p) [Arm p Nothing x (spanOf m)]
verifyExpr (Fun (EvParam _) (Match e bs an _) _) = do
  -- Handle desugared `function`.
  verifyExpr e
  verifyMatch
    (\(Arm p _ _ _) -> tell . pure $ MatchToFun an p)
    an (getType e) bs

verifyExpr (Fun _ x _) = verifyExpr x
verifyExpr (Begin es _) = traverse_ verifyExpr es
verifyExpr Literal{} = pure ()
verifyExpr (Match e bs an _) = do
  verifyExpr e
  verifyMatch
    (\(Arm p _ _ _) -> unless (gadtPat p) $ tell . pure $ MatchToLet an p)
    an (getType e) bs
verifyExpr Function{} = error "Impossible: Function has been desugared."
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
verifyExpr (Vta e _ _) = verifyExpr e
verifyExpr (OpenIn _ e _) = verifyExpr e
verifyExpr (ListExp e _) = traverse_ verifyExpr e
verifyExpr (ListComp e qs _) = verifyExpr e *> traverse_ verifyCompStmt qs
verifyExpr (Idiom _ _ es _) = verifyExpr es
verifyExpr (DoExpr _ qs _) = traverse_ verifyCompStmt qs
verifyExpr (ListFrom _ x _) = verifyExpr x
verifyExpr (ListFromTo _ x y _) = verifyExpr x *> verifyExpr y
verifyExpr (ListFromThen _ x y _) = verifyExpr x *> verifyExpr y
verifyExpr (ListFromThenTo _ x y z _) = verifyExpr x *> verifyExpr y *> verifyExpr z
verifyExpr (ExprWrapper w e a) =
  case w of
    WrapFn (MkWrapCont k _) -> verifyExpr (k e)
    ExprApp x -> verifyExpr x *> verifyExpr e
    x :> y -> verifyExpr (ExprWrapper x (ExprWrapper y e a) a)
    _ -> verifyExpr e

verifyCompStmt :: MonadVerify m => CompStmt Typed -> m ()
verifyCompStmt (CompGuard e) = verifyExpr e
verifyCompStmt (CompGen _ e _) = verifyExpr e
verifyCompStmt stmt@(CompLet bg _) =
  verifyBindingGroup (modify . Set.insert) (BecauseOf stmt) bg

unguardedVars :: Expr Typed -> Set.Set (Var Typed)
unguardedVars (Ascription e _ _)   = unguardedVars e
unguardedVars (RecordExt e rs _)   = unguardedVars e <> foldMap (unguardedVars . view fExpr) rs
unguardedVars (BinOp a b c _)      = unguardedVars a <> unguardedVars b <> unguardedVars c
unguardedVars (VarRef v _)         = Set.singleton v
unguardedVars (Begin es _)         = foldMap unguardedVars es
unguardedVars (Let _ vs b _)       = (unguardedVars b <> foldMap (unguardedVars . view bindBody) vs)
                              Set.\\ foldMapOf (each . bindVariable) Set.singleton vs
unguardedVars (App f x _) =
  case f of
    ExprWrapper _ (VarRef v _) _ | v == forceName -> freeIn f <> freeIn x
    _ -> freeIn f <> unguardedVars x
unguardedVars Fun{}                = mempty
unguardedVars (Record rs _)        = foldMap (unguardedVars . view fExpr) rs
unguardedVars (Access e _ _)       = unguardedVars e
unguardedVars (Match t ps _ _)     = unguardedVars t <> foldMap unguardedVarsBranch ps where
  unguardedVarsBranch (Arm p g e _) = (foldMap unguardedVars g <> unguardedVars e) Set.\\ bound p
unguardedVars Literal{}            = mempty
unguardedVars Hole{}               = mempty
unguardedVars (If a b c _)         = unguardedVars a <> unguardedVars b <> unguardedVars c
unguardedVars (Tuple es _)         = foldMap unguardedVars es
unguardedVars (ExprWrapper w e a) =
  case w of
    WrapFn (MkWrapCont k _) -> unguardedVars (k e)
    ExprApp x -> unguardedVars x <> unguardedVars e
    x :> y -> unguardedVars (ExprWrapper x (ExprWrapper y e a) a)
    _ -> unguardedVars e
unguardedVars (Parens e _)         = unguardedVars e
unguardedVars (LeftSection a b _)  = unguardedVars a <> unguardedVars b
unguardedVars (RightSection a b _) = unguardedVars a <> unguardedVars b
unguardedVars (BothSection b _)    = unguardedVars b
unguardedVars AccessSection{}      = mempty
unguardedVars x = error (show x)

-- | Get all binding sites within a pattern
bindingSites :: Pattern Typed -> Set.Set BindingSite
bindingSites (Capture v (s, t)) = Set.singleton (BindingSite v s t)
bindingSites (PAs p v (s, t)) = Set.insert (BindingSite v s t) (bindingSites p)
bindingSites Wildcard{} = mempty
bindingSites PLiteral{} = mempty
bindingSites (Destructure _ p _) = foldMap bindingSites p
bindingSites (PType p _ _) = bindingSites p
bindingSites (PRecord rs _) = foldMap (bindingSites . snd) rs
bindingSites (PTuple ps _) = foldMap bindingSites ps
bindingSites (PGadtCon _ _ _ p _) = foldMap bindingSites p
bindingSites PList{} = error "PList is handled by desugar"

-- | Is this of type lazy?
isLazy :: Type Typed -> Bool
isLazy ty = tyLazy == head (appsView (getHead ty))

isWrappedThunk :: Expr Typed -> Bool
isWrappedThunk (ExprWrapper (WrapFn (MkWrapCont _ x)) _ _) =
  x == "automatic thunking"
isWrappedThunk _ = False

-- | Determine if the right-hand-side of a binding is non-trivial.
nonTrivialRhs :: Binding Typed -> Bool
nonTrivialRhs (TypedMatching _ e _ _) = nonTrivial e
nonTrivialRhs (Binding _ e _ _) = nonTrivial e
nonTrivialRhs _ = error "nonTrivialRHS pre-TC Bindings"

-- | Determining if an expression is non-trivial. Namely, evaluating it
-- may have side-effects.
nonTrivial :: Expr Typed -> Bool
nonTrivial App{} = True
nonTrivial BinOp{} = True
nonTrivial (If c t e _) = nonTrivial c || nonTrivial t || nonTrivial e
nonTrivial (Let _ vs e _) = nonTrivial e || any nonTrivialRhs vs
nonTrivial (Begin es _) = any nonTrivial es
nonTrivial (Match e cs _ _) = nonTrivial e || any (\(Arm _ g a _) -> nonTrivial a || maybe False nonTrivial g) cs
nonTrivial VarRef{} = False
nonTrivial Fun{} = False
nonTrivial Literal{} = False
nonTrivial Function{} = False
nonTrivial Hole{} = False
nonTrivial (Ascription e _ _) = nonTrivial e
nonTrivial (Vta e _ _) = nonTrivial e
nonTrivial (Record rs _) = any (nonTrivial . view fExpr) rs
nonTrivial (RecordExt e rs _) = nonTrivial e || any (nonTrivial . view fExpr) rs
nonTrivial (Access e _ _) = nonTrivial e
nonTrivial LeftSection{} = False
nonTrivial AccessSection{} = False
nonTrivial RightSection{} = False
nonTrivial BothSection{} = False
nonTrivial (Parens e _) = nonTrivial e
nonTrivial (Tuple es _) = any nonTrivial es
nonTrivial (ListExp es _) = any nonTrivial es
nonTrivial ListComp{} = False
nonTrivial ListFrom{} = True
nonTrivial ListFromTo{} = True
nonTrivial ListFromThen{} = True
nonTrivial ListFromThenTo{} = True
nonTrivial DoExpr{} = False
nonTrivial Idiom{} = False
nonTrivial TupleSection{} = False
nonTrivial (OpenIn _ e _) = nonTrivial e
nonTrivial Lazy{} = False
nonTrivial (ExprWrapper w e _) =
  case w of
    WrapFn (MkWrapCont k _) -> nonTrivial (k e)
    _ -> nonTrivial e

gadtPat :: Pattern Typed -> Bool
gadtPat = any isGadt . universe where
  isGadt (PGadtCon _ _ vs _ _) = not (null vs)
  isGadt _ = False

-- | Verify a series of patterns are total
verifyMatch :: MonadVerify m
            => (Arm Typed -> m ())
            -- ^ An additional function to call on well-formed matches of one case.
            -> Span -- ^ The match expression, used for error reporting
            -> Type Typed -- ^ The type of the term to match against
            -> [Arm Typed] -- ^ The arms within the current match
            -> m ()
verifyMatch _ m ty [] = do
  VerifyScope env as _ <- ask
  when (inhabited env as ty) $
    tell . pure $ MissingPattern m [VVariable undefined ty]
verifyMatch rep m ty bs = do
  VerifyScope env va _ <- ask
  ty <- pure $ expandTypeWith (env ^. tySyms) ty

  (_, ok, unc) <- foldlM (\(i :: Int, ok, alts) a@(Arm pat guard body _) -> do
    let cov  = covering env pat alts
    -- If the covered set is empty, this arm is redundant
    (va', ok) <- case covered cov of
      Seq.Empty -> do
        let b = case bs of
                  [_] -> BecauseMatch
                  _ | i == 0 -> BecauseArm
                  _ -> Shadowed
        tell . pure $ RedundantArm a b
        pure (va, False)
      (va', _) Seq.:<| _ -> pure (va', ok)

    local (\vs -> vs { patternAbs = va' }) $ do
      modify (Set.union (bindingSites pat))
      maybe (pure ()) verifyExpr guard
      verifyExpr body

    -- Return the filtered uncovered set if this pattern has no guard,
    -- otherwise use the original uncovered set.
    pure . (i + 1, ok, ) $ case guard of
      Nothing -> uncovered cov
      Just{} -> alts)
    (0, True, pure $ emptyAlt va ty) bs

  case (unc, bs) of
    (Seq.Empty, [a]) | ok -> rep a
    (Seq.Empty, _) -> pure ()
    (_, _) -> tell . pure . MissingPattern m . map snd . toList $ unc

unqualifyWrt :: (Var p ~ Var Resolved) => T.Text -> Type p -> Type p
unqualifyWrt n = transformType go where
  go (TyCon v a) = TyCon (unqualifyVarWrt n v) a
  go t = t

unqualifyVarWrt :: T.Text -> Var Resolved -> Var Resolved
unqualifyVarWrt n (TgName v id)
  | n `T.isPrefixOf` v = TgName (T.drop (T.length n) v) id
  | otherwise = TgName v id
unqualifyVarWrt _ n = n
