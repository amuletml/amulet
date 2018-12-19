{-# LANGUAGE ConstraintKinds, FlexibleContexts, FlexibleInstances,
   UndecidableInstances, MultiParamTypeClasses, OverloadedStrings,
   ScopedTypeVariables #-}
module Syntax.Verify
  ( VerifyError(..)
  , BindingSite(..)
  , runVerify
  , verifyProgram
  ) where

import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Foldable
import Data.Reason
import Data.Graph

import Control.Monad.Writer.Strict
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Lens hiding (Lazy, (:>))

import Text.Pretty.Semantic

import Syntax.Verify.Pattern
import Syntax.Verify.Error

import Syntax.Builtin (tyUnit, tyLazy, forceName)
import Syntax.Pretty
import Syntax.Subst
import Syntax.Types
import Syntax.Let

import Language.Lua.Parser

import Types.Infer.Builtin (spine, getHead)

data VerifyScope = VerifyScope Env AbsState

type MonadVerify m =
  ( MonadWriter (Seq.Seq VerifyError) m
  , MonadReader VerifyScope m
  , MonadState (Set.Set BindingSite) m )

runVerify :: Env
          -> Var Resolved
          -> WriterT (Seq.Seq VerifyError) (StateT (Set.Set BindingSite) (Reader VerifyScope)) () -> Either (Seq.Seq VerifyError) ()
runVerify env var = fixup
                  . flip runReader (VerifyScope env (emptyAbsState var))
                  . flip runStateT mempty
                  . runWriterT where
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
  verifyStmt st@(LetStmt _ vs) = verifyBindingGroup (flip const) (BecauseOf st) vs
  verifyStmt Class{} = pure ()
  verifyStmt Instance{} = pure ()
  verifyStmt st@(ForeignVal _ v d t (_, _)) = do
    case parseExpr (SourcePos ("definition of " ++ displayS (pretty v)) 1 1) (d ^. lazy) of
      Left e -> tell (Seq.singleton (ParseErrorInForeign st e))
      Right _ -> pure ()
    parametricity st t

  verifyStmt TypeDecl{} = pure ()
  verifyStmt (Module _ _ p) = verifyProgram p
  verifyStmt Open{} = pure ()

verifyBindingGroup :: MonadVerify m
                   => (BindingSite -> Set.Set BindingSite -> Set.Set BindingSite)
                   -> SomeReason -> [Binding Typed] -> m ()
verifyBindingGroup k _ = traverse_ verifyScc . depOrder where
  verifyScc (AcyclicSCC (Binding v e c (s, t))) =
    when c $ do
      modify (k (BindingSite v s t))
      verifyExpr e
  verifyScc (AcyclicSCC (TypedMatching p e _ _)) = do
    traverse_ (modify . k) $ bindingSites p
    verifyExpr e

  verifyScc (AcyclicSCC Matching{}) = error "Matching after TC"

  verifyScc (CyclicSCC vs) = do
    let vars = foldMapOf (each . bindVariable) Set.singleton vs
    for_ vs $ \(Binding var _ c (s, ty)) ->
      when c $ modify (k (BindingSite var s ty))
    for_ vs $ \b@(Binding var ex c (_, _)) -> when c $ do
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
verifyExpr m@(Fun (PatParam p) x _) = verifyMatch m (getType p) [Arm p Nothing x]
verifyExpr (Fun _ x _) = verifyExpr x
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
verifyExpr m@(Match e bs _) = do
  verifyExpr e
  verifyMatch m (getType e) bs
verifyExpr m@(Function bs (_, ty)) = do
  let (TyPi (Anon arg) _) = ty
  verifyMatch m arg bs
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
verifyExpr (ListComp e _ _) = verifyExpr e
verifyExpr (ExprWrapper w e a) =
  case w of
    WrapFn (MkWrapCont k _) -> verifyExpr (k e)
    ExprApp x -> verifyExpr x *> verifyExpr e
    x :> y -> verifyExpr (ExprWrapper x (ExprWrapper y e a) a)
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
    ExprWrapper _ (VarRef v _) _ | v == forceName -> freeIn f <> freeIn x
    _ -> freeIn f <> unguardedVars x
unguardedVars Fun{}                = mempty
unguardedVars (Record rs _)        = foldMap (unguardedVars . view fExpr) rs
unguardedVars (Access e _ _)       = unguardedVars e
unguardedVars (Match t ps _)       = unguardedVars t <> foldMap unguardedVarsBranch ps where
  unguardedVarsBranch (Arm p g e)  = (foldMap unguardedVars g <> unguardedVars e) Set.\\ bound p
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
bindingSites PList{} = error "PList is handled by desugar"

isLazy :: Type Typed -> Bool
isLazy ty = tyLazy == head (spine (getHead ty))

isWrappedThunk :: Expr Typed -> Bool
isWrappedThunk (ExprWrapper (WrapFn (MkWrapCont _ x)) _ _) =
  x == "automatic thunking"
isWrappedThunk _ = False

nonTrivialRhs :: Binding Typed -> Bool
nonTrivialRhs (TypedMatching _ e _ _) = nonTrivial e
nonTrivialRhs (Binding _ e _ _) = nonTrivial e
nonTrivialRhs _ = error "nonTrivialRHS pre-TC Bindings"

nonTrivial :: Expr Typed -> Bool
nonTrivial App{} = True
nonTrivial BinOp{} = True
nonTrivial (If c t e _) = nonTrivial c || nonTrivial t || nonTrivial e
nonTrivial (Let vs e _) = nonTrivial e || any nonTrivialRhs vs
nonTrivial (Begin es _) = any nonTrivial es
nonTrivial (Match e cs _) = nonTrivial e || any (\(Arm _ g a) -> nonTrivial a || maybe False nonTrivial g) cs
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

-- | Verify a series of patterns are total
verifyMatch :: MonadVerify m => Expr Typed -> Type Typed -> [Arm Typed] -> m ()
verifyMatch m ty [] = do
  VerifyScope env as <- ask
  when (inhabited env as ty) $
    tell . pure $ MissingPattern m [VVariable undefined ty]

verifyMatch m ty bs = do
  VerifyScope env va <- ask

  unc <- foldlM (\alts a@(Arm pat guard body) -> do
    let cov  = covering env pat alts
    -- If the covered set is empty, this arm is redundant
    va' <- case covered cov of
      Seq.Empty -> do
        tell . pure $ RedundantArm a
        pure va
      (va', _) Seq.:<| _ -> pure va'

    local (\(VerifyScope env _) -> VerifyScope env va') $ do
      modify (Set.union (bindingSites pat))
      maybe (pure ()) verifyExpr guard
      verifyExpr body

    -- Return the filtered uncovered set if this pattern has no guard,
    -- otherwise use the original uncovered set.
    pure $ case guard of
      Nothing -> uncovered cov
      Just{} -> alts)
    (pure $ emptyAlt va ty) bs

  unless (null unc) (tell . pure . MissingPattern m . map snd . toList $ unc)
