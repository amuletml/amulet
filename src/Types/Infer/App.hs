{-# LANGUAGE FlexibleContexts, TupleSections, ScopedTypeVariables,
   TypeFamilies, CPP, StandaloneDeriving, UndecidableInstances #-}

-- | This module implements the "Quick Look" impredicative polymorphism.
-- At a glance: Application spines are traversed twice, the first time
-- to collect impredicative instantiations (this is the "quick look"
-- pass, which is so called because it ignores hard expressions - @let@,
-- @match@ etc) and the second time to type-check as usual.
module Types.Infer.App (inferApps) where

import Prelude

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Traversable
import Data.Foldable
import Data.Spanned
import Data.Reason
import Data.Triple
import Data.Maybe

import Control.Monad.Infer
import Control.Lens

import Syntax.Builtin
import Syntax.Subst
import Syntax.Types
import Syntax.Var
import Syntax

import {-# SOURCE #-} Types.Infer
import Types.Infer.Builtin
import Types.Kinds
import Types.Unify

import Text.Pretty.Semantic

import Types.Unify.Trace

-- | Infer an application, supporting "Quick Look" impredicativity: We
-- consider all of the arguments to a function application (or
-- operator) together, then do two passes over them:
--
--  * The first pass is what we call a 'quick look'. Here, we try to
--  infer polymorphic type instantiations for some of the function's
--  arguments. This pass only considers "simple" arguments: Variables
--  (for now).
--
--  * The second pass is the real use of 'checks' that we've always done
--  for arguments.
inferApps :: forall m. MonadInfer Typed m => Expr Desugared -> Maybe (Type Typed) -> m (Expr Typed, Type Typed)
inferApps exp expected =
  do
    let ((ExprArg function, _):arguments) = reverse $ spine exp
    (function, function_t) <- infer' function
    function_t <- refresh function_t

    traceM TcQL (string "function type:" <+> pretty function_t)

    -- Pass 1:
    ((ql_sub, quantifiers), cs) <-
      censor (const mempty) . listen $
        quickLook function_t =<< for arguments (\a@(x, y) -> (x, y,) <$> inferQL a)

    quantifiers <- pure (applyQ ql_sub quantifiers)

    let fake_ty_con = TyCon (TgInternal T.empty) ()
    for_ (Map.toList ql_sub) $ \(var, tau) ->
      -- Here we need these to be guarded, so they need to be in the RHS
      -- of a type application. Since we don't have a suitable type, we
      -- use an internal TyCon with an invisible name. (Evil!)
      unify (becauseExp exp) (TyApp fake_ty_con (TyVar var ())) (TyApp fake_ty_con tau)

    tell cs

    ty_syms <- view tySyms

    r_ql_sub <- case expected of
      Just tau@(TyApps (TyCon t ()) (_:_))
        | result@(TyApps (TyCon t' ()) (_:_)) <- getQuantR quantifiers
        , invariant ty_syms t, invariant ty_syms t' -> do
        -- Pushing down the result type into the quick look substitution
        -- is only sound if both are /guarded/, i.e. applications of an
        -- (invariant) constructor. Do note that missing a substitution
        -- here isn't unsound, it'll just lead to wonky inference.
        _ <- subsumes (becauseExp exp) result (apply ql_sub tau)
        pure $ fromMaybe mempty (unifyPure result tau)
      _ -> pure mempty

    traceM TcQL (string "QL pass subst:" <+> shown (pretty <$> (ql_sub <> r_ql_sub)))
    traceM TcQL (string "QL pass quant:" <+> pretty quantifiers)

    (arg_ks, result) <-
      checkArguments arguments (applyQ r_ql_sub quantifiers)

    traceM TcQL (pretty exp <+> soperator (char '↑') <+> pretty result)

    wrap <- case expected of
      Just tau -> do
        traceM TcQL (pretty result <+> soperator (char '≤') <+> pretty tau)
        wrap <- subsumes (becauseExp exp) result tau
        pure $ \ex -> ExprWrapper wrap ex (spanOf ex, tau)
      Nothing -> pure id

    pure (wrap (foldr ($) function (reverse arg_ks)), result)

spine :: (Ann p ~ Ann Resolved, Var p ~ Var Resolved) => Expr p -> [(Arg p, SomeReason)]
spine ex@(App fn arg _) =
  let sp = spine fn
   in (ExprArg arg, BecauseOf ex):sp
spine ex@(Vta fn arg _) =
  let sp = spine fn
   in (TypeArg arg, BecauseOf ex):sp
spine ex = [(ExprArg ex, BecauseOf ex)]

-- | Check the given 'Arg's against some 'Quantifiers', returning a set
-- of suspended 'App'/'Vta's and the result 'Type' of the expression.
checkArguments :: MonadInfer Typed m
               => [(Arg Desugared, SomeReason)]
               -> Quantifiers Typed
               -> m ( [ Expr Typed -> Expr Typed]
                    , Type Typed )
checkArguments ((ExprArg arg, _):as) (Quant tau dom cod inst_cont qs) =
  case dom of
    Anon dom -> do
      x <- check arg dom

      let cont ex = App ex x (spanOf ex <> spanOf x, cod)

      (conts, result) <- checkArguments as qs
      pure (inst_cont:cont:conts, result)
    Invisible _ _ Req -> do
      (_, t) <- infer arg
      b <- freshTV
      confesses (NotEqual tau (TyArr t b))
    _ -> error "checkArguments ExprArg: impossible quantifier"

checkArguments ((TypeArg arg, reason):as) (Quant tau dom cod inst_cont qs) =
  case dom of
    Invisible v kind r | r /= Infer{} -> do
      arg <- case kind of
        Just k -> checkAgainstKind reason arg k
        Nothing -> resolveKind reason arg

      let ty = apply (Map.singleton v arg) cod
          cont ex = ExprWrapper (TypeApp arg) ex (spanOf ex <> spanOf reason, ty)

      (conts, result) <- checkArguments as qs
      pure (inst_cont:cont:conts, result)
    _ -> confesses (ArisingFrom (CanNotVta tau arg) reason)

checkArguments [] Quant{} = error "arity mismatch. impossible in checkArguments"

checkArguments _ (Result tau) = pure ([], tau)

-- | Perform the "quick look" pass, exposing as many quantifiers in the
-- function's type as there are 'Arg's given, returning a substitution
-- with impredicative instantiation and a linked data structure
-- representing the quantifiers.
quickLook :: MonadInfer Typed m
          => Type Typed
          -> [(Arg Desugared, SomeReason, Maybe (Type Typed))]
          -> m ( Subst Typed
               , Quantifiers Typed )
quickLook t ((ExprArg _, reason, ql_t):args) = do
  (dom, cod, wrap) <- quantifier reason (/= Req) t
  case dom of
    Anon dom -> do
      let sub =
            case ql_t of
              Just ql_t -> fromMaybe mempty (unifyPure dom ql_t)
              _ -> mempty
      (rest_sub, qs) <- quickLook (apply sub cod) args
      pure (sub `compose` rest_sub, Quant t (Anon (apply sub dom)) (apply sub cod) wrap qs)
    _ -> do
      (sub, qs) <- quickLook cod args
      pure (sub, Quant t dom cod wrap qs)
quickLook t ((TypeArg tau, reason, _):args) = do
  tau <- liftType reason tau
  (dom, cod, wrap) <- quantifier reason (== Infer) t
  case dom of
    Invisible v _ _ -> do
      let sub = Map.singleton v tau
      (rest_sub, qs) <- quickLook (apply sub cod) args
      pure (sub `compose` rest_sub, Quant t dom (apply sub cod) wrap qs)
    _ -> do
      (sub, qs) <- quickLook cod args
      pure (sub, Quant t dom cod wrap qs)
quickLook tau [] = pure (mempty, Result tau)

-- | Return the impredicative instantiation from quickly looking at this
-- expression.
inferQL :: MonadInfer Typed m => (Arg Desugared, SomeReason) -> m (Maybe (Type Typed))
inferQL (arg, reason) = case arg of
  ExprArg a -> inferQL_ex a
  TypeArg tau -> pure <$> liftType reason tau

-- | Look at an expression quickly.
inferQL_ex :: MonadInfer Typed m => Expr Desugared -> m (Maybe (Type Typed))

inferQL_ex ex | trace TcQL (keyword "Γ ⊢" <+> pretty ex <+> soperator (char '↑')) False = undefined

inferQL_ex ex@(VarRef x _) = do
  (_, _, (new, _)) <- third3A (discharge ex) =<< lookupTy' Strong x
  (_, tau) <- censor (const mempty) $ instantiateTc (BecauseOf ex) new
  if hasPoly tau
     then pure (pure tau)
     else pure Nothing

-- Look at expressions quickly. Here, we assume that traversing
-- expressions is "cheap", but solving constraints is "expensive". Since
-- the second pass will solve constraints anyway, we /don't need any
-- solving from the quick look pass/.
inferQL_ex ex@App{} = quiet $ do
  t <- snd <$> inferApps ex Nothing
  pure . snd <$> instantiateTc (BecauseOf ex) t
inferQL_ex ex@Vta{} = quiet $ do
  t <- snd <$> inferApps ex Nothing
  pure . snd <$> instantiateTc (BecauseOf ex) t
inferQL_ex ex@(BinOp l o r a) = quiet $ do
  t <- snd <$> inferApps (App (App o l a) r a) Nothing
  pure . snd <$> instantiateTc (BecauseOf ex) t

inferQL_ex (Literal l _) = pure (pure (litTy l))
inferQL_ex ex@(Ascription _ t _) = pure <$> liftType (BecauseOf ex) t
inferQL_ex _ = pure Nothing

-- | Is this type constructor invariant in its arguments?
invariant :: TySyms -> Var Typed -> Bool
invariant syms x =
     x /= tyArrowName       -- The function type is co/contravariant
  && x /= tyTupleName       -- The tuple type is covariant
  && x `Map.notMember` syms -- Type families have weird variance that can't be solved quickly

data Arg p = ExprArg (Expr p) | TypeArg (Type p)
deriving instance ShowPhrase p => Show (Arg p)

instance Pretty (Var p) => Pretty (Arg p) where
  pretty (ExprArg e) = pretty e
  pretty (TypeArg t) = char '@' <> pretty t

data Quantifiers p = Quant (Type p) (TyBinder p) (Type p) (Expr p -> Expr p) (Quantifiers p) | Result (Type p)

instance Pretty (Var p) => Pretty (Quantifiers p) where
  pretty (Result t) = pretty t
  pretty (Quant _ b _ _ qs) = pretty b <+> pretty qs

applyQ :: Subst Typed -> Quantifiers Typed -> Quantifiers Typed
applyQ sub (Quant tau quant cod wrap qs) = Quant tau (apply sub quant) cod wrap (applyQ sub qs)
applyQ sub (Result t) = Result (apply sub t)

getQuantR :: Quantifiers p -> Type p
getQuantR (Quant _ _ _ _ qs) = getQuantR qs
getQuantR (Result t) = t

refresh :: MonadNamey m => Type Typed -> m (Type Typed)
refresh (TyPi (Invisible v k r) t) = do
  let nn (TgName t _) = t
      nn (TgInternal t) = t
  new_v <- genNameFrom (nn v)
  let sub = Map.singleton v (TyVar new_v ())
  TyPi (Invisible new_v k r) <$> refresh (apply sub t)
refresh t = pure t

hasPoly :: Type Typed -> Bool
hasPoly = any isForall . universe where
  isForall (TyPi Invisible{} _) = True
  isForall _ = False

quiet :: MonadWriter w m => m a -> m a
quiet = censor (const mempty)
