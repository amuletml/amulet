{-# LANGUAGE FlexibleContexts, TupleSections, ScopedTypeVariables,
   TypeFamilies, CPP, StandaloneDeriving, UndecidableInstances #-}
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

import Syntax.Subst
import Syntax.Var
import Syntax

import Types.Infer.Builtin
import {-# SOURCE #-} Types.Infer
import Types.Kinds
import Types.Unify

import Text.Pretty.Semantic


#ifdef TRACE_TC
import Debug.Trace
#endif

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

#ifdef TRACE_TC
    traceM ("function type: " ++ displayS (pretty function_t))
#endif

    ((ql_sub, quantifiers), cs) <-
      censor (const mempty) . listen $
        quickLook function_t =<< for arguments (\a@(x, y) -> (x, y,) <$> inferQL a)

    quantifiers <- pure (applyQ ql_sub quantifiers)

    let fake_ty_con = TyCon (TgInternal T.empty)
    for_ (Map.toList ql_sub) $ \(var, tau) ->
      -- Here we need these to be guarded, so they need to be in the RHS
      -- of a type application. Since we don't have a suitable type, we
      -- use an internal TyCon with an invisible name. (Evil!)
      unify (becauseExp exp) (TyApp fake_ty_con (TyVar var)) (TyApp fake_ty_con tau)

    tell cs

    r_ql_sub <- case expected of
      Just tau@(TyApps (TyCon _) (_:_)) | result@(TyApps (TyCon _) (_:_)) <- getQuantR quantifiers -> do
-- Pushing down the result type into the quick look substitution
-- is only sound if both are /guarded/. Do note that missing a
-- substitution here isn't unsound, it'll just lead to wonky inference.
        _ <- subsumes (becauseExp exp) result (apply ql_sub tau)
        pure $ fromMaybe mempty (unifyPure result tau)
      _ -> pure mempty

#ifdef TRACE_TC
    traceM ("Quick Look results: " ++ show (pretty <$> (ql_sub <> r_ql_sub), pretty quantifiers))
#endif

    (arg_ks, result) <-
      checkArguments arguments (applyQ r_ql_sub quantifiers)

#ifdef TRACE_TC
    traceM ("resulting type: " ++ displayS (pretty result))
#endif

    wrap <- case expected of
      Just tau -> do
#ifdef TRACE_TC
        traceM ("QL expected result: " ++ displayS (pretty tau))
#endif
        wrap <- subsumes (becauseExp exp) result tau
        pure $ \ex -> ExprWrapper wrap ex (annotation ex, tau)
      Nothing -> pure id

    pure (wrap (foldr (.) id (reverse arg_ks) function), result)
  where
    spine ex@(App fn arg _) =
      let sp = spine fn
       in (ExprArg arg, BecauseOf ex):sp
    spine ex@(Vta fn arg _) =
      let sp = spine fn
       in (TypeArg arg, BecauseOf ex):sp
    spine ex = [(ExprArg ex, BecauseOf ex)]

    checkArguments ((ExprArg arg, _):as) (Quant tau dom cod inst_cont qs) =
      case dom of
        Anon dom -> do
          x <- check arg dom

          let cont ex = App ex x (annotation ex <> annotation x, cod)

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
              cont ex = ExprWrapper (TypeApp arg) ex (annotation ex <> annotation reason, ty)

          (conts, result) <- checkArguments as qs
          pure (inst_cont:cont:conts, result)
        _ -> confesses (ArisingFrom (CanNotVta tau arg) reason)

    checkArguments [] Quant{} = error "arity mismatch. impossible in checkArguments"

    checkArguments _ (Result tau) = pure ([], tau)

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

inferQL :: MonadInfer Typed m => (Arg Desugared, SomeReason) -> m (Maybe (Type Typed))
inferQL (arg, reason) = case arg of
  ExprArg a -> inferQL_ex a
  TypeArg tau -> pure <$> liftType reason tau

inferQL_ex :: MonadInfer Typed m => Expr Desugared -> m (Maybe (Type Typed))
inferQL_ex ex@(VarRef x _) = do
  (_, _, (new, _)) <- third3A (discharge ex) =<< lookupTy' Strong x
  (_, tau) <- censor (const mempty) $ instantiateTc (BecauseOf ex) new
  if hasPoly tau
     then pure (pure tau)
     else pure Nothing
inferQL_ex (Literal l _) = pure (pure (litTy l))
inferQL_ex ex@(Ascription _ t _) = pure <$> liftType (BecauseOf ex) t
inferQL_ex _ = pure Nothing

data Arg p = ExprArg (Expr p) | TypeArg (Type p)
deriving instance (Show (Var p), Show (Ann p)) => Show (Arg p)

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
  let sub = Map.singleton v (TyVar new_v)
  TyPi (Invisible new_v k r) <$> refresh (apply sub t)
refresh t = pure t

hasPoly :: Type Typed -> Bool
hasPoly = any isForall . universe where
  isForall (TyPi Invisible{} _) = True
  isForall _ = False
