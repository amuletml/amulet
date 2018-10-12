{-# LANGUAGE FlexibleContexts, TupleSections, ScopedTypeVariables,
   ViewPatterns, LambdaCase #-}
module Types.Infer.Class (inferClass, inferInstance, reduceClassContext) where

import Prelude hiding (lookup)

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Set as Set
import Data.Traversable
import Data.Foldable
import Data.Spanned
import Data.Reason
import Data.Triple
import Data.Maybe
import Data.List (sortOn)
import Data.Char

import Control.Monad.State
import Control.Monad.Infer
import Control.Arrow (second)
import Control.Lens

import Syntax.Implicits
import Syntax.Subst
import Syntax.Types
import Syntax.Var
import Syntax

import {-# SOURCE #-} Types.Infer
import Types.Infer.Builtin
import Types.Kinds
import Types.Unify

import Text.Pretty.Semantic

import GHC.Stack

inferClass :: forall m. MonadInfer Typed m
           => Toplevel Resolved
           -> m ( [Toplevel Typed]
                , Telescope Typed
                , Toplevel Typed
                , ImplicitScope Typed )
inferClass clss@(Class name ctx _ methods classAnn) = do
  let toVar :: TyConArg Typed -> Type Typed
      toVar (TyVarArg v) = TyVar v
      toVar (TyAnnArg v _) = TyVar v

  let classCon' =
        T.cons (toUpper (T.head (nameName name)))
          (T.tail (nameName name))
  classCon <- TvName <$> genNameFrom classCon'

  name <- pure (TvName name)
  (k, params) <- resolveClassKind clss
  let classConstraint = foldl TyApp (TyCon name) (map toVar params)
      scope (TyAnnArg v k:cs) = one v k <> scope cs
      scope (_:cs) = scope cs
      scope [] = mempty

  local (names %~ focus (one name k <> scope params)) $ do
    -- Infer the types for every method
    (methods, decls, rows) <- fmap unzip3 . for methods $ \(method, ty) -> do
      method <- pure (TvName method)
      ty <- resolveKind (BecauseOf clss) ty
      withHead <- closeOver (BecauseOf clss) $
        TyPi (Implicit classConstraint) ty

      pure ( (method, ty)
           , (method, withHead)
           , (Anon, method, nameName (unTvName method), ty))

    let tele = one name k <> teleFromList decls
        unwind (TyTuple a b) = a:unwind b
        unwind t = pure t
        getContext Nothing = []
        getContext (Just t) = unwind t

    ctx <- traverse (\x -> checkAgainstKind (BecauseOf clss) x tyConstraint) ctx
    (fold -> scope, rows') <- fmap unzip . for (getContext ctx) $
      \obligation -> do
        impty <- retcon (const mempty) $
          closeOver (BecauseOf clss) $
            TyPi (Implicit classConstraint) obligation
        var@(TgName name _) <- genNameWith (classCon' <> T.singleton '$')
        pure ( singleton classAnn Superclass (TvName var) impty
             , (Implicit, TvName var, name, obligation))

    let inner :: Type Typed
        inner = TyExactRows (map (\(_, _, x, y) -> (x, y)) rows
                          ++ map (\(_, _, x, y) -> (x, y)) rows')

    classConTy <- retcon (const mempty) $
      closeOver (BecauseOf clss) (TyArr inner classConstraint)
    let tyDecl :: Toplevel Typed
        tyDecl = TypeDecl name params
          [ArgCon classCon inner (classAnn, classConTy)]
    let mkDecl :: (Type Typed -> TyBinder Typed, Var Typed, T.Text, Type Typed) -> m (Binding Typed)
        mkDecl (f, var, label, theTy) = do
          capture <- TvName <$> genName
          let ty = TyPi (f classConstraint) theTy
          let expr =
                Fun (PatParam
                       (Destructure classCon
                         (Just (Capture capture (classAnn, inner)))
                         (classAnn, classConstraint)))
                 (Access (VarRef capture (classAnn, inner)) label (classAnn, ty))
                 (classAnn, ty)
          ty <- retcon (const mempty) $
            closeOver (BecauseOf clss) ty
          pure (Binding var expr (classAnn, ty))
    decs <- traverse mkDecl (rows ++ rows')
    pure ( tyDecl:map (LetStmt . pure) decs, tele
         , Class name ctx params ((classCon, classConTy):methods) (classAnn, k)
         , scope)
inferClass _ = error "not a class"

inferInstance :: forall m. MonadInfer Typed m => Toplevel Resolved -> m (Toplevel Typed, Var Typed, Type Typed)
inferInstance inst@(Instance clss ctx instHead bindings ann) = do
  Class clss _ classParams ((classCon, classConTy):methodSigs) classAnn <-
    view (classDecs . at (TvName clss) . non undefined)
  instanceName <- TvName <$> genName
  localInstanceName <- TvName <$> genName
  let toVar :: TyConArg Typed -> Type Typed
      toVar (TyVarArg v) = TyVar v
      toVar (TyAnnArg v _) = TyVar v

      classHead :: Type Typed
      classHead = foldl TyApp (TyCon clss) (map toVar classParams)
  ctx <- case ctx of
    Just x -> checkAgainstKind (BecauseOf inst) x tyConstraint
    Nothing -> pure tyUnit

  instHead <- checkAgainstKind (BecauseOf inst) instHead tyConstraint
  globalInsnConTy <- retcon (const mempty) $
    closeOver (BecauseOf inst) (TyPi (Implicit ctx) instHead)

  scope <- view classes
  case filter ((/= Superclass) . view implSort) $ lookup instHead scope of
    [] -> pure ()
    (x:_) -> confesses (Overlap instHead (x ^. implSpan) ann)

  (instHead, skolSub) <- skolFreeTy (ByInstanceHead instHead ann) instHead
  ctx <- pure (apply skolSub ctx)

  (mappend skolSub -> sub, _, _) <- solve (pure (ConUnify (BecauseOf inst) undefined classHead instHead))
  localInsnConTy <- retcon (const mempty) $
    closeOver (BecauseOf inst) (TyPi (Implicit ctx) instHead)

  (localAssums', instancePattern) <-
    let mkBinds x | x == tyUnit = pure (mempty, PLiteral LiUnit (ann, tyUnit))
        mkBinds (TyTuple a b) = do
          var <- TvName <$> genName
          (scope, pat) <- mkBinds b
          pure (insert ann InstSort var a scope, PTuple [Capture var (ann, a), pat] (ann, TyTuple a b))
        mkBinds x = do
          var <- TvName <$> genName
          pure (singleton ann InstSort var x, Capture var (ann, x))
    in mkBinds ctx

  let localAssums = insert ann InstSort localInstanceName localInsnConTy localAssums'

  methodSigs <- Map.fromList <$> traverse (secondA (closeOver (BecauseOf inst) . apply sub)) methodSigs
  (Map.fromList -> methodMap, methods) <- fmap unzip . local (classes %~ mappend localAssums) $
    for bindings $ \case
      bind@(Binding v e an) -> do
        let sig = methodSigs ! TvName v

        v' <- genNameFrom (nameName v)

        (e, cs) <- listen $ check e sig
        (sub, wrap, cons) <- solve cs
        unless (null cons) $ 
          confesses (addBlame (BecauseOf bind) (UnsatClassCon (BecauseOf e) (head cons) InstanceMethod))

        pure ((nameName v, TvName v'), Binding (TvName v') (Ascription (solveEx sig sub wrap e) sig (an, sig)) (an, sig))
      _ -> error "not possible: non-Binding method"

  let findInner (TyPi Invisible{} k) = findInner k
      findInner (TyPi (Anon x) _) = x
      findInner _ = error "malfomed classConTy"
      TyExactRows whatDo = apply sub (findInner classConTy)

  scope <- mappend localAssums <$> view classes
  (fields, cs) <- listen $ for whatDo $ \(name, ty) ->
    if nameName (unTvName classCon) `T.isPrefixOf` name
       then do
         var <- TvName <$> genName
         tell (pure (ConImplicit (BecauseOf inst) scope var ty))
         pure (Field name
                (Ascription (ExprWrapper (WrapVar var)
                  (Fun (EvParam (Capture var (ann, ty)))
                    (VarRef var (ann, ty)) (ann, TyArr ty ty))
                    (ann, ty)) tyUnit (ann, ty))
                (ann, ty))
       else pure (Field name (VarRef (methodMap ! name) (ann, ty)) (ann, ty))

  (solution, needed, unsolved) <- solve cs

  unless (null unsolved) $
    confesses (addBlame (BecauseOf inst) (UnsatClassCon (BecauseOf inst) (head unsolved) (InstanceClassCon (fst classAnn))))

  let appArg (TyPi (Invisible v _) rest) ex =
        case Map.lookup v sub of
          Just x -> appArg rest $ ExprWrapper (TypeApp x) ex (annotation ex, rest)
          Nothing -> appArg rest $ ExprWrapper (TypeApp TyType) ex (annotation ex, rest)
      appArg _ ex = ex
      getSkol (TySkol s) = s
      getSkol _ = error "not a skol in skolSub"

      addArg ty@(TyPi (Invisible v k) rest) ex =
        addArg rest $ Ascription (ExprWrapper (TypeLam (getSkol (skolSub ! v)) (fromMaybe TyType k)) ex (ann, ty)) ty (ann, ty)
      addArg _ ex = ex
      fun = addArg globalInsnConTy $
        Let [Binding localInstanceName
              (Fun (EvParam (PType instancePattern ctx (ann, ctx)))
                (Let methods
                  (Ascription
                    (App (appArg classConTy (VarRef classCon (ann, classConTy)))
                      (solveEx (TyExactRows whatDo) solution needed (Record fields (ann, TyExactRows whatDo)))
                      (ann, instHead))
                    instHead (ann, instHead))
                  (ann, instHead))
                (ann, localInsnConTy))
              (ann, localInsnConTy)]
          (VarRef localInstanceName (ann, localInsnConTy))
          (ann, localInsnConTy)
      bind = Binding instanceName (Ascription fun globalInsnConTy (ann, globalInsnConTy)) (ann, globalInsnConTy)

  pure (LetStmt [bind], instanceName, globalInsnConTy)
inferInstance _ = error "not an instance"

reduceClassContext :: forall m. MonadInfer Typed m
                   => Ann Resolved
                   -> [Constraint Typed]
                   -> m (Type Typed -> Type Typed, Expr Typed -> Expr Typed)

reduceClassContext _ [] = pure (id, id)
reduceClassContext _ [ConImplicit _ _ var con] =
  pure (TyPi (Implicit con), fun var con)
reduceClassContext annot cons = do
  scope <- view classes
  let needed sub (ConImplicit _ _ var con:cs) = do
        (con, sub') <- skolFreeTy (ByConstraint con) (apply sub con)
        ((var, con):) <$> needed (sub `compose` sub') cs
      needed sub (_:cs) = needed sub cs
      needed _ [] = pure []

  needs <- sortOn fst <$> needed mempty cons

  -- First, deduplicate the constraints eliminating any redundancy
  let dedup :: ImplicitScope Typed -> [Need Typed] -> ([Binding Typed], [Need Typed], ImplicitScope Typed)
      dedup scope ((var, con):needs)
        | [ImplChoice _ t [] v _ _] <- lookup con scope =
          let (bindings, needs', scope') = dedup scope needs
           in if var == v then (bindings, needs', scope') else (Binding var (VarRef v (annot, t)) (annot, t):bindings, needs', scope')
        | otherwise =
          let (bindings, needs', scope') = dedup (insert annot InstSort var con scope) needs
           in (bindings, (var, con):needs', scope')
      dedup scope [] = ([], [], scope)
      (aliases, stillNeeded, usable) = dedup mempty needs

  let simpl :: ImplicitScope Typed -> [Need Typed] -> ([Binding Typed], [Need Typed])
      simpl scp ((var, con):needs)
        | (implicit@(ImplChoice _ _ cs _ Superclass _):_) <- lookup con scope, all (entails scp) cs
        = let (bindings, needs') = simpl scp needs
           in (Binding var (useForSimpl annot (scope <> scp) implicit con) (annot, con):bindings, needs')
        | otherwise = second ((var, con) :) (simpl scp needs)
      simpl _ [] = ([], [])
      (simplif, stillNeeded') = simpl usable stillNeeded

  let addCtx ((_, con):cons) = TyPi (Implicit con) . addCtx cons
      addCtx [] = id

  let addFns ((var, con):cons) = fun var con . addFns cons
      addFns [] = id

  let addLet ex = mkLet (aliases ++ simplif) ex (annotation ex, getType ex)
      mkLet [] x _ = x
      mkLet xs x t = Let xs x t

  pure (addCtx stillNeeded', addFns stillNeeded' . addLet)

fun :: Var Typed -> Type Typed -> Expr Typed -> Expr Typed
fun v t e = Fun (EvParam (Capture v (annotation e, t))) e (annotation e, TyArr t (getType e))

skolFreeTy :: MonadNamey m => SkolemMotive Typed -> Type Typed -> m (Type Typed, Subst Typed)
skolFreeTy motive ty = do
  vs <- for (Set.toList (ftv ty)) $ \v ->
    (v,) <$> freshSkol motive ty v
  pure (apply (Map.fromList vs) ty, Map.fromList vs)

nameName :: Var Resolved -> T.Text
nameName (TgInternal x) = x
nameName (TgName x _) = x

entails :: ImplicitScope Typed -> Obligation Typed -> Bool
entails _ (Quantifier _) = True
entails scp (Implication c) = not (null (lookup c scp))

useForSimpl :: Ann Resolved -> ImplicitScope Typed -> Implicit Typed -> Type Typed -> Expr Typed
useForSimpl span scope (ImplChoice head oty pre var _ _) ty =
  case unifyPure head ty of
    Nothing -> error "What?"
    Just sub ->
      let wrap (Quantifier (Invisible v _):cs) ex (TyPi (Invisible _ _) rest) =
            wrap cs (ExprWrapper (TypeApp (sub ! v)) ex (annotation ex, rest)) rest
          wrap (Quantifier _:_) _ _ = error "malformed Quantifier"
          wrap (Implication v:cs) ex (TyPi (Implicit _) rest) =
            let v' = apply sub v
                (x:_) = lookup v' scope
             in wrap cs (App ex (useForSimpl span scope x v') (annotation ex, rest)) rest
          wrap [] ex _ = ex
          wrap x _ t = error (displayS (string "badly-typed implicit" <+> shown x <+> pretty t))
      in wrap pre (VarRef var (span, oty)) oty

(!) :: (Show k, Ord k, HasCallStack) => Map.Map k v -> k -> v
m ! k = fromMaybe (error ("Key " ++ show k ++ " not in map")) (Map.lookup k m)

type Need t = (Var t, Type t)
