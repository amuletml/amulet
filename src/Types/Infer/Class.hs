{-# LANGUAGE FlexibleContexts, TupleSections, ScopedTypeVariables,
   ViewPatterns, LambdaCase #-}
module Types.Infer.Class (WrapFlavour(..), inferClass, inferInstance, reduceClassContext) where

import Prelude hiding (lookup)

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Traversable
import Data.Foldable
import Data.Spanned
import Data.Reason
import Data.Maybe
import Data.List (sortOn, partition)
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

import Text.Show.Pretty

import GHC.Stack

inferClass :: forall m. MonadInfer Typed m
           => Toplevel Resolved
           -> m ( [Toplevel Typed]
                , Telescope Typed
                , ClassInfo
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

      (signatures, defaults) = partition (\case { MethodSig{} -> True; DefaultMethod{} -> False }) methods

  local (names %~ focus (one name k <> scope params)) $ do
    -- Infer the types for every method
    (decls, rows) <- fmap unzip . for signatures $ \meth@(MethodSig method ty _) -> do
      method <- pure (TvName method)
      ty <- silence $ -- Any errors will have been caught by the resolveClassKind
        resolveKind (BecauseOf meth) ty
      withHead <- closeOver (BecauseOf meth) $
        TyPi (Implicit classConstraint) ty

      pure ( (method, withHead)
           , (Anon, method, nameName (unTvName method), ty))

    let tele = one name k <> teleFromList decls

        unwind (TyTuple a b) = a:unwind b
        unwind t = pure t

        getContext Nothing = []
        getContext (Just t) = unwind t

    ctx <- traverse (\x -> validContext "class" classAnn x *> checkAgainstKind (BecauseOf clss) x tyConstraint) ctx
    (fold -> scope, rows') <- fmap unzip . for (getContext ctx) $
      \obligation -> do
        impty <- silence $
          closeOver (BecauseOf clss) $
            TyPi (Implicit classConstraint) obligation
        var@(TgName name _) <- genNameWith (classCon' <> T.singleton '$')
        pure ( singleton classAnn Superclass (TvName var) impty
             , (Implicit, TvName var, name, obligation))

    (fold -> defaultMap) <-
      local (classes %~ mappend scope) . local (names %~ focus tele) . for defaults $ \(DefaultMethod (Binding method exp _) _) -> do
      let sig = tele ^. at method . non undefined

      (_, cs) <- listen $
        check exp sig

      (sub, _, deferred) <- condemn $ solve cs

      deferred <- pure (fmap (apply sub) deferred)
      (_, _, cons) <- solveHard (Seq.fromList deferred)

      unless (null cons) $ do
        let (c@(ConImplicit reason _ _ _):_) = reverse cons
        confesses (addBlame reason (UnsatClassCon reason c (BadDefault method sig)))

      pure (Map.singleton (nameName method) exp)

    let inner :: Type Typed
        inner =
          let rs = map (\(_, _, x, y) -> (x, y)) rows ++ map (\(_, _, x, y) -> (x, y)) rows'
           in case rs of
             [(_, x)] -> x
             _ -> TyExactRows rs

    classConTy <- silence $
      closeOver (BecauseOf clss) (TyArr inner classConstraint)

    let tyDecl :: Toplevel Typed
        tyDecl = TypeDecl name params
          [ ArgCon classCon inner (classAnn, classConTy)
          ]

    let mkDecl :: (Type Typed -> TyBinder Typed, Var Typed, T.Text, Type Typed) -> m (Binding Typed)
        mkDecl (f, var, label, theTy) = do
          capture <- TvName <$> genName
          let ty = TyPi (f classConstraint) theTy
          let expr =
                Fun (EvParam
                       (Destructure classCon
                         (Just (Capture capture (classAnn, inner)))
                         (classAnn, classConstraint)))
                 (Access (VarRef capture (classAnn, inner)) label (classAnn, ty))
                 (classAnn, ty)
          ty <- silence $
            closeOver (BecauseOf clss) ty
          pure (Binding var expr (classAnn, ty))

        newtypeClassDecl :: (Type Typed -> TyBinder Typed, Var Typed, T.Text, Type Typed) -> m (Binding Typed)
        newtypeClassDecl (f, var, _, theTy) = do
          capture <- TvName <$> genName
          let ty = TyPi (f classConstraint) theTy
          let expr =
                Fun (EvParam
                       (Destructure classCon
                         (Just (Capture capture (classAnn, inner)))
                         (classAnn, classConstraint)))
                 (VarRef capture (classAnn, inner))
                 (classAnn, ty)
          ty <- silence $
            closeOver (BecauseOf clss) ty
          pure (Binding var expr (classAnn, ty))

    decs <- case rows ++ rows' of
      [one] -> pure <$> newtypeClassDecl one
      rest -> traverse mkDecl rest

    let info = ClassInfo name classConstraint methodMap contextMap classCon classConTy classAnn defaultMap
        methodMap = Map.fromList (map (\(_, n, _, t) -> (n, t)) rows)
        contextMap = Map.fromList (map (\(_, _, l, t) -> (l, t)) rows')

    pure ( tyDecl:map (LetStmt . pure) decs, tele
         , info
         , scope)
inferClass _ = error "not a class"

inferInstance :: forall m. MonadInfer Typed m => Toplevel Resolved -> m (Toplevel Typed, Var Typed, Type Typed)
inferInstance inst@(Instance clss ctx instHead bindings ann) = do
  ClassInfo clss classHead methodSigs classContext classCon classConTy classAnn defaults <-
    view (classDecs . at (TvName clss) . non undefined)

  let classCon' = nameName (unTvName classCon)

  instanceName <- TvName <$> genNameWith (T.pack "$d" <> classCon')
  localInstanceName <- TvName <$> genNameWith (T.pack "$l" <> classCon')

  -- Make sure we have a valid context.
  -- **Note**: Instances with no context are given a tyUnit context so
  -- that the dictionaries can be recursive. We do this instead of using
  -- locally recursive instance methods because that's easier to
  -- desugar.
  ctx <- case ctx of
    Just x -> do
      validContext "instance" ann x
      checkAgainstKind (BecauseOf inst) x tyConstraint
    Nothing -> pure tyUnit

  instHead <- condemn $
    checkAgainstKind (BecauseOf inst) instHead tyConstraint
  globalInsnConTy <- silence $
    closeOver (BecauseOf inst) (TyPi (Implicit ctx) instHead)

  scope <- view classes
  case filter ((/= Superclass) . view implSort) $ lookup instHead scope of
    [] -> pure ()
    (x:_) -> confesses (Overlap instHead (x ^. implSpan) ann)

  (instHead, skolSub) <- skolFreeTy (ByInstanceHead instHead ann) instHead
  (ctx, mappend skolSub -> skolSub) <- skolFreeTy (ByInstanceHead ctx ann) (apply skolSub ctx)

  (mappend skolSub -> sub, _, _) <- solve (pure (ConUnify (BecauseOf inst) undefined classHead instHead))
  localInsnConTy <- silence $
    closeOver (BecauseOf inst) (TyPi (Implicit ctx) instHead)

  (localAssums', instancePattern) <-
    let mkBinds x | x == tyUnit = pure (mempty, PLiteral LiUnit (ann, tyUnit))
        mkBinds (TyTuple a b) = do
          var <- TvName <$> genName
          (scope, pat) <- mkBinds b
          pure (insert ann LocalAssum var a scope, PTuple [Capture var (ann, a), pat] (ann, TyTuple a b))
        mkBinds x = do
          var <- TvName <$> genName
          pure (singleton ann LocalAssum var x, Capture var (ann, x))
    in mkBinds ctx

  let localAssums = insert ann InstSort localInstanceName localInsnConTy localAssums'

  methodSigs <- traverse (closeOver (BecauseOf inst) . apply sub) methodSigs
  classContext <- pure $ fmap (apply sub) classContext
  let methodNames = Map.mapKeys (nameName . unTvName) methodSigs

  (Map.fromList -> methodMap, methods) <- fmap unzip . local (classes %~ mappend localAssums) $
    for bindings $ \case
      bind@(Binding v e an) -> do
        sig <- case Map.lookup (TvName v) methodSigs of
          Just x -> pure x
          Nothing -> confesses (WrongClass bind clss)

        v' <- genNameFrom (nameName v)

        (e, cs) <- listen $ check e sig
        (sub, wrap, deferred) <- condemn $ solve cs

        deferred <- pure (fmap (apply sub) deferred)
        (compose sub -> sub, wrap', cons) <- solveHard (Seq.fromList deferred)

        unless (null cons) $ do
          let (c@(ConImplicit reason _ _ _):_) = reverse cons
          confesses (addBlame reason (UnsatClassCon reason c (InstanceMethod ctx)))

        name <- TvName <$> genName
        let reify an ty var =
              case wrap' Map.! var of
                ExprApp v -> v
                x -> ExprWrapper x
                       (Fun (EvParam (Capture name (an, ty))) (VarRef name (an, ty)) (an, TyArr ty ty))
                       (an, ty)
            mkBind var (VarRef var' _) | var == var' = const []
            mkBind v e = (:[]) . Binding v e
        let addLet (ConImplicit _ _ var ty:cs) ex | an <- annotation ex =
              addLet cs $ mkLet (mkBind var (reify an ty var) (an, ty))
                ex (an, getType ex)
            addLet (_:cs) ex = addLet cs ex
            addLet [] ex = ex
            shove cs (ExprWrapper w e a) = ExprWrapper w (shove cs e) a
            shove cs x = addLet cs x

        pure ( (nameName v, TvName v')
             , Binding (TvName v') (Ascription (solveEx sig sub (wrap <> wrap') (shove deferred e)) sig (an, sig)) (an, sig))
      _ -> error "not possible: non-Binding method"

  let needDefaults = methodNames `Map.difference` methodMap
      definedHere = methodNames `Map.intersection` methodMap
      needed = defaults `Map.intersection` needDefaults
      diff = Map.toList (needDefaults `Map.difference` defaults)
  unless (null diff) $ confesses (UndefinedMethods instHead diff ann)

  scope <- mappend localAssums <$> view classes
  (usedDefaults, defaultMethods) <- fmap unzip . local (classes %~ mappend localAssums) . for (Map.toList needed) $ \(name, expr) -> do
    let ty = methodNames ! name
        an = annotation expr

    (e, cs) <- listen $ check expr ty
    (sub, wrap, deferred) <- condemn $ solve cs

    deferred <- pure (fmap (apply sub) deferred)
    (compose sub -> sub, wrap', cons) <- solveHard (Seq.fromList deferred)

    unless (null cons) $ do
      let (c@(ConImplicit reason _ _ _):_) = reverse cons
      confesses (addBlame reason (UnsatClassCon reason c (InstanceMethod ctx)))

    capture <- TvName <$> genName
    let reify an ty var =
          case wrap' Map.! var of
            ExprApp v -> v
            x -> ExprWrapper x
                   (Fun (EvParam (Capture capture (an, ty))) (VarRef capture (an, ty)) (an, TyArr ty ty))
                   (an, ty)
        mkBind var (VarRef var' _) | var == var' = const []
        mkBind v e = (:[]) . Binding v e
    let addLet (ConImplicit _ _ var ty:cs) ex | an <- annotation ex =
          addLet cs $ mkLet (mkBind var (reify an ty var) (an, ty))
            ex (an, getType ex)
        addLet (_:cs) ex = addLet cs ex
        addLet [] ex = ex
        shove cs (ExprWrapper w e a) = ExprWrapper w (shove cs e) a
        shove cs x = addLet cs x

    var <- TvName <$> genNameFrom name
    let expr = Ascription (solveEx ty sub (wrap <> wrap') (shove deferred e)) ty (an, ty)
        bind = Binding var expr (an, ty)

    pure (Field name (VarRef var (an, ty)) (an, ty), bind)

  (contextFields, cs) <- listen . for (Map.toList classContext) $ \(name, ty) -> do
    var <- TvName <$> genName
    tell (pure (ConImplicit (BecauseOf inst) scope var ty))
    pure (Field name
           (ExprWrapper (WrapVar var)
             (Fun (EvParam (Capture var (ann, ty)))
               (VarRef var (ann, ty)) (ann, TyArr ty ty))
               (ann, ty))
             (ann, ty))

  let methodFields = map (\(name, ty) -> Field name (VarRef (methodMap ! name) (ann, ty)) (ann, ty)) (Map.toList definedHere)
      whatDo = Map.toList (methodNames <> classContext)
      fields = methodFields ++ usedDefaults ++ contextFields

  (solution, needed, unsolved) <- solve cs
  (_, wrapper, unsolved') <-
    reduceClassContext localAssums ann unsolved

  unless (null unsolved') $
    confesses (addBlame (BecauseOf inst) (UnsatClassCon (BecauseOf inst) (head unsolved) (InstanceClassCon classAnn)))

  let appArg (TyPi (Invisible v _) rest) ex =
        case Map.lookup v sub of
          Just x -> appArg rest $ ExprWrapper (TypeApp x) ex (annotation ex, rest)
          Nothing -> appArg rest $ ExprWrapper (TypeApp TyType) ex (annotation ex, rest)
      appArg _ ex = ex

      addArg ty@(TyPi (Invisible v k) rest) ex =
        case Map.lookup v skolSub of
          Just (TySkol s) -> ExprWrapper (TypeLam s (fromMaybe TyType k)) (addArg rest ex) (ann, ty)
          _ ->
            let fakeSkol = Skolem v v ty (ByConstraint ty)
             in ExprWrapper (TypeLam fakeSkol (fromMaybe TyType k)) (addArg rest ex) (ann, ty)
      addArg _ ex = ex

      inside = case whatDo of
        [(_, one)] -> solveEx one solution needed (fields ^. to head . fExpr)
        _ -> solveEx (TyExactRows whatDo) solution needed (Record fields (ann, TyExactRows whatDo))

      fun = addArg globalInsnConTy $
        Let [Binding localInstanceName
              (Fun (EvParam (PType instancePattern ctx (ann, ctx)))
                (wrapper Full (mkLet (methods ++ defaultMethods)
                  (App (appArg classConTy (VarRef classCon (ann, classConTy)))
                    inside
                    (ann, instHead))
                  (ann, instHead)))
                (ann, localInsnConTy))
              (ann, localInsnConTy)]
          (VarRef localInstanceName (ann, localInsnConTy))
          (ann, localInsnConTy)
      bind = Binding instanceName (Ascription fun globalInsnConTy (ann, globalInsnConTy)) (ann, globalInsnConTy)

  pure (LetStmt [bind], instanceName, globalInsnConTy)
inferInstance _ = error "not an instance"

reduceClassContext :: forall m. (MonadInfer Typed m, HasCallStack)
                   => ImplicitScope Typed
                   -> Ann Resolved
                   -> [Constraint Typed]
                   -> m (Type Typed -> Type Typed, WrapFlavour -> Expr Typed -> Expr Typed, [Need Typed])

reduceClassContext _ _ [] = pure (id, const id, mempty)
reduceClassContext extra annot cons = do
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
          let (bindings, needs', scope') = dedup (insert annot LocalAssum var con scope) needs
           in (bindings, (var, con):needs', scope')
      dedup scope [] = ([], [], scope)
      (aliases, stillNeeded, usable) = dedup mempty needs

  let simpl :: ImplicitScope Typed -> [Need Typed] -> ([Binding Typed], [Need Typed])
      simpl scp ((var, con):needs)
        | superclasses <- filter ((== Superclass) . view implSort) $ lookup con scope
        , First (Just implicit) <- foldMap (isUsable scp) superclasses
        = let (bindings, needs') = simpl scp needs
           in (Binding var (useForSimpl annot (scope <> scp) implicit con) (annot, con):bindings, needs')
        | otherwise = second ((var, con) :) (simpl scp needs)
      simpl _ [] = ([], [])
      (simplif, stillNeeded') = simpl (usable <> extra) stillNeeded

      isUsable scp x@(ImplChoice _ _ cs _ _ _) =
        if all (entails scp) cs
           then First (Just x)
           else First Nothing

  let addCtx ((_, con):cons) = TyPi (Implicit con) . addCtx cons
      addCtx [] = id

  let addFns ((var, con):cons) = fun var con . addFns cons
      addFns [] = id

  let addLet ex = mkLet (aliases ++ simplif) ex (annotation ex, getType ex)
      shove (ExprWrapper w e a) = ExprWrapper w (shove e) a
      shove (Fun x@EvParam{} e a) = Fun x (shove e) a
      shove x = addLet x

  let wrap flv =
        addFns stillNeeded' . (case flv of { Full -> shove; _ -> id })

  pure (addCtx stillNeeded', wrap, stillNeeded')

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
entails scp (Implication c) = any isLocal (lookup c scp) where
  isLocal x = x ^. implSort == LocalAssum

useForSimpl :: HasCallStack => Ann Resolved -> ImplicitScope Typed -> Implicit Typed -> Type Typed -> Expr Typed
useForSimpl span scope (ImplChoice head oty pre var _ _) ty =
  case unifyPure head ty of
    Nothing -> error "What?"
    Just sub ->
      let wrap (Quantifier (Invisible v _):cs) ex (TyPi (Invisible _ _) rest) =
            wrap cs (ExprWrapper (TypeApp (sub ! v)) ex (annotation ex, rest)) rest
          wrap (Quantifier _:_) _ _ = error "malformed Quantifier"
          wrap (Implication v:cs) ex (TyPi (Implicit _) rest) =
            let v' = apply sub v
                choices = lookup v' scope
             in case choices of
               [] -> error $ "No choice for entailed implicit " ++ displayS (pretty v') ++ ppShow scope
               (x:_) -> wrap cs (App ex (useForSimpl span scope x v') (annotation ex, rest)) rest
          wrap [] ex _ = ex
          wrap x _ t = error (displayS (string "badly-typed implicit" <+> shown x <+> pretty t))
      in wrap pre (VarRef var (span, oty)) oty

mkLet :: [Binding p] -> Expr p -> Ann p -> Expr p
mkLet [] = const
mkLet xs = Let xs

(!) :: (Show k, Ord k, HasCallStack) => Map.Map k v -> k -> v
m ! k = fromMaybe (error ("Key " ++ show k ++ " not in map")) (Map.lookup k m)

validContext :: MonadChronicles TypeError m => String -> Ann Resolved -> Type Resolved -> m ()
validContext what ann t@(TyApp f _)
  | TyCon{} <- f = pure ()
  | otherwise = validContext "" ann f `catchChronicle` \_ -> confesses (InvalidContext what ann t)

validContext what ann (TyTuple a b) = do
  validContext what ann a
  validContext what ann b

validContext _ _ TyCon{} = pure ()
validContext what ann t@TyPromotedCon{} = confesses (InvalidContext what ann t)
validContext w a t@TyVar{} = confesses (InvalidContext w a t)
validContext w a t@TyArr{} = confesses (InvalidContext w a t)
validContext w a t@(TyPi _ x) =
  validContext w a x `catchChronicle` \_ -> confesses (InvalidContext w a t)
validContext w a t@TyRows{} = confesses (InvalidContext w a t)
validContext w a t@TyExactRows{} = confesses (InvalidContext w a t)
validContext w a t@TyWildcard{} = confesses (InvalidContext w a t)
validContext w a t@TySkol{} = confesses (InvalidContext w a t)
validContext w a t@TyWithConstraints{} = confesses (InvalidContext w a t)
validContext w a t@TyType{} = confesses (InvalidContext w a t)

type Need t = (Var t, Type t)
data WrapFlavour = Thin | Full
