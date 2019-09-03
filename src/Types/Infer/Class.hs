{-# LANGUAGE FlexibleContexts, TupleSections, ScopedTypeVariables,
   ViewPatterns, LambdaCase, TypeFamilies #-}
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
import Data.Graph
import Data.Maybe
import Data.List (sortOn, partition)
import Data.Char

import Control.Monad.State
import Control.Monad.Infer
import Control.Arrow (second)
import Control.Lens

import Syntax.Implicits
import Syntax.Transform
import Syntax.Builtin
import Syntax.Boolean
import Syntax.Subst
import Syntax.Types
import Syntax.Let
import Syntax.Var
import Syntax

import {-# SOURCE #-} Types.Infer
import Types.Infer.Builtin
import Types.Kinds
import Types.Unify

import Text.Pretty.Semantic

import Text.Show.Pretty hiding (reify)

import GHC.Stack

inferClass :: forall m. MonadInfer Typed m
           => Toplevel Desugared
           -> m ( [Toplevel Typed]
                , Telescope Typed
                , ClassInfo
                , ImplicitScope Typed )
inferClass clss@(Class name _ ctx _ methods classAnn) = do
  let toVar :: TyConArg Typed -> Type Typed
      toVar (TyVarArg v) = TyVar v
      toVar (TyAnnArg v _) = TyVar v

  let classCon' =
        T.cons (toUpper (T.head (nameName name)))
          (T.tail (nameName name))
  classCon <- genNameFrom classCon'

  (k, params) <- resolveClassKind clss
  let classConstraint = foldl TyApp (TyCon name) (map toVar params)
      scope (TyAnnArg v k:cs) = one v k <> scope cs
      scope (_:cs) = scope cs
      scope [] = mempty

      (signatures, defaults) = partition (\case { MethodSig{} -> True; DefaultMethod{} -> False }) methods

  let forallVars = getForallVars k
      getForallVars (TyForall v _ t) = Set.singleton v <> getForallVars t
      getForallVars _ = mempty

  let declaredVars =
         foldMap (\case
           TyVarArg v -> Set.singleton v
           TyAnnArg v _ -> Set.singleton v) params

      vars = declaredVars <> forallVars

  let addForallVars (TyForall v k t) ty
        | v `Set.member` forallVars && v `Set.member` ftv ty = TyPi (Invisible v k Infer) (addForallVars t ty)
        | otherwise = addForallVars t ty
      addForallVars _ ty = ty

  local (names %~ focus (one name k <> scope params)) $ do
    -- Infer the types for every method
    (decls, rows) <- fmap unzip . for signatures $ \meth@(MethodSig method ty _) -> do
      checkWildcard meth ty

      kinded <- silence . fmap (addForallVars k) $ -- Any errors will have been caught by the resolveClassKind
        resolveKind (BecauseOf meth) ty

      -- any free tvs are kind variables
      let tv = Set.toList (ftv kinded `Set.difference` vars)
          ty = foldr (\ v -> TyPi (Invisible v Nothing Infer)) kinded tv

      checkValidMethodTy (BecauseOf meth) method declaredVars kinded

      withHead <- closeOver' vars (BecauseOf meth) $
        TyPi (Implicit classConstraint) ty

      pure ( (method, withHead)
           , (Anon, method, nameName method, ty))

    let tele = one name k <> teleFromList decls

        unwind (TyTuple a b) = a:unwind b
        unwind t = pure t

        getContext Nothing = []
        getContext (Just t) = unwind t

    ctx <- for ctx $ \x -> do
      validContext "class" classAnn x
      checkAgainstKind (BecauseOf clss) x tyConstraint

    (fold -> scope, rows') <- fmap unzip . for (getContext ctx) $
      \obligation -> do
        impty <- silence $
          closeOver' vars (BecauseOf clss) $
            TyPi (Implicit classConstraint) obligation
        ~var@(TgName name _) <- genNameWith (classCon' <> T.singleton '$')
        pure ( singleton classAnn Superclass var impty
             , (Implicit, var, name, obligation))

    (fold -> defaultMap) <-
      local (classes %~ mappend scope)
      . local (names %~ focus tele)
      . for defaults $ \(DefaultMethod (Binding method exp _ _) _) -> do
      let sig = tele ^. at method . non undefined

      (_, cs) <- listen $
        check exp sig

      (sub, _, deferred) <- condemn $ solve cs

      deferred <- pure (fmap (apply sub) deferred)
      (_, _, cons) <- solve (Seq.fromList deferred)

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
      closeOver' vars (BecauseOf clss) (TyArr inner classConstraint)

    let tyDecl :: Toplevel Typed
        tyDecl = TypeDecl Public name params
          (Just [ArgCon Private classCon inner (classAnn, classConTy)])
          (classAnn, undefined)

    let mkDecl :: (Type Typed -> TyBinder Typed, Var Typed, T.Text, Type Typed) -> m (Binding Typed)
        mkDecl (f, var, label, theTy) = do
          capture <- genName
          let ty = TyPi (f classConstraint) theTy
          let expr =
                Fun (EvParam
                       (Destructure classCon
                         (Just (Capture capture (classAnn, inner)))
                         (classAnn, classConstraint)))
                 (Access (VarRef capture (classAnn, inner)) label (classAnn, ty))
                 (classAnn, ty)
          ty <- silence $
            closeOver' vars (BecauseOf clss) ty
          pure (Binding var expr True (classAnn, ty))

        newtypeClassDecl :: (Type Typed -> TyBinder Typed, Var Typed, T.Text, Type Typed) -> m (Binding Typed)
        newtypeClassDecl (f, var, _, theTy) = do
          capture <- genName
          let ty = TyPi (f classConstraint) theTy
          let expr =
                Fun (EvParam
                       (Destructure classCon
                         (Just (Capture capture (classAnn, inner)))
                         (classAnn, classConstraint)))
                 (VarRef capture (classAnn, inner))
                 (classAnn, ty)
          ty <- silence $
            closeOver' vars (BecauseOf clss) ty
          pure (Binding var expr False (classAnn, ty))

    decs <- case rows ++ rows' of
      [one] -> pure <$> newtypeClassDecl one
      rest -> traverse mkDecl rest

    let info =
          ClassInfo name classConstraint methodMap contextMap classCon classConTy classAnn defaultMap
          (makeMinimalFormula methodMap defaultMap)
        methodMap = Map.fromList (map (\(_, n, _, t) -> (n, t)) rows)
        contextMap = Map.fromList (map (\(_, _, l, t) -> (l, t)) rows')

    pure ( tyDecl:map (LetStmt Public . pure) decs, tele
         , info
         , scope)
inferClass _ = error "not a class"

inferInstance :: forall m. MonadInfer Typed m => Toplevel Desugared -> m (Toplevel Typed, Var Typed, Type Typed)
inferInstance inst@(Instance clss ctx instHead bindings ann) = do
  traverse_ (checkWildcard inst) ctx
  checkWildcard inst instHead

  ClassInfo clss classHead methodSigs classContext classCon classConTy classAnn defaults minimal <-
    view (classDecs . at clss . non undefined)

  let classCon' = nameName classCon

  instanceName <- genNameWith (T.pack "$d" <> classCon')

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
  instHead <- expandType instHead

  globalInsnConTy <- silence $
    closeOver (BecauseOf inst) (TyPi (Implicit ctx) instHead)

  (instHead, skolSub) <- skolFreeTy (ByInstanceHead instHead ann) instHead

  scope <- view classes
  let overlapping = filter ((/= Superclass) . view implSort) . filter (applicable instHead scope) $
        lookup instHead scope
  case overlapping of
    [] -> pure ()
    (x:_) -> confesses (Overlap instHead (x ^. implSpan) ann)

  (ctx, mappend skolSub -> skolSub) <- skolFreeTy (ByInstanceHead ctx ann) (apply skolSub ctx)

  (mappend skolSub -> instSub, _, _) <- solve (pure (ConUnify (BecauseOf inst) undefined classHead instHead))
  localInsnConTy <- silence $
    closeOver (BecauseOf inst) (TyPi (Implicit ctx) instHead)

  fullCtx <- genName
  (localAssums', instancePattern) <-
    let mkBinds x | x == tyUnit = pure (mempty, PLiteral LiUnit (ann, tyUnit))
        mkBinds (TyTuple a b) = do
          var <- genName
          (scope, pat) <- mkBinds b
          pure (insert ann LocalAssum var a scope, PTuple [Capture var (ann, a), pat] (ann, TyTuple a b))
        mkBinds x = do
          var <- genName
          pure (singleton ann LocalAssum var x, Capture var (ann, x))
        addFull (as, p) = (as, PAs p fullCtx (ann, ctx))
     in addFull <$> mkBinds ctx

  let localAssums = insert ann InstSort instanceName globalInsnConTy localAssums'

  methodSigs <- traverse (closeOver (BecauseOf inst) . apply instSub) methodSigs
  classContext <- pure $ fmap (apply instSub) classContext
  let methodNames = Map.mapKeys nameName methodSigs
      addStuff = local (classes %~ mappend localAssums) . local (typeVars %~ mappend (Map.keysSet skolSub))

  (Map.fromList -> methodMap, Map.fromList -> methodDef, methods) <- fmap unzip3 . addStuff $
    for bindings $ \case
      bind@(Binding v e _ an) -> do
        sig <- case Map.lookup v methodSigs of
          Just x -> pure x
          Nothing -> confesses (WrongClass bind clss)

        let bindGroupTy = transplantPi globalInsnConTy (TyArr ctx sig)

        v' <- genNameFrom (T.cons '$' (nameName v))

        (e, cs) <- listen $ check e sig
        (sub, wrap, deferred) <- condemn $ solve cs

        deferred <- pure (fmap (apply sub) deferred)
        (compose sub -> sub, wrap', cons) <- solve (Seq.fromList deferred)

        unless (null cons) $ do
          let (c@(ConImplicit reason _ _ _):_) = reverse cons
          confesses (addBlame reason (UnsatClassCon reason c (InstanceMethod ctx)))

        name <- genName
        let shove cs (ExprWrapper w e a) = ExprWrapper w (shove cs e) a
            shove cs x = addLet wrap' name cs x
            fakeExp =
              App (appArg instSub bindGroupTy (VarRef v' (an, bindGroupTy)))
                  (VarRef fullCtx (an, ctx))
                  (an, sig)

        pure ( (nameName v, v')
             , (nameName v, fakeExp)
             , Binding v'
                (addArg skolSub bindGroupTy
                  (Fun (EvParam instancePattern)
                    (Ascription
                      (solveEx mempty sub (wrap <> wrap') (shove deferred e))
                        sig (an, sig))
                  (an, TyArr ctx sig)))
               True
               (an, bindGroupTy))
      _ -> error "not possible: non-Binding method"

  let needDefaults = methodNames `Map.difference` methodMap
      definedHere = methodNames `Map.intersection` methodMap
      needed = defaults `Map.intersection` needDefaults

  case satisfy (`Map.member` definedHere) minimal of
    Sat -> pure ()
    Unsat xs -> confesses (UndefinedMethods instHead xs ann)


  scope <- mappend localAssums' <$> view classes
  (usedDefaults, defaultMethods) <- fmap unzip
    . local (classes %~ mappend localAssums)
    . for (Map.toList needed) $ \(name, expr) -> do
    let ty = methodNames ! name
        bindGroupTy = transplantPi globalInsnConTy (TyArr ctx ty)
        an = annotation expr

    (e, cs) <- listen $ check expr ty
    (sub, wrap, deferred) <- condemn $ solve cs

    deferred <- pure (fmap (apply sub) deferred)
    (compose sub -> sub, wrap', cons) <- solve (Seq.fromList deferred)

    unless (null cons) $ do
      let (c@(ConImplicit reason _ _ _):_) = reverse cons
      confesses (addBlame reason (UnsatClassCon reason c (InstanceMethod ctx)))

    capture <- genName
    let shove cs (ExprWrapper w e a) = ExprWrapper w (shove cs e) a
        shove cs x = addLet wrap' capture cs x

    var <- genNameFrom name
    body <- expandEta ty $ solveEx mempty sub (wrap <> wrap') (shove deferred e)
    let bind = Binding var (addArg skolSub bindGroupTy fun) False (an, bindGroupTy)
        fun = Fun (EvParam instancePattern)
                body
                (an, TyArr ctx ty)
        fakeExp =
          App (appArg instSub bindGroupTy (VarRef var (an, bindGroupTy)))
              (VarRef fullCtx (an, ctx))
              (an, ty)

    pure (Field name fakeExp (an, ty), bind)

  (contextFields, cs) <- listen . for (Map.toList classContext) $ \(name, ty) -> do
    var <- genName
    tell (pure (ConImplicit (BecauseOf inst) scope var ty))
    pure (Field name
           (ExprWrapper (WrapVar var)
             (Fun (EvParam (PType (Capture var (ann, ty)) ty (ann, ty)))
               (VarRef var (ann, ty)) (ann, TyArr ty ty))
               (ann, ty))
             (ann, ty))

  let methodFields = flip map (Map.toList definedHere) $ \(name, ty) ->
        Field name (methodDef ! name) (ann, ty)
      whatDo = Map.toList (methodNames <> classContext)
      fields = methodFields ++ usedDefaults ++ contextFields

  (solution, needed, unsolved) <- solve cs

  unless (null unsolved) $
    confesses (addBlame (BecauseOf inst)
      (UnsatClassCon (BecauseOf inst) (head unsolved) (InstanceClassCon classAnn)))

  let inside = case whatDo of
        [(_, _)] -> solveEx mempty solution needed (fields ^. to head . fExpr)
        _ -> solveEx mempty solution needed (Record fields (ann, TyExactRows whatDo))

      fun = addArg skolSub globalInsnConTy $
              Fun (EvParam (PType instancePattern ctx (ann, ctx)))
                  (App (appArg instSub classConTy (VarRef classCon (ann, classConTy)))
                    inside
                    (ann, instHead))
                (ann, localInsnConTy)
      bind = Binding instanceName (Ascription fun globalInsnConTy (ann, globalInsnConTy)) True (ann, globalInsnConTy)

  pure (LetStmt Public (methods ++ defaultMethods ++ [bind]), instanceName, globalInsnConTy)
inferInstance _ = error "not an instance"

addArg :: Map.Map (Var Typed) (Type Typed) -> Type Typed -> Expr Typed -> Expr Typed
addArg skolSub ty@(TyPi (Invisible v k req) rest) ex =
  case Map.lookup v skolSub of
    Just (TySkol s) ->
      ExprWrapper (TypeLam s (fromMaybe TyType k))
        (addArg skolSub rest ex)
        (annotation ex, TyPi (Invisible v k req) (getType ex))
    _ ->
      let fakeSkol = Skolem v v ty (ByConstraint ty)
       in ExprWrapper (TypeLam fakeSkol (fromMaybe TyType k)) (addArg skolSub rest ex)
            (annotation ex, TyPi (Invisible v k req) (getType ex))
addArg _ _ ex = ex

appArg :: Map.Map (Var Typed) (Type Typed) -> Type Typed -> Expr Typed -> Expr Typed
appArg sub (TyPi (Invisible v _ _) rest) ex =
  case Map.lookup v sub of
    Just x -> appArg sub rest $ ExprWrapper (TypeApp x) ex (annotation ex, rest)
    Nothing -> appArg sub rest $ ExprWrapper (TypeApp TyType) ex (annotation ex, rest)
appArg _ _ ex = ex

transplantPi :: Type Typed -> Type Typed -> Type Typed
transplantPi (TyPi b@Invisible{} rest) ty = TyPi b (transplantPi rest ty)
transplantPi _ t = t

reduceClassContext :: forall m. (MonadInfer Typed m, HasCallStack)
                   => ImplicitScope Typed
                   -> Ann Desugared
                   -> [Constraint Typed]
                   -> m (Type Typed -> Type Typed, WrapFlavour -> Expr Typed -> Expr Typed, [Need Typed])

reduceClassContext _ _ [] = pure (id, const id, mempty)
reduceClassContext extra annot cons = do
  scope <- view classes
  let needed sub (ConImplicit r _ var con:cs) = do
        (con, sub') <- skolFreeTy (ByConstraint con) (apply sub con)
        ((var, con, r):) <$> needed (sub `compose` sub') cs
      needed sub (_:cs) = needed sub cs
      needed _ [] = pure []

  needs :: [Need Typed] <- sortOn (view _1) <$> needed mempty cons

  -- First, deduplicate the constraints eliminating any redundancy
  let dedup :: ImplicitScope Typed -> [Need Typed] -> ([Binding Typed], [Need Typed], ImplicitScope Typed)
      dedup scope ((var, con, r):needs)
        | [ImplChoice _ t [] v _ _] <- lookup con scope =
          let (bindings, needs', scope') = dedup scope needs
           in if var == v
             then (bindings, needs', scope')
             else (Binding var (VarRef v (annot, t)) False (annot, t):bindings, needs', scope')
        | otherwise =
          let (bindings, needs', scope') = dedup (insert annot LocalAssum var con scope) needs
           in (bindings, (var, con, r):needs', scope')
      dedup scope [] = ([], [], scope)
      (aliases, stillNeeded, usable) = dedup mempty needs

      simpl :: ImplicitScope Typed -> [Need Typed] -> ([Binding Typed], [Need Typed])
      simpl scp ((var, con, why):needs)
        | superclasses <- filter ((== Superclass) . view implSort) $ lookup con scope
        , First (Just implicit) <- foldMap (isUsable scp) superclasses
        = let (bindings, needs') = simpl scp needs
           in (Binding var (useForSimpl annot scp implicit con) False (annot, con):bindings, needs')
        | otherwise = second ((var, con, why) :) (simpl scp needs)
      simpl _ [] = ([], [])
      (simplif, stillNeeded') = simpl (usable <> extra) stillNeeded

      isUsable scp x@(ImplChoice _ _ cs _ _ _) =
        if all (entails scp) cs
           then First (Just x)
           else First Nothing

      addCtx' ((_, con, _):cons) = TyPi (Implicit (deSkolFreeTy con)) . addCtx cons
      addCtx' [] = id

      addCtx ctx (TyPi x@Invisible{} k) = TyPi x (addCtx ctx k)
      addCtx ctx ty = addCtx' ctx ty

      addFns ((var, con, _):cons) = fun var con . addFns cons
      addFns [] = id

      shoveFn cs (ExprWrapper w e a) = ExprWrapper w (shoveFn cs e) a
      shoveFn cs e = addFns cs e

      addLet ex = mkLet (aliases ++ simplif) ex (annotation ex, getType ex)
      shove (ExprWrapper w e a) = ExprWrapper w (shove e) a
      shove (Fun x@EvParam{} e a) = Fun x (shove e) a
      shove x = addLet x

      wrap flv =
        shoveFn stillNeeded' . (case flv of { Full -> shove; _ -> id })

  for_ stillNeeded $ \(var, ty, reason) ->
    when (tooConcrete ty) $
      confesses (addBlame reason (UnsatClassCon reason (ConImplicit reason scope var ty) (TooConcrete ty)))

  pure (addCtx stillNeeded', wrap, stillNeeded')

addLet :: Map.Map (Var Typed) (Wrapper Typed) -> Var Typed -> [Constraint Typed] -> Expr Typed -> Expr Typed
addLet wrap' name (ConImplicit _ _ var ty:cs) ex | an <- annotation ex =
  addLet wrap' name cs $
    mkLet (mkBind var (reify wrap' name an ty var) (an, ty))
      ex (an, getType ex)

addLet wrap' name (_:cs) ex = addLet wrap' name cs ex
addLet _ _ [] ex = ex

mkBind :: Var Typed -> Expr Typed -> Ann Typed -> [Binding Typed]
mkBind var (VarRef var' _) | var == var' = const []
mkBind v e = (:[]) . Binding v e False

reify :: Map.Map (Var Typed) (Wrapper Typed) -> Var Typed -> Ann Resolved -> Type Typed -> Var Typed -> Expr Typed
reify wrap' name an ty var =
  case wrap' Map.! var of
    ExprApp v -> v
    x -> ExprWrapper x
           (Fun (EvParam (Capture name (an, ty))) (VarRef name (an, ty)) (an, TyArr ty ty))
           (an, ty)

fun :: Var Typed -> Type Typed -> Expr Typed -> Expr Typed
fun v t e = ExprWrapper (TypeAsc ty) (Fun (EvParam (Capture v (an, t))) e (an, ty)) (an, ty) where
  ty = TyArr t (getType e)
  an = annotation e

skolFreeTy :: MonadNamey m => SkolemMotive Typed -> Type Typed -> m (Type Typed, Subst Typed)
skolFreeTy motive ty = do
  vs <- for (Set.toList (ftv ty)) $ \v ->
    (v,) <$> freshSkol motive ty v
  pure (apply (Map.fromList vs) ty, Map.fromList vs)

deSkolFreeTy :: Type Typed -> Type Typed
deSkolFreeTy = transformType go where
  go (TySkol v) = TyVar (v ^. skolVar)
  go x = x

nameName :: Var Desugared -> T.Text
nameName (TgInternal x) = x
nameName (TgName x _) = x

entails :: ImplicitScope Typed -> Obligation Typed -> Bool
entails _ (Quantifier _) = True
entails scp (Implication c) = any isLocal (lookup c scp) where
  isLocal x = x ^. implSort == LocalAssum

useForSimpl :: HasCallStack => Ann Desugared -> ImplicitScope Typed -> Implicit Typed -> Type Typed -> Expr Typed
useForSimpl span scope (ImplChoice head oty pre var _ _) ty =
  case unifyPure head ty of
    Nothing -> error "What?"
    Just sub ->
      let wrap (Quantifier (Invisible v _ _):cs) ex (TyPi Invisible{} rest) =
            wrap cs (ExprWrapper (TypeApp (Map.findWithDefault TyType v sub)) ex (annotation ex, rest)) rest
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

validContext :: MonadChronicles TypeError m => String -> Ann Desugared -> Type Desugared -> m ()
validContext what ann t@(TyApp f _)
  | TyCon{} <- f = pure ()
  | otherwise = validContext "" ann f `catchChronicle` \_ -> confesses (InvalidContext what ann t)

validContext what ann (TyTuple a b) = do
  validContext what ann a
  validContext what ann b

validContext what ann (TyOperator a _ b) = do
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
validContext w a (TyParens t) = validContext w a t

tooConcrete :: Type Typed -> Bool
tooConcrete (spine -> (x:xs)) = isIt x --> any isIt xs where
  isIt TyCon{} = True
  isIt _ = False
  x --> y = not x || y

tooConcrete _ = False


type Need t = (Var t, Type t, SomeReason)
data WrapFlavour = Thin | Full

-- | Build a boolean formula (in CNF) representing the smallest set of
-- methods nescessary for an implementation of the class not to diverge
-- indirectly.
makeMinimalFormula :: Map.Map (Var Resolved) (Type Typed)
                   -> Map.Map T.Text (Expr Desugared)
                   -> Formula T.Text
makeMinimalFormula signatures' defaults = mkAnd (sccs' ++ noDefaults) where
  signatures = Map.mapKeys nameName signatures'

  -- Methods without a default must be implemented
  noDefaults = map Var (Map.keys (signatures `Map.difference` defaults))

  methodRefs e =
    Set.map nameName (freeIn e) `Set.intersection` Map.keysSet signatures

  sccs :: [SCC T.Text]
  sccs = stronglyConnComp (map build (Map.toList defaults))

  sccs' :: [Formula T.Text]
  sccs' = flip map sccs $ \case
    AcyclicSCC _ -> mkAnd [] -- A method with no other references is a usable default by itself.
    CyclicSCC xs -> mkOr (map Var xs)
      -- If you have a cycle f → g → f, you can implement either f (and
      -- use the default g) or g (and use the default f) to
      -- break the cycle.

  build (x, e) = (x, x, Set.toList (x `Set.delete` methodRefs e))

expandEta :: MonadNamey m => Type Typed -> Expr Typed -> m (Expr Typed)
expandEta ty ex = go ty ex where
  an = annotation ex

  go (TyPi (Anon a) b) ex = do
    x <- genName
    inside <- go b (App ex (VarRef x (an, a)) (an, b))
    pure $
      Fun (EvParam (Capture x (an, a)))
        inside (an, TyArr a b)
  go (TyPi (Implicit a) b) ex = do
    x <- genName
    inside <- go b (App ex (VarRef x (an, a)) (an, b))
    pure $
      Fun (EvParam (Capture x (an, a)))
        inside (an, TyArr a b)
  go (TyPi (Invisible v (fromMaybe TyType -> k) req) b) ex = do
    ~x@(TySkol sk) <- freshSkol undefined k v
    inside <- go b (ExprWrapper (TypeApp x) ex (an, b))
    pure $
      ExprWrapper (TypeLam sk k)
        inside (an, TyPi (Invisible v (Just k) req) b)
  go _ e = pure e

checkValidMethodTy :: MonadInfer Typed m
                   => SomeReason
                   -> Var Typed
                   -> Set.Set (Var Typed)
                   -> Type Typed
                   -> m ()
checkValidMethodTy reason method vars ty = unless (Set.null diff) err where
  diff = vars `Set.difference` ftv ty
  err = dictates (addBlame reason (AmbiguousMethodTy method ty diff))
