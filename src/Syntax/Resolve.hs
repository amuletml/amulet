{-# LANGUAGE FlexibleContexts
  , ConstraintKinds
  , OverloadedStrings
  , MultiParamTypeClasses
  , TupleSections
  , TypeFamilies
  , ScopedTypeVariables
  , LambdaCase #-}

{- | The resolver is the first step after parsing. It performs several key
   actions:

    * Determines what variable every identifier is pointing to, including
      a module's variables and handling ambiguous variables.

    * Handles module definitions and @open@s.

    * Prohibit some dubious constructs, such as patterns which bind the
      same identifier multiple times.

    * Reorganise binary operators, taking precedence and associativity
      into account (the parser ignores these intentionally).
-}
module Syntax.Resolve
  ( resolveProgram
  , ResolveError(..)
  , ResolveResult(..)
  , VarKind(..)
  ) where

import Control.Lens hiding (Lazy, Context)
import Control.Monad.Chronicles
import Control.Monad.Reader
import Control.Applicative
import Control.Monad.Namey

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Traversable
import Data.Foldable
import Data.Function
import Data.Functor
import Data.Reason
import Data.Triple
import Data.Maybe
import Data.These
import Data.List

import Syntax.Resolve.Import
import Syntax.Resolve.Scope
import Syntax.Resolve.Error
import Syntax.Subst
import Syntax

import Parser.Unicode

data ResolveResult = ResolveResult
  { program :: [Toplevel Resolved] -- ^ The resolved program
  -- | The exported signature, which other modules may import
  , exposed :: Signature
  -- | The current resolver state, suitable for use within REPL.
  , state :: Signature
  }

type MonadResolve m = ( MonadChronicles ResolveError m
                      , MonadReader Context m
                      , MonadImport m
                      , MonadNamey m )

-- | Resolve a program within a given 'Scope' and 'ModuleScope'
resolveProgram :: (MonadNamey m, MonadImport m)
               => Signature -- ^ The scope in which to resolve this program
               -- ^ The current module scope. We return an updated
               -- version of this if we declare or extend any modules.
               -> [Toplevel Parsed] -- ^ The program to resolve
               -> m (Either [ResolveError] ResolveResult)
               -- ^ The resolved program or a list of resolution errors
resolveProgram sc ts
  = (these (Left . toList) (\(s, exposed, inner) -> Right (ResolveResult s exposed inner)) (\x _ -> Left (toList x))<$>)
  . runChronicleT . flip runReaderT (Context sc mempty)
  $ reTops ts mempty

-- | Resolve the whole program
reTops :: MonadResolve m
       => [Toplevel Parsed] -> Signature
       -> m ([Toplevel Resolved], Signature, Signature)
reTops [] sig = views scope ([], sig,)

reTops (LetStmt am bs:rest) sig = do
  (bs', vs, ts) <- unzip3 <$> traverse reBinding bs
  reTopsWith am rest sig (withVals (concat vs)) $ extendTyvars (concat ts) $
    LetStmt am <$> traverse (uncurry (flip (<$>) . reExpr . view bindBody)) (zip bs bs')

reTops (r@(ForeignVal am v t ty a):rest) sig = do
  v' <- tagVar v
  reTopsWith am rest sig (withVal v v') $
    ForeignVal am
          <$> lookupEx v `catchJunk` r
          <*> pure t
          <*> reType r (wrap ty)
          <*> pure a

  where wrap x = foldr (TyPi . flip (flip Invisible Nothing) Spec) x (toList (ftv x))

reTops (d@(TySymDecl am t vs ty ann):ts) sig = do
  t' <- tagVar t
  (vs', sc) <- resolveTele d vs
  decl <- extendTyvars sc $
    TySymDecl am t' vs' <$> reType d ty <*> pure ann
  reTopsWith am ts sig (withTy t t') (pure decl)

reTops (d@(TypeFunDecl am tau args kindsig eqs ann):rest) sig = do
  tau' <- tagVar tau
  (args, _) <- resolveTele d args
  reTopsWith am rest sig (withTy tau tau') $ do
    eqs <- for eqs $ \clause@(TyFunClause lhs@(TyApps t xs) rhs ann) -> do
      when (t /= TyCon tau) $
        confesses (ArisingFrom (TFClauseWrongHead t tau) (BecauseOf clause))
      when (length xs /= length args) $
        confesses (ArisingFrom (TFClauseWrongArity (length xs) (length args)) (BecauseOf clause))

      let fv = Set.toList (ftv lhs)
      fv' <- traverse tagVar fv
      extendTyvars (zip fv fv') $
        (\x y -> TyFunClause x y ann) <$> reType clause lhs <*> reType clause rhs

    kindsig <- traverse (reType d) kindsig
    pure $ TypeFunDecl am tau' args kindsig eqs ann


reTops (d@(TypeDecl am t vs cs ann):rest) sig = do
  t'  <- tagVar t
  (vs', sc) <- resolveTele d vs
  let c = maybe [] (map extractCons) cs
  c' <- traverse tagVar c
  reTopsWith am rest sig (withTy t t' . withVals (zip c c')) $
    TypeDecl am t' vs'
      <$> maybe (pure Nothing) (fmap Just . traverse (resolveCons sc) . zip c') cs
      <*> pure ann

  where resolveCons _  (v', UnitCon ac _ a) = pure $ UnitCon ac v' a
        resolveCons vs (v', r@(ArgCon ac _ t a)) = ArgCon ac v' <$> extendTyvars vs (reType r t) <*> pure a
        resolveCons _  (v', r@(GadtCon ac _ t a)) = do
          let fvs = toList (ftv t)
          fresh <- traverse tagVar fvs
          t' <- extendTyvars (zip fvs fresh) (reType r t)
          pure (GadtCon ac v' t' a)

        extractCons (UnitCon _ v _) = v
        extractCons (ArgCon _ v _ _) = v
        extractCons (GadtCon _ v _ _) = v

reTops (r@(Open mod):rest) sig = do
  (mod', sig') <- retcons (wrapError r) $ reModule mod
  case sig' of
    Nothing -> confess empty
    Just sig' -> local (scope %~ (<>sig')) $ first3 (Open mod':) <$> reTops rest sig

reTops (r@(Include mod):rest) sig = do
  (mod', sig') <- retcons (wrapError r) $ reModule mod
  case sig' of
    Nothing -> confess empty
    Just sig' -> local (scope %~ (<>sig')) $ do
      (prog, siga, sigb) <- reTops rest sig
      pure (Include mod':prog, siga <> sig', sigb)

reTops (r@(Module am name mod):rest) sig = do
  name' <- tagVar name
  (mod', sig') <- retcons (wrapError r) $ reModule mod
  reTopsWith am rest sig (withMod name name' sig') $ pure (Module am name' mod')

reTops (t@(Class name am ctx tvs fds ms ann):rest) sig = do
  name' <- tagVar name
  (tvs', tvss) <- resolveTele t tvs

  (ctx', fds', (ms', vs')) <- local (scope %~ withTy name name') $ do
    tyfuns <- fmap concat . for ms $ \case
      AssocType name _ _ _ -> (:[]) . (name,) <$> tagVar name
      _ -> pure []

    extendTyvars tvss . local (scope %~ withTys tyfuns) $
      (,,) <$> traverse (reType t) ctx
           <*> traverse reFd fds
           <*> reClassItem (map fst tvss) ms

  let (vars, types) = partition ((\case { AssocType{} -> False; _ -> True}) . snd) (zip vs' ms)

  reTopsWith am rest sig (withVals (map fst vars) . withTys ((name, name') : map fst types)) $ do
    ms'' <- extendTyvars tvss $ sequence ms'
    pure $ Class name' am ctx' tvs' fds' ms'' ann

  where
    reClassItem tvs' (m@(MethodSig name ty an):rest) = do
      (ra, rb) <- reClassItem tvs' rest
      name' <- tagVar name
      pure ( (MethodSig name' <$> reType m (wrap tvs' ty) <*> pure an):ra
           , (name, name'):rb )
    reClassItem tvs' (m@(AssocType name args ty an):rest) = do
      name' <- lookupTy name
      (ra, rb) <- reClassItem tvs' rest
      (tele, _) <- resolveTele m args
      pure ( (AssocType name' tele <$> reType m (wrap tvs' ty) <*> pure an):ra
           , (name, name'):rb )
    reClassItem tvs' (DefaultMethod b an:rest) = do
      (ra, rb) <- reClassItem tvs' rest
      pure ( (DefaultMethod <$> (fmap unMethodImpl . fst =<< reMethod (MethodImpl b)) <*> pure an):ra
           , rb )
    reClassItem _ [] = pure ([], [])

    unMethodImpl (MethodImpl x) = x
    unMethodImpl _ = undefined

    wrap tvs' x = foldr (TyPi . flip (flip Invisible Nothing) Spec) x (ftv x `Set.difference` Set.fromList tvs')
    reFd fd@(Fundep f t a) = Fundep <$> traverse tv f <*> traverse tv t <*> pure a where
      tv x = lookupTyvar x `catchJunk` fd


reTops (t@(Instance cls ctx head ms ann):rest) sig = do
  cls' <- lookupTy cls `catchJunk` t

  let fvs = toList (foldMap ftv ctx <> ftv head)
  fvs' <- traverse tagVar fvs

  t' <- extendTyvars (zip fvs fvs') $ do
    ctx' <- traverse (reType t) ctx
    head' <- reType t head

    (ms', vs) <- unzip <$> traverse reMethod ms
    ms'' <- extendVals (concat vs) (sequence ms')

    pure (Instance cls' ctx' head' ms'' ann)

  first3 (t':) <$> reTops rest sig

reTopsWith :: MonadResolve m
           => TopAccess -> [Toplevel Parsed] -> Signature
           -> (Signature -> Signature)
           -> m (Toplevel Resolved)
           -> m ([Toplevel Resolved], Signature, Signature)
reTopsWith am ts sig extend t = do
  let sig' = case am of
        Public -> extend sig
        Private -> sig
  local (scope %~ extend) $ do
    t' <- t
    first3 (t':) <$> reTops ts sig'

-- | Resolve a module term.
reModule :: MonadResolve m
         => ModuleTerm Parsed
         -> m (ModuleTerm Resolved, Maybe Signature)
reModule (ModStruct bod an) = do
  res <- recover Nothing $ Just <$> reTops bod mempty
  pure $ case res of
    Nothing -> (ModStruct [] an, Nothing)
    Just (bod', sig, _) -> (ModStruct bod' an, Just sig)
reModule (ModRef ref an) = do
  (ref', sig) <- recover (junkVar, Nothing)
               $ view scope >>= lookupIn (^.modules) ref VarModule
  pure (ModRef ref' an, sig)
reModule r@(ModLoad path a) = do
  result <- importModule a path
  (var, sig) <- case result of
    Imported var sig -> pure (var, Just sig)
    Errored -> do
      -- Mark us as having failed resolution, but don't print an error for this
      -- module.
      dictate mempty
      pure (junkVar, Nothing)
    ImportCycle loop -> do
      dictates (wrapError r (ImportLoop loop))
      pure (junkVar, Nothing)
    NotFound -> do
      dictates (wrapError r (UnresolvedImport path))
      pure (junkVar, Nothing)

  -- Replace this with a reference so we don't have to care later on
  -- about this. Bit ugly, but I'll survive
  pure (ModRef var a, sig)

resolveTele :: (MonadResolve m, Reasonable f p)
            => f p -> [TyConArg Parsed] -> m ([TyConArg Resolved], [(Var Parsed, Var Resolved)])
resolveTele r (TyVarArg v:as) = do
  v' <- tagVar v
  (as, vs) <- resolveTele r as
  pure (TyVarArg v':as, (v, v'):vs)
resolveTele r (TyAnnArg v k:as) = do
  v' <- tagVar v
  ((as, vs), k) <-
    (,) <$> resolveTele r as <*> reType r k
  pure (TyAnnArg v' k:as, (v, v'):vs)
resolveTele _ [] = pure ([], [])

reExpr :: MonadResolve m => Expr Parsed -> m (Expr Resolved)
reExpr r@(VarRef v a) = flip VarRef a <$> (lookupEx v `catchJunk` r)

reExpr (Let bs c a) = do
  (bs', vs, ts) <- unzip3 <$> traverse reBinding bs
  extendTyvars (concat ts) . extendVals (concat vs) $
    Let <$> traverse (uncurry (flip (<$>) . reExpr . view bindBody)) (zip bs bs')
        <*> reExpr c
        <*> pure a
reExpr (If c t b a) = If <$> reExpr c <*> reExpr t <*> reExpr b <*> pure a
reExpr (App f p a) = App <$> reExpr f <*> reExpr p <*> pure a
reExpr (Fun p e a) = do
  let reWholePattern' (PatParam p) = do
        (p', vs, ts) <- reWholePattern p
        pure (PatParam p', vs, ts)
      reWholePattern' _ = error "EvParam resolve"
  (p', vs, ts) <- reWholePattern' p
  extendTyvars ts . extendVals vs $ Fun p' <$> reExpr e <*> pure a

reExpr r@(Begin [] a) = dictates (wrapError r EmptyBegin) $> junkExpr a
reExpr (Begin es a) = Begin <$> traverse reExpr es <*> pure a

reExpr (Literal l a) = pure (Literal l a)

reExpr (Match e ps a) = do
  e' <- reExpr e
  ps' <- traverse reArm ps
  pure (Match e' ps' a)

reExpr (Function ps a) = flip Function a <$> traverse reArm ps

reExpr (BinOp l o r a) = BinOp <$> reExpr l <*> reExpr o <*> reExpr r <*> pure a
reExpr (Hole v a) = Hole <$> tagVar v <*> pure a
reExpr r@(Ascription e t a) = do
  t <- reType r t
  let boundByT (TyPi (Invisible v@(TgName p _) _ _) t) = (Name p, v):boundByT t
      boundByT _ = []
  Ascription <$> extendTyvars (boundByT t) (reExpr e) <*> pure t <*> pure a
reExpr e@(Record fs a) = do
  let ls = map (view fName) fs
      dupes = mapMaybe (listToMaybe . tail) . group . sort $ ls
  traverse_ (dictates . NonLinearRecord e) dupes
  Record <$> traverse reField fs <*> pure a
reExpr ex@(RecordExt e fs a) = do
  let ls = map (view fName) fs
      dupes = mapMaybe (listToMaybe . tail) . group . sort $ ls
  traverse_ (dictates . NonLinearRecord ex) dupes
  RecordExt <$> reExpr e <*> traverse reField fs <*> pure a

reExpr (Access e t a) = Access <$> reExpr e <*> pure t <*> pure a
reExpr (LeftSection o r a) = LeftSection <$> reExpr o <*> reExpr r <*> pure a
reExpr (RightSection l o a) = RightSection <$> reExpr l <*> reExpr o <*> pure a
reExpr (BothSection o a) = BothSection <$> reExpr o <*> pure a
reExpr (AccessSection t a) = pure (AccessSection t a)
reExpr (Parens e a) = flip Parens a <$> reExpr e

reExpr (Tuple es a) = Tuple <$> traverse reExpr es <*> pure a
reExpr (ListExp es a) = ListExp <$> traverse reExpr es <*> pure a
reExpr (TupleSection es a) = TupleSection <$> traverse (traverse reExpr) es <*> pure a

reExpr r@(OpenIn m e a) = retcons (wrapError r) $ do
  -- Disable structs in local lets - they may bind variables, and we don't
  -- currently support that.
  case m of
    ModStruct{} -> dictates (wrapError r LetOpenStruct)
    _ -> pure ()

  (m', sig) <- reModule m
  case sig of
    Nothing -> pure $ OpenIn m' (junkExpr a) a
    Just sig -> OpenIn m' <$> local (scope %~ (<>sig)) (reExpr e) <*> pure a

reExpr (Lazy e a) = Lazy <$> reExpr e <*> pure a
reExpr (Vta e t a) = Vta <$> reExpr e <*> reType e t <*> pure a

reExpr (ListComp e qs a) =
  let go (CompGuard e:qs) acc = do
        e <- reExpr e
        go qs (CompGuard e:acc)
      go (CompGen b e an:qs) acc = do
        e <- reExpr e
        (b, es, ts) <- reWholePattern b
        extendTyvars ts . extendVals es $
          go qs (CompGen b e an:acc)
      go (CompLet bs an:qs) acc =do
        (bs', vs, ts) <- unzip3 <$> traverse reBinding bs
        extendTyvars (concat ts) . extendVals (concat vs) $ do
          bs <- traverse (uncurry (flip (<$>) . reExpr . view bindBody)) (zip bs bs')
          go qs (CompLet bs an:acc)
      go [] acc = ListComp <$> reExpr e <*> pure (reverse acc) <*> pure a
  in go qs []

reExpr r@(Idiom vp va es a) = Idiom <$> lookupEx' vp <*> lookupEx' va <*> traverse reExpr es <*> pure a where
  lookupEx' v = lookupEx v `catchJunk` r

reExpr (DoExpr var qs a) =
  let go (CompGuard e:qs) acc flag = do
        e <- reExpr e
        go qs (CompGuard e:acc) flag
      go [r@CompGen{}] _ _ = confesses (ArisingFrom LastStmtNotExpr (BecauseOf r))
      go [r@CompLet{}] _ _ = confesses (ArisingFrom LastStmtNotExpr (BecauseOf r))
      go (r@(CompGen b e an):qs) acc flag = do
        e <- reExpr e
        (b, es, ts) <- reWholePattern b
        extendTyvars ts . extendVals es $
          go qs (CompGen b e an:acc) (flag <|> Just r)
      go (CompLet bs an:qs) acc flag = do
        (bs', vs, ts) <- unzip3 <$> traverse reBinding bs
        extendTyvars (concat ts) . extendVals (concat vs) $ do
          bs <- traverse (uncurry (flip (<$>) . reExpr . view bindBody)) (zip bs bs')
          go qs (CompLet bs an:acc) flag
      go [] acc flag = do
        var <-
          case flag of
            Just r -> lookupEx var `catchJunk` r
            Nothing -> pure undefined
        pure $ DoExpr var (reverse acc) a
  in go qs [] Nothing

reExpr ExprWrapper{} = error "resolve cast"

reField :: MonadResolve m => Field Parsed -> m (Field Resolved)
reField (Field n e s) = Field n <$> reExpr e <*> pure s

reArm :: MonadResolve m
      => Arm Parsed -> m (Arm Resolved)
reArm (Arm p g b) = do
  (p', vs, ts) <- reWholePattern p
  extendTyvars ts . extendVals vs $
    Arm p' <$> traverse reExpr g <*> reExpr b

reType :: (MonadResolve m, Reasonable a p)
       => a p -> Type Parsed -> m (Type Resolved)
reType r (TyCon v) = TyCon <$> (lookupTy v `catchJunk` r)
reType r (TyVar v) = TyVar <$> (lookupTyvar v `catchJunk` r)
reType r (TyPromotedCon v) = TyPromotedCon <$> (lookupEx v `catchJunk` r)
reType _ v@TySkol{} = error ("impossible! resolving skol " ++ show v)
reType _ v@TyWithConstraints{} = error ("impossible! resolving withcons " ++ show v)
reType _ (TyLit v) = pure (TyLit v)
reType r (TyPi (Invisible v k req) ty) = do
  v' <- tagVar v
  ty' <- extendTyvar v v' $ reType r ty
  k <- traverse (reType r) k
  pure (TyPi (Invisible v' k req) ty')
reType r (TyPi (Anon f) x) = TyPi . Anon <$> reType r f <*> reType r x
reType r (TyPi (Implicit f) x) = TyPi . Implicit <$> reType r f <*> reType r x
reType r (TyApp f x) = TyApp <$> reType r f <*> reType r x
reType r (TyRows t f) = TyRows <$> reType r t
                               <*> traverse (\(a, b) -> (a,) <$> reType r b) f
reType r (TyExactRows f) = TyExactRows <$> traverse (\(a, b) -> (a,) <$> reType r b) f
reType r (TyTuple ta tb) = TyTuple <$> reType r ta <*> reType r tb
reType _ (TyWildcard _) = pure (TyWildcard Nothing)
reType r (TyParens t) = TyParens <$> reType r t
reType r (TyOperator tl o tr) = TyOperator <$> reType r tl <*> (lookupTy o `catchJunk` r) <*> reType r tr
reType _ TyType = pure TyType

reWholePattern :: forall m. MonadResolve m
               => Pattern Parsed
               -> m ( Pattern Resolved
                    , [(Var Parsed, Var Resolved)]
                    , [(Var Parsed, Var Resolved)])
reWholePattern p = do
  -- Resolves a pattern and ensures it is linear
  (p', vs, ts) <- rePattern p
  checkLinear vs
  checkLinear ts
  pure (p', map lim vs, map lim ts)

 where checkLinear :: [(Var Parsed, Var Resolved, Pattern Resolved)] -> m ()
       checkLinear = traverse_ (\vs@((_,v, _):_) -> dictates . wrapError p $ NonLinearPattern v (map thd3 vs))
                   . filter ((>1) . length)
                   . groupBy ((==) `on` fst3)
                   . sortOn fst3
       lim (a, b, _) = (a, b)

rePattern :: MonadResolve m
          => Pattern Parsed
          -> m ( Pattern Resolved
               , [(Var Parsed, Var Resolved, Pattern Resolved)]
               , [(Var Parsed, Var Resolved, Pattern Resolved )])
rePattern (Wildcard a) = pure (Wildcard a, [], [])
rePattern (Capture v a) = do
  v' <- tagVar v
  let p = Capture v' a
  pure (p, [(v, v', p)], [])
rePattern (PAs p v a) = do
  v' <- tagVar v
  (p', vs, ts) <- rePattern p
  let as = PAs p' v' a
  pure (as, (v, v', as):vs, ts)
rePattern r@(Destructure v Nothing a) = do
  v' <- lookupEx v `catchJunk` r
  pure (Destructure v' Nothing a, [], [])
rePattern r@(Destructure v p a) = do
  v' <- lookupEx v `catchJunk` r
  (p', vs, ts) <- case p of
    Nothing -> pure (Nothing, [], [])
    Just pat -> do
      (p', vs, ts) <- rePattern pat
      pure (Just p', vs, ts)
  pure (Destructure v' p' a, vs, ts)
rePattern r@(PType p t a) = do
  (p', vs, ts) <- rePattern p
  let fvs = toList (ftv t)
  fresh <- for fvs $ \x -> lookupTyvar x `absolving` tagVar x
  t' <- extendTyvars (zip fvs fresh) (reType r t)
  let r' = PType p' t' a
  pure (r', vs, zip3 fvs fresh (repeat r') ++ ts)
rePattern (PRecord f a) = do
  (f', vss, tss) <- unzip3 <$> traverse (\(n, p) -> do
                                       (p', vs, ts) <- rePattern p
                                       pure ((n, p'), vs, ts))
                              f
  pure (PRecord f' a, concat vss, concat tss)
rePattern (PTuple ps a) = do
  (ps', vss, tss) <- unzip3 <$> traverse rePattern ps
  pure (PTuple ps' a, concat vss, concat tss)
rePattern (PList ps a) = do
  (ps', vss, tss) <- unzip3 <$> traverse rePattern ps
  pure (PList ps' a, concat vss, concat tss)
rePattern (PLiteral l a) = pure (PLiteral l a, [], [])
rePattern PGadtCon{} = error "Impossible PGadtCon"

reBinding :: MonadResolve m
          => Binding Parsed
          -> m ( Expr Resolved -> Binding Resolved
               , [(Var Parsed, Var Resolved)]
               , [(Var Parsed, Var Resolved)] )
reBinding (Binding v _ c a) = do
  v' <- tagVar v
  pure ( \e' -> Binding v' e' c a, [(v, v')], [])
reBinding (Matching p _ a) = do
  (p', vs, ts) <- reWholePattern p
  pure ( \e' -> Matching p' e' a, vs, ts)
reBinding TypedMatching{} = error "reBinding TypedMatching{}"

reMethod :: MonadResolve m
         => InstanceItem Parsed
         -> m (m (InstanceItem Resolved), [(Var Parsed, Var Resolved)])
reMethod (MethodImpl b@(Binding var bod c an)) = do
  var' <- retcons (wrapError b) $ lookupEx var
  pure ( (\bod' -> MethodImpl (Binding var' bod' c an)) <$> reExpr bod
       , [(var, var')] )
reMethod (MethodImpl b@(Matching (Capture var _) bod an)) = do
  var' <- retcons (wrapError b) $ lookupEx var
  pure ( (\bod' -> MethodImpl (Binding var' bod' True an)) <$> reExpr bod
       , [(var, var')] )
reMethod (MethodImpl b@Matching{}) =
  confesses (ArisingFrom IllegalMethod (BecauseOf b))

reMethod b@(TypeImpl var args exp ann) = do
  var' <- retcons (wrapError b) $ lookupTy var
  (args, sc) <- resolveTele b args
  exp <- extendTyvars sc $
    reType b exp
  pure (pure (TypeImpl var' args exp ann), [(var, var')])

reMethod (MethodImpl TypedMatching{}) = error "reBinding TypedMatching{}"

-- | Lookup a variable in a signature, using a specific lens.
lookupIn :: MonadResolve m
         => (Signature -> Map.Map VarName a)
         -> Var Parsed -> VarKind -> Signature
         -> m a
lookupIn g v k = go v where
  go (Name n) env =
    case Map.lookup n (g env) of
      Nothing -> confesses (NotInScope k v [])
      Just x -> pure x
  go (InModule m n) env =
    case Map.lookup m (env ^. modules) of
      Nothing -> confesses (NotInScope k v [])
      -- Abort without an error if the module is unresolved. This is "safe", as
      -- we'll have already produced an error at the original error.
      Just (_, Nothing) -> confess mempty
      Just (_, Just env) -> go n env

-- | Convert a slot into a concrete variable
lookupSlot :: MonadResolve m
           => Var Parsed -> Slot
           -> m (Var Resolved)
lookupSlot _ (SVar x) = pure x
lookupSlot v (SAmbiguous vs) = confesses (Ambiguous v vs)

-- | Lookup a value/expression variable.
lookupEx :: MonadResolve m => Var Parsed -> m (Var Resolved)
lookupEx v = view scope
         >>= lookupIn (^.vals) v (if isCtorVar v then VarCtor else VarVar)
         >>= lookupSlot v

-- | Lookup a type name.
lookupTy :: MonadResolve m => Var Parsed -> m (Var Resolved)
lookupTy v = view scope
         >>= lookupIn (^.types) v (if isCtorVar v then VarCtor else VarType)
         >>= lookupSlot v

-- | Lookup a tyvar.
lookupTyvar :: MonadResolve m => Var Parsed -> m (Var Resolved)
lookupTyvar v@(Name n) = do
  vars <- view tyvars
  case Map.lookup n vars of
    Nothing -> confesses (NotInScope VarTyvar v [])
    Just x -> lookupSlot v x
lookupTyvar InModule{} = error "Impossible: InModule tyvar"

-- | A garbage variable used when we cannot resolve something.
junkVar :: Var Resolved
junkVar = TgInternal "<missing>"

junkExpr :: Ann Resolved -> Expr Resolved
junkExpr = VarRef junkVar

wrapError :: Reasonable e p => e p -> ResolveError -> ResolveError
wrapError _  e@(ArisingFrom _ _) = e
wrapError r e = ArisingFrom e (BecauseOf r)

-- | Catch an error, returning a junk variable instead.
catchJunk :: (MonadResolve m, Reasonable e p)
          => m (Var Resolved) -> e p -> m (Var Resolved)
catchJunk m r = recover junkVar (retcons (wrapError r) m)

isCtorVar :: Var Parsed -> Bool
isCtorVar (Name t) = T.length t > 0 && classify (T.head t) == Upper
isCtorVar (InModule _ v) = isCtorVar v
