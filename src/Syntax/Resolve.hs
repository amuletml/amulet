{-# LANGUAGE FlexibleContexts
  , ConstraintKinds
  , OverloadedStrings
  , MultiParamTypeClasses
  , TupleSections
  , TypeFamilies
  , ScopedTypeVariables #-}

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
  , VarKind(..)
  ) where

import Control.Monad.Chronicles
import Control.Monad.Reader
import Control.Applicative
import Control.Monad.State
import Control.Monad.Namey
import Control.Lens hiding (Lazy)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Traversable
import Data.Sequence (Seq)
import Data.Foldable
import Data.Function
import Data.Functor
import Data.Reason
import Data.Triple
import Data.Maybe
import Data.These
import Data.List

import Syntax.Resolve.Toplevel
import Syntax.Resolve.Scope
import Syntax.Resolve.Error
import Syntax.Subst
import Syntax

import Parser.Unicode

type MonadResolve m = ( MonadChronicles ResolveError m
                      , MonadReader Scope m
                      , MonadNamey m
                      , MonadState ModuleScope m)

-- | Resolve a program within a given 'Scope' and 'ModuleScope'
resolveProgram :: MonadNamey m
               => Scope -- ^ The scope in which to resolve this program
               -> ModuleScope
               -- ^ The current module scope. We return an updated
               -- version of this if we declare or extend any modules.
               -> [Toplevel Parsed] -- ^ The program to resolve
               -> m (Either [ResolveError] ([Toplevel Resolved], ModuleScope))
               -- ^ The resolved program or a list of resolution errors
resolveProgram scope modules = runResolve scope modules . resolveModule

-- | Run the resolver monad.
runResolve :: MonadNamey m
           => Scope -- ^ The initial state to resolve objects in
           -> ModuleScope -- ^ The scope for modules
           -> StateT ModuleScope (ReaderT Scope (ChronicleT (Seq ResolveError) m)) a
           -> m (Either [ResolveError] (a, ModuleScope))
runResolve scope modules
  = (these (Left . toList) Right (\x _ -> Left (toList x))<$>)
  . runChronicleT . flip runReaderT scope . flip runStateT modules

-- | Resolve the whole program
resolveModule :: MonadResolve m => [Toplevel Parsed] -> m [Toplevel Resolved]
resolveModule [] = pure []

resolveModule (LetStmt am bs:rs) = do
  (bs', vs, ts) <- unzip3 <$> traverse reBinding bs
  extendTyvarN (concat ts) . extendN (concat vs) $ (:)
    <$> (LetStmt am <$> traverse (uncurry (flip (<$>) . reExpr . view bindBody)) (zip bs bs'))
    <*> resolveModule rs

resolveModule (r@(ForeignVal am v t ty a):rs) = do
  v' <- tagVar v
  extend (v, v') $ (:)
    <$> (ForeignVal am
          <$> lookupEx v `catchJunk` r
          <*> pure t
          <*> reType r (wrap ty)
          <*> pure a)
    <*> resolveModule rs

  where wrap x = foldr (TyPi . flip (flip Invisible Nothing) Spec) x (toList (ftv x))
resolveModule (d@(TypeDecl am t vs cs):rs) = do
  t'  <- tagVar t
  (vs', sc) <- resolveTele d vs
  let c = map extractCons cs
  c' <- traverse tagVar c
  extendTy (t, t') $ extendN (zip c c') $ (:)
    . TypeDecl am t' vs'
    <$> traverse (resolveCons sc) (zip cs c')
    <*> resolveModule rs

  where resolveCons _ (UnitCon ac _ a, v') = pure $ UnitCon ac v' a
        resolveCons vs (r@(ArgCon ac _ t a), v') = ArgCon ac v' <$> extendTyvarN vs (reType r t) <*> pure a
        resolveCons _  (r@(GadtCon ac _ t a), v') = do
          let fvs = toList (ftv t)
          fresh <- traverse tagVar fvs
          t' <- extendTyvarN (zip fvs fresh) (reType r t)
          pure (GadtCon ac v' t' a)

        extractCons (UnitCon _ v _) = v
        extractCons (ArgCon _ v _ _) = v
        extractCons (GadtCon _ v _ _) = v

resolveModule (r@(Open name as):rs) =
  -- Opens hard-fail, as anything inside it will probably fail to resolve
  retcons (wrapError r) $
    resolveOpen name as (\name' -> (Open name' as:) <$> resolveModule rs)

resolveModule (Module am name body:rs) = do
  fullName <- foldl (flip InModule) name <$> asks modStack
  body' <- extendM name $ resolveModule body

  let (vars, tys) = extractToplevels body
  let (vars', tys') = extractToplevels body'

  (ModuleScope modules) <- get
  (name', scope) <- case Map.lookup fullName modules of
                      Just env -> pure env
                      Nothing -> (,emptyScope) <$> tagModule fullName

  let scope' = scope { varScope = foldr (uncurry Map.insert) (varScope scope) (zip vars (map SVar vars'))
                     , tyScope  = foldr (uncurry Map.insert) (tyScope scope) (zip tys (map SVar tys')) }
  put $ ModuleScope $ Map.insert fullName (name', scope') modules

  extendN (modZip name name' vars vars') $ extendTyN (modZip name name' tys tys') $ (:)
    <$> pure (Module am name' body')
    <*> resolveModule rs

  where modZip name name' v v' = zip (map (name<>) v) (map (name'<>) v')

resolveModule (t@(Class name am ctx tvs ms ann):rs) = do
  name' <- tagVar name
  (tvs', tvss) <- resolveTele t tvs

  extendTy (name, name') $ do
    (ctx', (ms', vs')) <- extendTyvarN tvss $
      (,) <$> traverse (reType t) ctx
          <*> (unzip <$> traverse (reClassItem (map fst tvss)) ms)

    extendN (mconcat vs') $ do
      ms'' <- extendTyvarN tvss (sequence ms')
      (Class name' am ctx' tvs' ms'' ann:) <$> resolveModule rs

  where
    reClassItem tvs' m@(MethodSig name ty an) = do
      name' <- tagVar name
      pure ( MethodSig name' <$> reType m (wrap tvs' ty) <*> pure an
           , [(name, name')] )
    reClassItem _ (DefaultMethod b an) =
      pure ( DefaultMethod <$> (fst =<< reMethod b) <*> pure an
           , [] )
    wrap tvs' x = foldr (TyPi . flip (flip Invisible Nothing) Spec) x (ftv x `Set.difference` Set.fromList tvs')

resolveModule (t@(Instance cls ctx head ms ann):rs) = do
  cls' <- lookupTy cls `catchJunk` t

  let fvs = toList (foldMap ftv ctx <> ftv head)
  fvs' <- traverse tagVar fvs

  t' <- extendTyvarN (zip fvs fvs') $ do
    ctx' <- traverse (reType t) ctx
    head' <- reType t head

    (ms', vs) <- unzip <$> traverse reMethod ms
    ms'' <- extendN (concat vs) (sequence ms')

    pure (Instance cls' ctx' head' ms'' ann)

  (t':) <$> resolveModule rs

lookupVar :: MonadResolve m
          => Var Parsed -> VarKind -> Map.Map (Var Parsed) ScopeVariable
          -> m (Var Resolved)
lookupVar v k m = case Map.lookup v m of
    Nothing -> confesses (NotInScope k v [])
    Just (SVar x) -> pure x
    Just (SAmbiguous vs) -> confesses (Ambiguous v vs)

lookupEx :: MonadResolve m => Var Parsed -> m (Var Resolved)
lookupEx v = asks varScope >>= lookupVar v (if isCtorVar v then VarCtor else VarVar)

lookupTy :: MonadResolve m => Var Parsed -> m (Var Resolved)
lookupTy v = asks tyScope >>= lookupVar v (if isCtorVar v then VarCtor else VarType)

lookupTyvar :: MonadResolve m => Var Parsed -> m (Var Resolved)
lookupTyvar v = asks tyvarScope >>= lookupVar v VarTyvar

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
  extendTyvarN (concat ts) . extendN (concat vs) $
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
  extendTyvarN ts . extendN vs $ Fun p' <$> reExpr e <*> pure a

reExpr r@(Begin [] a) = dictates (wrapError r EmptyBegin) $> junkExpr a
reExpr (Begin es a) = Begin <$> traverse reExpr es <*> pure a

reExpr (Literal l a) = pure (Literal l a)

reExpr (Match e ps a) = do
  e' <- reExpr e
  ps' <- traverse reArm ps
  pure (Match e' ps' a)

reExpr r@(Function [] a) = dictates (ArisingFrom EmptyMatch (BecauseOf r)) $> junkExpr a
reExpr (Function ps a) = flip Function a <$> traverse reArm ps

reExpr (BinOp l o r a) = BinOp <$> reExpr l <*> reExpr o <*> reExpr r <*> pure a
reExpr (Hole v a) = Hole <$> tagVar v <*> pure a
reExpr r@(Ascription e t a) = Ascription
                          <$> reExpr e
                          <*> reType r t
                          <*> pure a
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

reExpr r@(OpenIn m e a) =
  retcons (wrapError r) $
    resolveOpen m Nothing (\m' -> OpenIn m' <$> reExpr e <*> pure a)

reExpr (Lazy e a) = Lazy <$> reExpr e <*> pure a
reExpr (Vta e t a) = Vta <$> reExpr e <*> reType e t <*> pure a

reExpr (ListComp e qs a) =
  let go (CompGuard e:qs) acc = do
        e <- reExpr e
        go qs (CompGuard e:acc)
      go (CompGen b e an:qs) acc = do
        e <- reExpr e
        (b, es, ts) <- reWholePattern b
        extendTyvarN ts . extendN es $
          go qs (CompGen b e an:acc)
      go (CompLet bs an:qs) acc =do
        (bs', vs, ts) <- unzip3 <$> traverse reBinding bs
        extendTyvarN (concat ts) . extendN (concat vs) $ do
          bs <- traverse (uncurry (flip (<$>) . reExpr . view bindBody)) (zip bs bs')
          go qs (CompLet bs an:acc)
      go [] acc = ListComp <$> reExpr e <*> pure (reverse acc) <*> pure a
  in go qs []

reExpr (For (var, a) b c d an) = do
  a <- reExpr a
  v <- tagVar var
  extend (var, v) $
    For (v, a) <$> reExpr b <*> reExpr c <*> reExpr d <*> pure an

reExpr (While b c a) = While <$> reExpr b <*> reExpr c <*> pure a

reExpr ExprWrapper{} = error "resolve cast"

reArm :: MonadResolve m
      => Arm Parsed -> m (Arm Resolved)
reArm (Arm p g b) = do
  (p', vs, ts) <- reWholePattern p
  extendTyvarN ts . extendN vs $
    Arm p' <$> traverse reExpr g <*> reExpr b


reType :: (MonadResolve m, Reasonable a p)
       => a p -> Type Parsed -> m (Type Resolved)
reType r (TyCon v) = TyCon <$> (lookupTy v `catchJunk` r)
reType r (TyVar v) = TyVar <$> (lookupTyvar v `catchJunk` r)
reType r (TyPromotedCon v) = TyPromotedCon <$> (lookupEx v `catchJunk` r)
reType _ v@TySkol{} = error ("impossible! resolving skol " ++ show v)
reType _ v@TyWithConstraints{} = error ("impossible! resolving withcons " ++ show v)
reType r (TyPi (Invisible v k req) ty) = do
  v' <- tagVar v
  ty' <- extendTyvar (v, v') $ reType r ty
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
  t' <- extendTyvarN (zip fvs fresh) (reType r t)
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
rePattern PWrapper{} = error "Impossible PWrapper"
rePattern PSkolem{} = error "Impossible PSkolem"

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
         => Binding Parsed
         -> m (m (Binding Resolved), [(Var Parsed, Var Resolved)])
reMethod b@(Binding var bod c an) = do
  var' <- retcons (wrapError b) $ lookupEx var
  pure ( (\bod' -> Binding var' bod' c an) <$> reExpr bod
       , [(var, var')] )
reMethod b@(Matching (Capture var _) bod an) = do
  var' <- retcons (wrapError b) $ lookupEx var
  pure ( (\bod' -> Binding var' bod' True an) <$> reExpr bod
       , [(var, var')] )
reMethod b@Matching{} =
  confesses (ArisingFrom IllegalMethod (BecauseOf b))
reMethod TypedMatching{} = error "reBinding TypedMatching{}"

resolveOpen :: MonadResolve m => Var Parsed -> Maybe T.Text -> (Var Resolved -> m a) -> m a
resolveOpen name as m = do
  stack <- asks modStack
  (ModuleScope modules) <- get
  case lookupModule name modules stack of
    Nothing -> confesses (NotInScope VarModule name [])
    Just (name', Scope vars tys _ _) ->
      let prefix = case as of
                     Nothing -> id
                     Just v -> InModule v
      in
        local (\s -> s { varScope = Map.mapKeys prefix vars `Map.union` varScope s
                       , tyScope  = Map.mapKeys prefix tys  `Map.union` tyScope s }) (m name')

  where
    lookupModule n m [] = Map.lookup n m
    lookupModule n m x@(_:xs) = Map.lookup (foldl (flip InModule) n x) m
                                <|> lookupModule n m xs

junkVar :: Var Resolved
junkVar = TgInternal "<missing>"

junkExpr :: Ann Resolved -> Expr Resolved
junkExpr = VarRef junkVar

wrapError :: Reasonable e p => e p -> ResolveError -> ResolveError
wrapError _  e@(ArisingFrom _ _) = e
wrapError r e = ArisingFrom e (BecauseOf r)

catchJunk :: (MonadResolve m, Reasonable e p)
          => m (Var Resolved) -> e p -> m (Var Resolved)
catchJunk m r = recover junkVar (retcons (wrapError r) m)

reField :: MonadResolve m => Field Parsed -> m (Field Resolved)
reField (Field n e s) = Field n <$> reExpr e <*> pure s

isCtorVar :: Var Parsed -> Bool
isCtorVar (Name t) = T.length t > 0 && classify (T.head t) == Upper
isCtorVar (InModule _ v) = isCtorVar v
