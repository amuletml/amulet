{-# LANGUAGE FlexibleContexts
  , ConstraintKinds
  , OverloadedStrings
  , MultiParamTypeClasses
  , TupleSections #-}

{- | The resolver is the first step after parsing. It performs several key
   actions:

    * Determines what variable every identifier is pointing to, including
      module variables and handling ambiguous variables.

    * Handles module definitions and `open`s.

    * Prohibit some dubious constructs, such as patterns which bind the
      same identifier multiple times.

    * Reorganise binary operators, taking precedence and associativity
      into account (the parser ignores these intentionally).
-}
module Syntax.Resolve
  ( resolveProgram
  , runResolve
  , ResolveError(..)
  ) where

import Control.Monad.Except
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Applicative hiding (empty)
import Control.Monad.State
import Control.Monad.Namey
import Control.Lens hiding (Lazy)

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import Data.Traversable
import Data.Sequence (Seq)
import Data.Foldable
import Data.Function
import Data.Functor
import Data.Spanned
import Data.Reason
import Data.Triple
import Data.Maybe
import Data.Span
import Data.List

import Syntax.Resolve.Scope
import Syntax.Resolve.Toplevel
import Syntax.Pretty
import Syntax.Subst

import Text.Pretty.Semantic hiding (group)
import Text.Pretty.Note

-- | An error in the resolution process. Note that one error may be
-- thrown multiple times.
data ResolveError
  = NotInScope (Var Parsed)
  | NoSuchModule (Var Parsed)
  | Ambiguous (Var Parsed) [Var Resolved]
  | NonLinearPattern (Var Resolved) [Pattern Resolved]
  | NonLinearRecord (Expr Parsed) T.Text
  | EmptyMatch
  | EmptyBegin

  | ArisingFrom ResolveError SomeReason
  deriving (Show)

instance Pretty ResolveError where
  pretty (NotInScope e) = "Variable not in scope:" <+> verbatim e
  pretty (NoSuchModule e) = "Module not in scope:" <+> verbatim e
  pretty (Ambiguous v _) = "Ambiguous reference to variable:" <+> verbatim v
  pretty (NonLinearPattern v _) = "Non-linear pattern (multiple definitions of" <+> verbatim v <+> ")"
  pretty EmptyMatch = "Empty match expression"
  pretty EmptyBegin = "Empty begin expression"
  pretty (NonLinearRecord _ t) = "Duplicate field" <+> stypeSkol (text t) <+> "in record" <#> empty
  pretty (ArisingFrom er ex) = pretty er <#> empty <#> nest 4 (string "Arising from use of" <+> blameOf ex </> pretty ex)

instance Spanned ResolveError where
  annotation (ArisingFrom _ x) = annotation x
  annotation (NonLinearRecord e _) = annotation e
  annotation _ = undefined

instance Note ResolveError Style where
  diagnosticKind _ = ErrorMessage

  formatNote f x = indent 2 (Right <$> pretty x) <#> fromJust (body x) where
    body (ArisingFrom er a) = body er <|> Just (f [annotation a])
    body (NonLinearPattern _ ps) = Just (f (map annotation ps))
    body (NonLinearRecord e _) = Just (f [annotation e])
    body _ = Nothing

type MonadResolve m = ( MonadError ResolveError m
                      , MonadWriter (Seq ResolveError) m
                      , MonadReader Scope m
                      , MonadNamey m
                      , MonadState ModuleScope m)

resolveProgram :: MonadNamey m
               => Scope -> ModuleScope -> [Toplevel Parsed]
               -> m (Either [ResolveError] ([Toplevel Resolved], ModuleScope))
resolveProgram scope modules = runResolve scope modules . resolveModule

-- | Run the resolver monad
runResolve :: MonadNamey m
           => Scope -- ^ The initial state to resolve objects in
           -> ModuleScope -- ^ The scope for modules
           -> StateT ModuleScope (ReaderT Scope (ExceptT ResolveError (WriterT (Seq ResolveError) m))) a
           -> m (Either [ResolveError] (a, ModuleScope))
runResolve scope modules = fmap handle . runWriterT . runExceptT . flip runReaderT scope . flip runStateT modules
  where handle (Left e, er) = Left (e:toList er)
        handle (Right e, er) = case toList er of
                                [] -> Right e
                                er -> Left er

-- | Resolve the whole program
resolveModule :: MonadResolve m => [Toplevel Parsed] -> m [Toplevel Resolved]
resolveModule [] = pure []

resolveModule (LetStmt vs:rs) =  do
  let vars = vs ^.. each . bindVariable
  vars' <- traverse tagVar vars
  extendN (zip vars vars') $ (:)
    <$> (LetStmt
          <$> zipWithM (\v' (Binding _ e p a) -> flip (flip (Binding v') p) a <$> reExpr e) vars' vs)
    <*> resolveModule rs

resolveModule (r@(ForeignVal v t ty a):rs) = do
  v' <- tagVar v
  extend (v, v') $ (:)
    <$> (ForeignVal
          <$> lookupEx v `catchJunk` r
          <*> pure t
          <*> reType r (wrap ty)
          <*> pure a)
    <*> resolveModule rs

  where wrap x = foldr (TyPi . flip Invisible Nothing) x (toList (ftv x))
resolveModule (d@(TypeDecl t vs cs):rs) = do
  t'  <- tagVar t
  (vs', sc) <- resolveTele d vs
  let c = map extractCons cs
  c' <- traverse tagVar c
  extendTy (t, t') $ extendN (zip c c') $ (:)
    . TypeDecl t' vs' <$> traverse (resolveCons sc) (zip cs c')
    <*> resolveModule rs

  where resolveCons _ (UnitCon _ a, v') = pure $ UnitCon v' a
        resolveCons vs (r@(ArgCon _ t a), v') = ArgCon v' <$> extendTyvarN vs (reType r t) <*> pure a
        resolveCons _  (r@(GeneralisedCon _ t a), v') = do
          let fvs = toList (ftv t)
          fresh <- traverse tagVar fvs
          t' <- extendTyvarN (zip fvs fresh) (reType r t)
          pure (GeneralisedCon v' t' a)

        extractCons (UnitCon v _) = v
        extractCons (ArgCon v _ _) = v
        extractCons (GeneralisedCon v _ _) = v

resolveModule (r@(Open name as):rs) =
  -- Opens hard-fail, as anything inside it will probably fail to resolve
  resolveOpen name as (\name' -> (Open name' as:) <$> resolveModule rs)
  `catchError` (throwError . wrapError r)

resolveModule (Module name body:rs) = do
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
    <$> pure (Module name' body')
    <*> resolveModule rs

  where modZip name name' v v' = zip (map (name<>) v) (map (name'<>) v')

lookupVar :: MonadResolve m
          => (Var Parsed -> ResolveError) -> Var Parsed -> Map.Map (Var Parsed) ScopeVariable
          -> m (Var Resolved)
lookupVar err v m = case Map.lookup v m of
    Nothing -> throwError (err v)
    Just (SVar x) -> pure x
    Just (SAmbiguous vs) -> throwError (Ambiguous v vs)

lookupEx :: MonadResolve m => Var Parsed -> m (Var Resolved)
lookupEx v = asks varScope >>= lookupVar NotInScope v

lookupTy :: MonadResolve m => Var Parsed -> m (Var Resolved)
lookupTy v = asks tyScope >>= lookupVar NotInScope v

lookupTyvar :: MonadResolve m => Var Parsed -> m (Var Resolved)
lookupTyvar v = asks tyvarScope >>= lookupVar NotInScope v

resolveTele :: (MonadResolve m, Reasonable f p) => f p -> [TyConArg Parsed] -> m ([TyConArg Resolved], [(Var Parsed, Var Resolved)])
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

reExpr (Let vs c a) = do
  let vars = vs ^.. each . bindVariable
  vars' <- traverse tagVar vars
  extendN (zip vars vars') $ Let <$> zipWithM (\v' (Binding _ e p a) -> flip (flip (Binding v') p) a <$> reExpr e) vars' vs
                                 <*> reExpr c
                                 <*> pure a
reExpr (If c t b a) = If <$> reExpr c <*> reExpr t <*> reExpr b <*> pure a
reExpr (App f p a) = App <$> reExpr f <*> reExpr p <*> pure a
reExpr (Fun p e a) = do
  let reWholePattern' (PatParam p) = do
        (p', vs, ts) <- reWholePattern p
        pure (PatParam p', vs, ts)
      reWholePattern' (ImplParam p) = do
        (p', vs, ts) <- reWholePattern p
        pure (ImplParam p', vs, ts)
  (p', vs, ts) <- reWholePattern' p
  extendTyvarN ts . extendN vs $ Fun p' <$> reExpr e <*> pure a

reExpr r@(Begin [] a) = tell (pure (wrapError r EmptyBegin)) $> junkExpr a
reExpr (Begin es a) = Begin <$> traverse reExpr es <*> pure a

reExpr (Literal l a) = pure (Literal l a)

-- reExpr r@(Match e [] a) = do
--   _ <- reExpr e
--   tell (pure (ArisingFrom EmptyMatch (BecauseOf r)))
--   pure (junkExpr a)
reExpr (Match e ps a) = do
  e' <- reExpr e
  ps' <- traverse (\(p, b) -> do
                  (p', vs, ts) <- reWholePattern p
                  (p',) <$> extendTyvarN ts (extendN vs (reExpr b)))
              ps
  pure (Match e' ps' a)

reExpr r@(Function [] a) = tell (pure (ArisingFrom EmptyMatch (BecauseOf r))) $> junkExpr a
reExpr (Function ps a) =
  flip Function a <$> for ps (\(p, b) -> do
    (p', vs, ts) <- reWholePattern p
    (p',) <$> extendTyvarN ts (extendN vs (reExpr b)))

reExpr o@BinOp{} = do
  (es, os) <- reOp [] [] o
  let ([x], []) = popUntil es os 0 AssocLeft
  pure x
    where
      reOp :: MonadResolve m
           => [Expr Resolved]
           -> [(Expr Resolved, Int)]
           -> Expr Parsed
           -> m ([Expr Resolved], [(Expr Resolved, Int)])
      reOp es ops (BinOp l o@VarRef{} r _) = do
        (es', ops') <- reOp es ops l

        o'@(VarRef op _) <- reExpr o
        let (opre, oass) = varPrecedence op

        let (es'', ops'') = popUntil es' ops' opre oass
        reOp es'' ((o', opre):ops'') r
      reOp _ _ BinOp{} = error "BinOp with non-name operator"
      reOp es ops e = (,ops) . (:es) <$> reExpr e

      popUntil :: [Expr Resolved]
               -> [(Expr Resolved, Int)]
               -> Int -> Associativity
               -> ([Expr Resolved], [(Expr Resolved, Int)])
      popUntil es ((sop, spre):os) opre assoc
        | (assoc == AssocLeft && spre >= opre) || (assoc == AssocRight && spre > opre)
        = let (right:left:es') = es
          in popUntil ( BinOp left sop right
                              (mkSpanUnsafe (spanStart (annotation left)) (spanEnd (annotation right)))
                        : es' ) os opre assoc
      popUntil es os _ _ = (es, os)

reExpr (Hole v a) = Hole <$> tagVar v <*> pure a
reExpr r@(Ascription e t a) = Ascription
                          <$> reExpr e
                          <*> reType r t
                          <*> pure a
reExpr e@(Record fs a) = do
  let ls = map fst fs
      dupes = mapMaybe (listToMaybe . tail) . group . sort $ ls
  traverse_ (tell . Seq.singleton . NonLinearRecord e) dupes
  Record <$> traverse (traverse reExpr) fs <*> pure a
reExpr ex@(RecordExt e fs a) = do
  let ls = map fst fs
      dupes = mapMaybe (listToMaybe . tail) . group . sort $ ls
  traverse_ (tell . Seq.singleton . NonLinearRecord ex) dupes
  RecordExt <$> reExpr e <*> traverse (traverse reExpr) fs <*> pure a

reExpr (Access e t a) = Access <$> reExpr e <*> pure t <*> pure a
reExpr (LeftSection o r a) = LeftSection <$> reExpr o <*> reExpr r <*> pure a
reExpr (RightSection l o a) = RightSection <$> reExpr l <*> reExpr o <*> pure a
reExpr (BothSection o a) = BothSection <$> reExpr o <*> pure a
reExpr (AccessSection t a) = pure (AccessSection t a)
reExpr (Parens e a) = flip Parens a <$> reExpr e

reExpr (Tuple es a) = Tuple <$> traverse reExpr es <*> pure a
reExpr (TupleSection es a) = TupleSection <$> traverse (traverse reExpr) es <*> pure a

reExpr r@(OpenIn m e a) =
  resolveOpen m Nothing (\m' -> OpenIn m' <$> reExpr e <*> pure a)
  `catchError` (throwError . wrapError r)

reExpr (Lazy e a) = Lazy <$> reExpr e <*> pure a
reExpr ExprWrapper{} = error "resolve cast"

reType :: (MonadResolve m, Reasonable a p)
       => a p -> Type Parsed -> m (Type Resolved)
reType r (TyCon v) = TyCon <$> (lookupTy v `catchJunk` r)
reType r (TyVar v) = TyVar <$> (lookupTyvar v `catchJunk` r)
reType r (TyPromotedCon v) = TyPromotedCon <$> (lookupEx v `catchJunk` r)
reType _ v@TySkol{} = error ("impossible! resolving skol " ++ show v)
reType _ v@TyWithConstraints{} = error ("impossible! resolving withcons " ++ show v)
reType r (TyPi (Invisible v k) ty) = do
  v' <- tagVar v
  ty' <- extendTyvar (v, v') $ reType r ty
  k <- traverse (reType r) k
  pure (TyPi (Invisible v' k) ty')
reType r (TyPi (Anon f) x) = TyPi . Anon <$> reType r f <*> reType r x
reType r (TyPi (Implicit f) x) = TyPi . Implicit <$> reType r f <*> reType r x
reType r (TyApp f x) = TyApp <$> reType r f <*> reType r x
reType r (TyRows t f) = TyRows <$> reType r t
                               <*> traverse (\(a, b) -> (a,) <$> reType r b) f
reType r (TyExactRows f) = TyExactRows <$> traverse (\(a, b) -> (a,) <$> reType r b) f
reType r (TyTuple ta tb) = TyTuple <$> reType r ta <*> reType r tb
reType _ TyType = pure TyType

reWholePattern :: MonadResolve m
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

  where checkLinear = traverse_ (\vs@((_,v, _):_) -> tell (pure (wrapError p (NonLinearPattern v (map thd3 vs)))))
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
  fresh <- for fvs $ \x -> lookupTyvar x `catchError` const (tagVar x)
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
rePattern (PLiteral l a) = pure (PLiteral l a, [], [])
rePattern PWrapper{} = undefined

data Associativity = AssocLeft | AssocRight
  deriving (Eq, Show)

varPrecedence :: Var Resolved -> (Int, Associativity)
varPrecedence (TgName n _) = precedence n
varPrecedence (TgInternal n) = precedence n

precedence :: T.Text -> (Int, Associativity)
precedence t
  | T.isPrefixOf "**" t = (10, AssocRight)

  | T.isPrefixOf "*" t = (9, AssocLeft)
  | T.isPrefixOf "/" t = (9, AssocLeft)
  | T.isPrefixOf "%" t = (9, AssocLeft)

  | T.isPrefixOf "+" t = (8, AssocLeft)
  | T.isPrefixOf "-" t = (8, AssocLeft)

  | T.isPrefixOf "::" t = (7, AssocRight)

  | T.isPrefixOf "@" t = (6, AssocRight)
  | T.isPrefixOf "^" t = (6, AssocRight)

  | T.isPrefixOf "=" t = (5, AssocLeft)
  | T.isPrefixOf "!" t = (5, AssocLeft)
  | T.isPrefixOf "<" t = (5, AssocLeft)
  | T.isPrefixOf ">" t = (5, AssocLeft)

  | T.isPrefixOf "&&" t = (4, AssocLeft)
  | T.isPrefixOf "||" t = (3, AssocLeft)

  | T.isPrefixOf "&" t = (5, AssocLeft)
  | T.isPrefixOf "|" t = (5, AssocLeft)

  | otherwise = (11, AssocLeft)

resolveOpen :: MonadResolve m => Var Parsed -> Maybe T.Text -> (Var Resolved -> m a) -> m a
resolveOpen name as m = do
  stack <- asks modStack
  (ModuleScope modules) <- get
  case lookupModule name modules stack of
    Nothing -> throwError $ NoSuchModule name
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
catchJunk m r = m `catchError` \err -> tell (pure (wrapError r err)) $> junkVar
