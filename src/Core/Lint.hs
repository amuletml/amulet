{-# LANGUAGE FlexibleContexts, FlexibleInstances,
  ScopedTypeVariables, OverloadedStrings, TupleSections, ViewPatterns #-}
module Core.Lint
  ( CoreError
  , runLint, runLintOK, emptyScope
  , checkStmt, checkTerm, checkAtom, checkType
  ) where

import Control.Applicative.Lift
import Control.Monad.Writer
import Control.Monad.Except
import Control.Applicative
import Control.Lens

import qualified Data.VarMap as VarMap
import qualified Data.VarSet as VarSet
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import Data.Traversable
import Data.Foldable
import Data.Function
import Data.Functor
import Data.Text ()
import Data.Maybe
import Data.Tuple
import Data.List

import Core.Optimise
import Core.Builtin
import Core.Types

import Text.Pretty.Semantic hiding ((<>))
import Text.Pretty.Annotation

data CoreError a
  = TypeMismatch (Type a) (Type a)
  | InfoMismatch a VarInfo VarInfo
  | InfoIllegal a VarInfo VarInfo
  | NoSuchVar a
  | IllegalUnbox
  | InvalidCoercion (Coercion a)
  | PatternMismatch [(a, Type a)] [(a, Type a)]

type CoreErrors a = (Seq.Seq (CoreError a))

type LintResult = Sum Int

data Scope a = Scope { vars :: VarMap.Map (Type a, VarInfo)
                     , types :: VarMap.Map VarInfo
                     , tyVars :: VarSet.Set }
  deriving (Show)

emptyScope :: IsVar a => Scope a
emptyScope = Scope (VarMap.fromList (map (\(a, b) -> (a, (b, varInfo a))) builtinVarList))
                   (VarMap.fromList (map (,TypeConVar) builtinTyList))
                   mempty

insertVar :: IsVar a => a -> Type a -> VarMap.Map (Type a, VarInfo) -> VarMap.Map (Type a, VarInfo)
insertVar v t = VarMap.insert (toVar v) (t, varInfo v)

insertTy :: IsVar a => a -> VarMap.Map VarInfo -> VarMap.Map VarInfo
insertTy v = VarMap.insert (toVar v) (varInfo v)

instance Pretty a => Pretty (CoreError a) where
  pretty (TypeMismatch l r) = text "Expected type" <+> pretty l </>
                              text "     got type" <+> pretty r
  pretty (InfoMismatch v l r) = text "Expected var info" <+> string (show l) </>
                                text "     got var info" <+> string (show r) </>
                                text "for" <+> pretty v
  pretty (InfoIllegal v l r) = text "Expected var info like" <+> string (show l) </>
                               text "          got var info" <+> string (show r) </>
                               text "for" <+> pretty v
  pretty (NoSuchVar a) = text "No such variable" <+> pretty a
  pretty IllegalUnbox = text "Illegal unboxed type"
  pretty (InvalidCoercion a) = text "Illegal coercion" <+> pretty a
  pretty (PatternMismatch l r) = text "Expected vars" <+> pVs l </>
                                 text "     got vars" <+> pVs r
    where pVs = hsep . punctuate comma . map (\(v, ty) -> pretty v <+> colon <+> pretty ty)
          pVs :: [(a, Type a)] -> Doc

  prettyList = vsep . map pretty

instance Pretty a => Annotation (CoreError a) where
  annotated e a = "(* XXX" <+> align (pretty e) <+> "*)" <#> a

instance Pretty a => Annotation (Seq.Seq (CoreError a)) where
  annotated = flip (foldr annotated)

instance Pretty a => Pretty [(AnnStmt (CoreErrors a) a, CoreErrors a)] where
  pretty = vsep . map (\(s, es) -> annotated es (pretty s))

runLint :: Pretty b
        => String -> Writer LintResult b
        -> c -> c
runLint context m res =
  case runLintOK m of
    Nothing -> res
    Just (c, e) -> error . T.unpack . displayDetailed $
     string ("Core lint failed after `" ++ context ++ "` with " ++ show c ++ " errors") <#>
     pretty e

runLintOK :: Writer LintResult b -> Maybe (Int, b)
runLintOK m = case runWriter m of
                (_, Sum 0) -> Nothing
                (x, Sum c) -> Just (c, x)

checkStmt :: (IsVar a, MonadWriter LintResult m)
          => Scope a
          -> [Stmt a]
          -> m [(AnnStmt (CoreErrors a) a, CoreErrors a)]
checkStmt _ [] = pure []
checkStmt s (Foreign v ty b:xs) = do
  es <- gatherError' . liftError $
    -- Ensure we're declaring a value
       unless (varInfo v == ValueVar) (pushError (InfoIllegal v ValueVar (varInfo v)))
    -- And the type is well formed
    *> checkType s ty

  ((Foreign v ty b, es):) <$> checkStmt (s { vars = insertVar v ty (vars s) }) xs

checkStmt s (StmtLet (One (v, ty, e)):xs) = do
  (ty', e') <- checkTerm s e

  es <- gatherError' . liftError $
    -- Ensure type is valid and we're declaring a value
    (case ty' of
       Just ty' | ty `apart` ty' -> pushError (TypeMismatch ty ty')
       _ -> pure ())
    *> unless (varInfo v == ValueVar) (pushError (InfoIllegal v ValueVar (varInfo v)))
    *> checkType s ty

  ((StmtLet (One (v, ty, e')), es):) <$> checkStmt (s { vars = insertVar v ty (vars s) }) xs
checkStmt s (StmtLet (Many vs):xs) = do
  let s' = s { vars = foldr (\(v, t, _) -> insertVar v t) (vars s) vs }

  (vs', es) <- forMA vs $ \(v, ty, e) -> do
    (ty', e') <- checkTerm s' e
    es <- gatherError' . liftError $
      -- Ensure type is valid and we're declaring a value
      (case ty' of
         Just ty' | ty `apart` ty' -> pushError (TypeMismatch ty ty')
         _ -> pure ())
      *> unless (varInfo v == ValueVar) (pushError (InfoIllegal v ValueVar (varInfo v)))
      *> checkType s ty
    pure ((v, ty, e'), es)

  ((StmtLet (Many vs'), es):) <$> checkStmt s' xs

checkStmt s (Type v ctors:xs) = do
  let s' = s { vars = foldr (uncurry insertVar) (vars s) ctors
             , types = foldr insertTy (types s) (toVar v : map (toVar . fst) ctors)}

  es <- gatherError' . liftError $
    -- Ensure we're declaring a type
       unless (varInfo v == TypeConVar) (pushError (InfoIllegal v TypeConVar (varInfo v)))
    *> for_ ctors (\(v, x) ->
         -- Ensure we're declaring a constructor
         unless (varInfo v == DataConVar) (pushError (InfoIllegal v DataConVar (varInfo v)))
          -- Ensure the type is well formed
         *> checkType s' x)

  ((Type v ctors, es):) <$> checkStmt s' xs

checkAtom :: IsVar a
         => Scope a
         -> Atom a
         -> Errors (CoreErrors a) (Type a)
checkAtom s (Ref v ty) =
  case VarMap.lookup (toVar v) (vars s) of
    Nothing -> pushError (NoSuchVar v)
    Just (ty', inf')
      -- Ensure the types line up
      | ty `apart` ty' -> pushError (TypeMismatch ty' ty)
      -- Ensure the variable info lines up, and it's a valid variable
      | inf' /= varInfo v -> pushError (InfoMismatch v inf'(varInfo v))
      | not (isValueInfo inf') -> pushError (InfoIllegal v ValueVar inf')
      | otherwise -> pure ty
checkAtom _ (Lit l) = pure (litTy l)

checkTerm :: forall a m. (IsVar a, MonadWriter LintResult m)
         => Scope a
         -> Term a
         -> m (Maybe (Type a), AnnTerm (CoreErrors a) a)
checkTerm s (Atom a) = do
  res <- gatherError (liftError (checkAtom s a))
  pure (flip AnnAtom a <$> res)

checkTerm s (App f x) = do
  res <- gatherError $ do
    (f', x') <- liftError $ (,) <$> checkAtom s f <*> checkAtom s x
    case f' of
      ForallTy Irrelevant a r | a `uni` x' -> pure r
      _ -> chuckError (TypeMismatch (ForallTy Irrelevant x' unknownTyvar) f')
  pure ((\es -> AnnApp es f x) <$> res)

checkTerm s (Lam arg@(TermArgument a ty) bod) = do
  errs <- gatherError' . liftError $
    -- Ensure type is valid and we're declaring a value
       unless (varInfo a == ValueVar) (pushError (InfoIllegal a ValueVar (varInfo a)))
    *> checkType s ty

  (bty, bod') <- checkTerm (s { vars = insertVar a ty (vars s) }) bod
  pure ( ForallTy Irrelevant ty <$> bty
       , AnnLam errs arg bod')
checkTerm s (Lam arg@(TypeArgument a ty) bod) = do
  errs <- gatherError' . liftError $
    -- Ensure type is valid and we're declaring a tyvar
       unless (varInfo a == TypeVar) (pushError (InfoIllegal a TypeVar (varInfo a)))
    *> checkType s ty

  (bty, bod') <- checkTerm (s { tyVars = VarSet.insert (toVar a) (tyVars s) }) bod
  pure ( ForallTy (Relevant a) ty <$> bty
       , AnnLam errs arg bod')

checkTerm s (Let (One (v, ty, e)) r) = do
  (ty', e') <- checkTerm s e
  (tyr, r') <- checkTerm (s { vars = insertVar v ty (vars s) }) r

  es <- gatherError' . liftError $
    -- Ensure type is valid and we're declaring a value
    (case ty' of
       Just ty' | ty `apart` ty' -> pushError (TypeMismatch ty ty')
       _ -> pure ())
    *> unless (varInfo v == ValueVar) (pushError (InfoIllegal v ValueVar (varInfo v)))
    *> checkType s ty

  pure ( tyr, AnnLet es (One (v, ty, e')) r')

checkTerm s (Let (Many vs) r) = do
  let s' = s { vars = foldr (\(v, t, _) -> insertVar v t) (vars s) vs }

  (vs', es) <- forMA vs $ \(v, ty, e) -> do
    (ty', e') <- checkTerm s' e
    es <- gatherError' . liftError $
      -- Ensure type is valid and we're declaring a value
      (case ty' of
         Just ty' | ty `apart` ty' -> pushError (TypeMismatch ty ty')
         _ -> pure ())
      *> unless (varInfo v == ValueVar) (pushError (InfoIllegal v ValueVar (varInfo v)))
      *> checkType s ty
    pure ((v, ty, e'), es)

  (tyr, r') <- checkTerm s' r
  pure ( tyr, AnnLet es (Many vs') r')

checkTerm s (Match e bs) = do
  (tye, es) <- gatherError . liftError $ checkAtom s e

  -- Build up our arms and verify they are well formed
  (unzip -> (tys, bs'), es') <- forMA bs $
    \Arm { _armPtrn = p, _armTy = ty, _armBody = r, _armVars = vs, _armTyvars = tvs } -> do
      let pVars = patternVars p
      (tyr, r') <- checkTerm (s { vars = foldr (uncurry insertVar) (vars s) pVars
                                , tyVars = tyVars s <> foldMap (freeInTy . snd) pVars }) r
      es <- gatherError' . liftError $
        (case tye of
           Just tye | ty `apart` tye -> pushError (TypeMismatch ty tye)
           _ -> pure ())
        *> when (vs /= patternVars p) (pushError (PatternMismatch (patternVars p) vs))
        *> checkPattern s ty p
      pure ((tyr, Arm p ty r' vs tvs), es)

  -- Verify the types are consistent
  let ty = foldr1 (<|>) tys
  es'' <- gatherError' . liftError . for_ tys $ \ty' ->
    case (ty, ty') of
      (Just ty, Just ty') | ty `apart` ty' -> pushError (TypeMismatch ty ty')
      _ -> pure ()

  pure ( ty, AnnMatch (es <> es' <> es'') e bs')

checkTerm s (Extend f fs) = do
  res <- gatherError $ do
    tyf' <- liftError $
         for_ fs (\(_, ty, v) ->
           (\ty' -> if ty `apart` ty' then pushError (TypeMismatch ty ty') else pure ())
           <$> checkAtom s v)
      *> checkAtom s f

    pure $ case tyf' of
      RowsTy tau rs ->
        let updated = deleteFirstsBy ((==) `on` fst) rs tys
            tys = map (\(f, ty, _) -> (f, ty)) fs
            inner = case updated of
              [] -> tau
              xs -> RowsTy tau xs
         in RowsTy inner tys
      x -> RowsTy x . map (\(f, ty, _) -> (f, ty)) $ fs

  pure ((\es -> AnnExtend es f fs) <$> res)

checkTerm s (Values xs) = do
  (ty, es) <- gatherError . liftError $ for xs (checkAtom s)
  pure (ValuesTy <$> ty, AnnValues es xs)

checkTerm s (TyApp f x) = do
  res <- gatherError $ do
    (f', ()) <- liftError $ (,) <$> checkAtom s f <*> checkTypeBoxed s x
    case f' of
      ForallTy (Relevant a) _ ty -> pure (substituteInType (VarMap.singleton (toVar a) x) ty)
      _ -> chuckError (TypeMismatch (ForallTy (Relevant unknownVar) unknownTyvar unknownTyvar) f')

  pure ((\es -> AnnTyApp es f x) <$> res)

checkTerm s (Cast x co) = do
  res <- gatherError $ do
    (ty, (from, to)) <- liftError $ (,) <$> checkAtom s x <*> checkCoercion s co
    when (from `apart` ty) (chuckError (TypeMismatch from ty))
    pure to

  pure ((\es -> AnnCast es x co) <$> res)

-- | Verify a type is valid within the given scope
--
-- TODO: We should really verify the kinds match up, but we're a long way away
-- from that working
checkType :: IsVar a => Scope a -> Type a -> Errors (CoreErrors a) ()
checkType s (ConTy v) =
  case VarMap.lookup (toVar v) (types s) of
    Nothing -> pushError (NoSuchVar v)
    Just inf | not (isTypeInfo inf) -> pushError (InfoIllegal v TypeConVar inf)
             | otherwise -> pure ()
checkType _ (VarTy v)
  | varInfo v /= TypeVar = pushError (InfoIllegal v TypeVar (varInfo v))
  | otherwise = pure ()
checkType s (ForallTy Irrelevant a r) = checkType s a *> checkTypeBoxed s r
checkType s (ForallTy (Relevant vs) c v) =
  let s' = s { tyVars = VarSet.insert (toVar vs) (tyVars s) }
  in unless (varInfo vs == TypeVar) (pushError (InfoIllegal vs TypeVar (varInfo vs)))
  *> checkType s' c
  *> checkType s' v
checkType s (AppTy f x) = checkType s f *> checkTypeBoxed s x
checkType s (RowsTy f fs) = checkType s f *> traverse_ (checkTypeBoxed s . snd) fs
checkType s (ValuesTy xs) = traverse_ (checkTypeBoxed s) xs
checkType _ StarTy = pure ()
checkType _ NilTy = pure ()

checkTypeBoxed :: IsVar a => Scope a -> Type a -> Errors (CoreErrors a) ()
checkTypeBoxed s x = checkType s x *> checkNoUnboxed x

-- | Check a coercion is well formed within a given scope, returning the input
-- and output types.
--
-- Ideally this would use ApplicativeDo, but GHC is still a little silly at
-- desugaring it all.
checkCoercion :: IsVar a => Scope a -> Coercion a -> Errors (CoreErrors a) (Type a, Type a)
checkCoercion s = checkCo where
  checkCo (SameRepr a b) =
       checkType s a
    *> checkType s b
    $> (a, b)
  checkCo (Application l r) =
    (\(f, g) (x, y) -> (AppTy f x, AppTy g y))
    <$> checkCo l
    <*> checkCo r
  checkCo (ExactRecord rs) =
    bimap ExactRowsTy ExactRowsTy . unzip
    <$> for rs (\(t, c) -> (\(a, b) -> ((t, a), (t, b))) <$> checkCo c)
  checkCo (Record c rs) =
    (\(a, b) (as, bs) -> (RowsTy a as, RowsTy b bs))
    <$> checkCo c
    <*> fmap unzip (for rs (\(t, c) -> (\(a, b) -> ((t, a), (t, b))) <$> checkCo c))
  checkCo (Projection rs rs') =
    (\(as, bs) (ss, ts) ->
       let first = unionBy ((==) `on` fst) as ss
       in (ExactRowsTy first, RowsTy (ExactRowsTy bs) ts))
    <$> fmap unzip (for rs (\(t, c) -> (\(a, b) -> ((t, a), (t, b))) <$> checkCo c))
    <*> fmap unzip (for rs' (\(t, c) -> (\(a, b) -> ((t, a), (t, b))) <$> checkCo c))

  checkCo (CoercionVar x) =
    case VarMap.lookup (toVar x) (vars s) of
      Just (AppTy (AppTy _ l) r, _) -> pure (l, r)
      _ -> pure (VarTy (fromVar tyvarA), VarTy (fromVar tyvarB))

  checkCo (Nth co i) =
    case VarMap.lookup (toVar co) (vars s) of
      Just (ExactRowsTy rs, _) | (_, AppTy (AppTy _ l) r) <- rs !! i -> pure (l, r)
      _ -> pushError (InvalidCoercion (Nth co i))

  checkCo (Axiom ax i) =
    case VarMap.lookup (toVar ax) (vars s) of
      Just (pi, _) -> checkCoAx (Axiom ax i) pi i
      _ -> pushError (InvalidCoercion (Axiom ax i))

  checkCo (Symmetry x) = swap <$> checkCo x
  checkCo (Quantified v l r) =
    (\(f, g) (x, y) -> (ForallTy v f x, ForallTy v g y)) <$> checkCo l <*> checkCo r

  coAxT (ForallTy Irrelevant (AppTy (AppTy _ l) r) rest) = (l, r):coAxT rest
  coAxT (ForallTy Relevant{} _ rest) = coAxT rest
  coAxT (AppTy (AppTy _ l) r) = [(l, r)]
  coAxT x = error (show x)

  checkCoAx _ (coAxT -> ts) args =
    (VarTy (fromVar tyvarA), snd (last ts))
    -- TODO: This is quite sad. Can we fix it? See lint/tyfun-unsat.ml
      <$ traverse_ checkCo' (zip ts args)
  checkCo' ((a, b), co) = (\(l, r) -> if (a `apart` l) || (b `apart` r) then pushError (InvalidCoercion co) else pure ())
    <$> checkCo co

checkPattern :: forall a. IsVar a => Scope a -> Type a -> Pattern a -> Errors (CoreErrors a) ()
checkPattern s = checkPat where
  checkCapture :: Type a -> Capture a -> Errors (CoreErrors a) ()
  checkCapture ty' (Capture a ty)
    | ty `apartOpen` ty' = pushError (TypeMismatch ty ty')
    | varInfo a /= ValueVar = pushError (InfoIllegal a ValueVar (varInfo a))
    | otherwise = pure ()

  checkPat :: Type a -> Pattern a -> Errors (CoreErrors a) ()
  checkPat _ PatWildcard = pure mempty
  checkPat (RowsTy _ _) (PatLit RecNil) = pure mempty
  checkPat NilTy (PatLit RecNil) = pure mempty
  checkPat ty' (PatLit l) =
    let ty = litTy l
        ty :: Type a
    in when (ty `apart` ty') (pushError (TypeMismatch ty ty'))
    $> mempty
  checkPat ty' (Constr a) =
    case VarMap.lookup (toVar a) (vars s) of
      Nothing -> pushError (NoSuchVar a)
      Just (ty, inf)
        -- Ensure types line up
        | inst ty `apartOpen` ty' -> pushError (TypeMismatch ty' (inst ty))
        -- Ensure we're matching on a constructor
        | inf /= varInfo a -> pushError (InfoMismatch a inf (varInfo a))
        | inf /= DataConVar  -> pushError (InfoMismatch a DataConVar (varInfo a))
        | otherwise -> pure mempty
  checkPat ty' (Destr a p) =
    case VarMap.lookup (toVar a) (vars s) of
      Nothing -> pushError (NoSuchVar a)
      Just (ty, inf)
        -- Ensure we're matching on a constructor
        | inf /= varInfo a -> pushError (InfoMismatch a inf (varInfo a))
        | inf /= DataConVar  -> pushError (InfoMismatch a DataConVar (varInfo a))
        -- Ensure types line up
        | (qs, r) <- splitForallTy $ inst ty
        , Just s <- r `unify` ty'
        -> () <$ zipWithM checkCapture (map (substituteInType s) qs) p
        | otherwise -> pushError (TypeMismatch (ForallTy Irrelevant unknownTyvar ty') (inst ty))
  checkPat (RowsTy _ _) (PatRecord []) = pure mempty
  checkPat NilTy (PatRecord []) = pure mempty
  checkPat ty@(RowsTy NilTy ts) (PatRecord fs) =
    let outer = filter (\(l, _) -> any ((== l) . fst) fs) ts
    in for_ fs (\(t, p) -> case find ((==t) . fst) outer of
         Nothing -> pushError (TypeMismatch (RowsTy NilTy [(t, unknownTyvar)]) ty)
         Just (_, ty) -> checkCapture ty p)
  checkPat ty@(RowsTy ext ts) (PatRecord fs) =
    for_ fs (\(t, p) -> case find ((==t) . fst) ts of
      Nothing -> pushError (TypeMismatch (RowsTy ext [(t, unknownTyvar)]) ty)
      Just (_, ty) -> checkCapture ty p)
  checkPat t (PatRecord fs) =
    pushError (TypeMismatch (RowsTy unknownTyvar (map (\(a, _) -> (a, unknownTyvar)) fs)) t)
  checkPat (ValuesTy ts) (PatValues ps) | length ts == length ps =
    traverse_ (uncurry checkCapture) (zip ts ps)
  checkPat t (PatValues ps) =
    pushError (TypeMismatch (ValuesTy (replicate (length ps) unknownTyvar)) t)

  inst (ForallTy (Relevant _) _ t) = inst t
  inst t = t
  splitForallTy (ForallTy Irrelevant q t) = splitForallTy t & _1 %~ (q:)
  splitForallTy t = ([], t)

checkNoUnboxed :: Type a -> Errors (CoreErrors a) ()
checkNoUnboxed ValuesTy{} = pushError IllegalUnbox
checkNoUnboxed _ = pure ()

unknownVar :: IsVar a => a
unknownVar = fromVar (CoVar (-100) (Just "?") ValueVar)

unknownTyvar :: IsVar a => Type a
unknownTyvar = VarTy unknownVar

-- | Throw an error within an applicative
pushError :: CoreError a -> Errors (CoreErrors a) b
pushError = failure . pure

-- | Throw an error within an error monad
chuckError :: MonadError (CoreErrors a) m => CoreError a -> m b
chuckError = throwError . pure

-- | Lift an 'Errors' into an error monad
liftError :: MonadError (CoreErrors a) m => Errors (CoreErrors a) b -> m b
liftError m = case runErrors m of
                Left e -> throwError e
                Right x -> pure x

gatherError :: MonadWriter LintResult m => ExceptT (CoreErrors a) m b -> m (Maybe b, CoreErrors a)
gatherError m = do
  res <- runExceptT m
  case res of
    Left e -> tell (Sum (Seq.length e)) >> pure (Nothing, e)
    Right x -> pure (Just x, mempty)

gatherError' :: MonadWriter LintResult m => ExceptT (CoreErrors a) m () -> m (CoreErrors a)
gatherError' = fmap snd . gatherError

forMA :: (Monoid m, Applicative f) => [a] -> (a -> f (b, m)) -> f ([b], m)
forMA vs f = foldr (\x a ->
                      (\(a, b) (a', b') -> (a : a', b <> b'))
                      <$> f x <*> a) (pure mempty) vs

litTy :: IsVar a => Literal -> Type a
litTy (Int _) = fromVar <$> tyInt
litTy (Str _) = fromVar <$> tyString
litTy (Float _) = fromVar <$> tyFloat
litTy LitTrue = fromVar <$> tyBool
litTy LitFalse = fromVar <$> tyBool
litTy Unit = fromVar <$> tyUnit
litTy RecNil = NilTy

uni, apart, uniOpen, apartOpen :: IsVar a => Type a -> Type a -> Bool
uni = uniOpen
apart a b = not (uni a b)
uniOpen a b = isJust (unify a b)
apartOpen a b = not (uniOpen a b)

patternVars :: Pattern a -> [(a, Type a)]
patternVars (Destr _ p) = map captureVars p
patternVars (PatRecord ps) = map (captureVars . snd) ps
patternVars (PatValues ps) = map captureVars ps
patternVars Constr{} = []
patternVars PatLit{} = []
patternVars PatWildcard{} = []

captureVars :: Capture a -> (a, Type a)
captureVars (Capture v ty) = (v, ty)
