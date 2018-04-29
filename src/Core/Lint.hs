{-# LANGUAGE FlexibleContexts, OverloadedStrings, ExistentialQuantification, ScopedTypeVariables #-}
module Core.Lint
  ( runLint, runLintOK, emptyScope
  , checkStmt, checkTerm, checkAtom, checkType
  ) where

import Control.Monad.Writer
import Control.Monad.Except

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.VarSet (IsVar(..))
import Data.Traversable
import Data.Foldable
import Data.Function
import Data.Text ()
import Data.Maybe
import Data.List

import Core.Optimise
import Core.Builtin
import Core.Types
import Syntax (Var(..))
import Pretty hiding ((<>))

data CoreError a = TypeMismatch (Type a) (Type a)
                 | forall b. Pretty b => ArisingIn (CoreError a) b
                 | NoSuchVar a
                 | InvalidCoercion (Coercion a)
                 | PatternMismatch [(a, Type a)] [(a, Type a)]

data Scope a = Scope { vars :: Map.Map a (Type a)
                     , types :: Set.Set a
                     , tyVars :: Set.Set a }
  deriving (Show)

emptyScope :: IsVar a => Scope a
emptyScope = Scope (Map.fromList builtinVarList) (Set.fromList builtinTyList) mempty

instance Pretty a => Pretty (CoreError a) where
  pretty (TypeMismatch l r) = text "Expected type" <+> pretty l </> text "     got type" <+> pretty r
  pretty (ArisingIn e c) = pretty e </> text "arising in " <+> pretty c
  pretty (NoSuchVar a) = text "No such variable " <+> pretty a
  pretty (InvalidCoercion a) = text "Illegal coercion" <+> pretty a
  pretty (PatternMismatch l r) = text "Expected vars" <+> pVs l </> text "     got vars" <+> pVs r
    where pVs = hsep . punctuate comma . map (\(v, ty) -> pretty v <+> colon <+> pretty ty)

  prettyList = vsep . map pretty

runLint :: (IsVar a, Pretty b)
        => ExceptT (CoreError a) (Writer [CoreError a]) ()
        -> b -> b
runLint m a =
  let (r, es) = runWriter (runExceptT m)
      es' = case r of
              Left e -> e:es
              Right _ -> es
  in case es' of
       [] -> a
       es' -> error $ renderDetailed $ string "Core lint failed:" <#>
                                       prettyList es' <#>
                                       string "for term" <#>
                                       pretty a

runLintOK :: IsVar a => ExceptT (CoreError a) (Writer [CoreError a]) () -> Bool
runLintOK m = case runWriter (runExceptT m) of
                     (Right _, []) -> True
                     _ -> False

checkStmt :: (IsVar a, MonadError (CoreError a) m, MonadWriter [CoreError a] m)
          => Scope a
          -> [Stmt a]
          -> m ()
checkStmt _ [] = pure ()
checkStmt s (Foreign v ty _:xs) = checkStmt (s { vars = Map.insert v ty (vars s) }) xs
checkStmt s t@(StmtLet vs:xs) = do
  let s' = s { vars = foldr (\(v, t, _) -> Map.insert v t) (vars s) vs }
  for_ vs $ \(_, ty, e) -> tryContext t $ do
    ty' <- checkTerm s' e
    if ty `apart` ty' then throwError (TypeMismatch ty ty') else pure ()

  checkStmt s' xs
checkStmt s t@(Type v ctors:xs) = do
  let s' = s { vars = foldr (uncurry Map.insert) (vars s) ctors
             , types = foldr Set.insert (types s) (v : map fst ctors)}
  for_ ctors $ \(_, x) -> checkType s' x `withContext` t

  checkStmt s' xs

checkAtom :: (IsVar a, MonadError (CoreError a) m, MonadWriter [CoreError a] m)
         => Scope a
         -> Atom a
         -> m (Type a)
checkAtom s a@(Ref v ty) =
  case Map.lookup v (vars s) of
    Nothing -> throwError (NoSuchVar v)
    Just ty' | ty `apart` ty' -> throwError (TypeMismatch ty' ty) `withContext` a
             | otherwise -> pure ty
checkAtom _ (Lit l) = pure (litTy l)
checkAtom s l@(Lam (TermArgument a ty) bod) = do
  checkType s ty
  bty <- checkTerm (s { vars = Map.insert a ty (vars s) }) bod `withContext` l
  pure (ForallTy Irrelevant ty bty)
checkAtom s l@(Lam (TypeArgument a ty) bod) = do
  checkType s ty
  bty <- checkTerm (s { tyVars = Set.insert a (tyVars s) }) bod `withContext` l
  pure (ForallTy (Relevant a) ty bty)

checkTerm :: forall a m. (IsVar a, MonadError (CoreError a) m, MonadWriter [CoreError a] m)
         => Scope a
         -> Term a
         -> m (Type a)
checkTerm s (Atom a) = checkAtom s a
checkTerm s (App f x) = do
  f' <- checkAtom s f
  x' <- checkAtom s x
  case f' of
    ForallTy Irrelevant a r | a `uni` x' -> pure r
    _ -> throwError (TypeMismatch (ForallTy Irrelevant x' unknownTyvar) f')
checkTerm s t@(Let (One (v, ty, e)) r) = do
  tryContext t $ do
    ty' <- checkTerm s e
    if ty `apart` ty' then throwError (TypeMismatch ty ty') else pure ()

  checkTerm (s { vars = Map.insert v ty (vars s) }) r
checkTerm s t@(Let (Many vs) r) = do
  let s' = s { vars = foldr (\(v, t, _) -> Map.insert v t) (vars s) vs }
  for_ vs $ \(_, ty, e) -> tryContext t $ do
    ty' <- checkTerm s' e
    if ty `apart` ty' then throwError (TypeMismatch ty ty') else pure ()

  checkTerm s' r

checkTerm s t@(Match e bs) = flip withContext t $ do
  tye <- checkAtom s e
  (ty:tys) <- for bs $ \Arm { armPtrn = p, armTy = ty, armBody = r, armVars = vs } -> do
    if vs /= patternVars p
    then tell [ArisingIn (PatternMismatch (patternVars p) vs) t]
    else pure ()

    pVars <- checkPattern ty p `withContext` p
    if ty `apart` tye
    then throwError (TypeMismatch ty tye)
    else checkTerm (s { vars = Map.union pVars (vars s), tyVars = tyVars s <> foldMap freeInTy pVars }) r

  -- Ensure all types are consistent
  foldrM (\ty ty' -> if ty `apart` ty'
                     then throwError (TypeMismatch ty ty')
                     else pure ty') ty tys
    where
      checkPattern :: Type a -> Pattern a -> m (Map.Map a (Type a))
      checkPattern ty' (Capture a ty)
        | Just{} <- ty `unify` ty' = pure (Map.singleton a ty)
        | otherwise = throwError (TypeMismatch ty ty')
      checkPattern (RowsTy _ _) (PatLit RecNil) = pure mempty
      checkPattern (ExactRowsTy _) (PatLit RecNil) = pure mempty
      checkPattern ty' (PatLit l) = do
        let ty = litTy l
        if ty `apart` ty'
        then throwError (TypeMismatch ty ty')
        else pure mempty
      checkPattern ty' (Constr a) =
        case Map.lookup a (vars s) of
          Nothing -> throwError (NoSuchVar a)
          Just ty | inst ty `uniOpen` ty' -> pure mempty
                  | otherwise -> throwError (TypeMismatch ty' (inst ty))
      checkPattern ty' (Destr a p) =
        case Map.lookup a (vars s) of
          Nothing -> throwError (NoSuchVar a)
          Just ty | ForallTy Irrelevant x r <- inst ty
                  , Just s <- r `unify` ty'
                  -- TODO: Do we need Relevant as well?
                  -> checkPattern (substituteInType s x) p
                  | otherwise -> throwError (TypeMismatch (ForallTy Irrelevant unknownTyvar ty') (inst ty))
      checkPattern ty@(RowsTy ext ts) (PatExtend f fs) = do
        v <- checkPattern ext f
        vs <- for fs $ \(t, p) ->
          case find ((==t) . fst) ts of
            Nothing -> throwError (TypeMismatch (RowsTy ext [(t, unknownTyvar)]) ty)
            Just (_, ty) -> checkPattern ty p

        pure (mconcat (v:vs))

      checkPattern ty@(ExactRowsTy ts) (PatExtend (PatLit RecNil) fs) =
        fmap mconcat . for fs $ \(t, p) ->
          case find ((==t) . fst) ts of
            Nothing -> throwError (TypeMismatch (ExactRowsTy [(t, unknownTyvar)]) ty)
            Just (_, ty) -> checkPattern ty p

      checkPattern t p@PatExtend{} = error ("extend pattern " ++ show p ++ " for type " ++ show t)

      inst (ForallTy (Relevant _) _ t) = inst t
      inst t = t

checkTerm s t@(Extend f fs) = do
  tyf' <- checkAtom s f
  for_ fs $ \(_, ty, v) -> tryContext t $ do
    ty' <- checkAtom s v
    if ty `apart` ty' then throwError (TypeMismatch ty ty') else pure ()

  case tyf' of
    ExactRowsTy fs' -> pure $ ExactRowsTy $ nubBy (on (==) fst) $ map (\(f, ty, _) -> (f, ty)) fs ++ fs'
    RowsTy f' fs' -> pure $ RowsTy f' $ nubBy (on (==) fst) $ map (\(f, ty, _) -> (f, ty)) fs ++ fs'
    _ -> throwError (TypeMismatch (ExactRowsTy []) tyf') `withContext` t

checkTerm s t@(TyApp f x) = do
  f' <- checkAtom s f
  checkType s x `withContext` t
  case f' of
    ForallTy (Relevant a) _ ty -> pure (substituteInType (Map.singleton a x) ty)
    _ -> throwError (TypeMismatch (ForallTy (Relevant unknownVar) unknownTyvar unknownTyvar) f') `withContext` t

checkTerm s t@(Cast x co) = do
  ty <- checkAtom s x

  (from, to) <- checkCo co
  tryContext t $
    if from `apart` ty then throwError (TypeMismatch from ty) else pure ()

  pure to where
    checkCo (SameRepr a b) = do
      checkType s a
      checkType s b
      pure (a, b)
    checkCo (Application l r) = do
      (f, g) <- checkCo l
      (x, y) <- checkCo r
      pure (AppTy f x, AppTy g y)
    checkCo (ExactRecord rs) = do
      (as, bs) <- fmap unzip . for rs $ \(t, c) -> do
        (a, b) <- checkCo c
        pure ((t, a), (t, b))
      pure (ExactRowsTy as, ExactRowsTy bs)
    checkCo (Record c rs) = do
      (as, bs) <- fmap unzip . for rs $ \(t, c) -> do
        (a, b) <- checkCo c
        pure ((t, a), (t, b))
      (a, b) <- checkCo c
      pure (RowsTy a as, RowsTy b bs)
    checkCo (Projection rs rs') = do
      (as, bs) <- fmap unzip . for rs $ \(t, c) -> do
        (a, b) <- checkCo c
        pure ((t, a), (t, b))
      (ss, ts) <- fmap unzip . for rs' $ \(t, c) -> do
        (a, b) <- checkCo c
        pure ((t, a), (t, b))
      let first = unionBy ((==) `on` fst) as ss
      pure (ExactRowsTy first, RowsTy (ExactRowsTy bs) ts)
    checkCo co@CoercionVar{} = throwError (InvalidCoercion co)
    checkCo (Symmetry x) = do
      (a,  b) <- checkCo x
      pure (b, a)
    checkCo (Domain x) = do
      (f, t) <- checkCo x
      f' <- case f of
              ForallTy _ a _ -> pure a
              _ -> throwError (TypeMismatch (ForallTy Irrelevant unknownTyvar unknownTyvar) t)
      t' <- case t of
              ForallTy _ a _ -> pure a
              _ -> throwError (TypeMismatch (ForallTy Irrelevant unknownTyvar unknownTyvar) t)
      pure (f', t')
    checkCo (Codomain x) = do
      (f, t) <- checkCo x
      f' <- case f of
              ForallTy _ _ a -> pure a
              _ -> throwError (TypeMismatch (ForallTy Irrelevant unknownTyvar unknownTyvar) t)
      t' <- case t of
              ForallTy _ _ a -> pure a
              _ -> throwError (TypeMismatch (ForallTy Irrelevant unknownTyvar unknownTyvar) t)
      pure (f', t')
    checkCo (Quantified v l r) = do
      (f, g) <- checkCo l
      (x, y) <- checkCo r
      pure (ForallTy v f x, ForallTy v g y)


-- TODO: We should really verify the kinds match up, but we're a long way
-- away from that working
checkType :: (IsVar a, MonadError (CoreError a) m)
         => Scope a
         -> Type a
         -> m ()
checkType s (ConTy v) = if v `Set.member` types s
                       then pure ()
                       else throwError (NoSuchVar v)
checkType _ (VarTy _) = pure ()
checkType s (ForallTy Irrelevant a r) = checkType s a >> checkType s r
checkType s (ForallTy (Relevant vs) c v) =
  let s' = s { tyVars = Set.insert vs (tyVars s) }
  in checkType s' c >> checkType s' v
checkType s (AppTy f x) = checkType s f >> checkType s x
checkType s (RowsTy f fs) = checkType s f >> traverse_ (checkType s . snd) fs
checkType s (ExactRowsTy fs) = traverse_ (checkType s . snd) fs
checkType _ StarTy = pure ()

unknownVar :: IsVar a => a
unknownVar = fromVar (TgInternal "?")

unknownTyvar :: IsVar a => Type a
unknownTyvar = VarTy unknownVar

tryContext :: (IsVar a, Pretty b, MonadError (CoreError a) m, MonadWriter [CoreError a] m)
            => b -> m () -> m ()
tryContext c m = m `catchError` (tell . pure . wrapContext c)

withContext :: (IsVar a, Pretty b, MonadError (CoreError a) m, MonadWriter [CoreError a] m)
            => m c -> b -> m c
withContext m c = m `catchError` (throwError . wrapContext c)

wrapContext :: Pretty b => b -> CoreError a -> CoreError a
wrapContext _ e@ArisingIn{} = e
wrapContext c e = ArisingIn e c

litTy :: IsVar a => Literal -> Type a
litTy (Int _) = fromVar <$> tyInt
litTy (Str _) = fromVar <$> tyString
litTy (Float _) = fromVar <$> tyFloat
litTy LitTrue = fromVar <$> tyBool
litTy LitFalse = fromVar <$> tyBool
litTy Unit = fromVar <$> tyUnit
litTy RecNil = ExactRowsTy []

uni, apart, uniOpen :: IsVar a => Type a -> Type a -> Bool
uni = unifyClosed
apart a b = not (uni a b)
uniOpen a b = isJust (unify a b)

patternVars :: Pattern a -> [(a, Type a)]
patternVars (Capture v ty) = [(v, ty)]
patternVars (Destr _ p) = patternVars p
patternVars (PatExtend p ps) = patternVars p ++ concatMap (patternVars . snd) ps
patternVars Constr{} = []
patternVars PatLit{} = []
