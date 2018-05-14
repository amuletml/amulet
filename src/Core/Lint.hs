{-# LANGUAGE FlexibleContexts, ExistentialQuantification, ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Core.Lint
  ( CoreError
  , runLint, runLintOK, emptyScope
  , checkStmt, checkTerm, checkAtom, checkType
  ) where

import Control.Monad.Writer
import Control.Monad.Except

import qualified Data.VarMap as VarMap
import qualified Data.VarSet as VarSet
import Data.Traversable
import Data.Foldable
import Data.Function
import Data.Text ()
import Data.Maybe
import Data.List

import Core.Optimise
import Core.Builtin
import Core.Types
import Core.Arity (lamArity, lamArity')
import Pretty hiding ((<>))

data CoreError a
  = TypeMismatch (Type a) (Type a)
  | InfoMismatch a VarInfo VarInfo
  | InfoIllegal a VarInfo VarInfo
  | forall b. Pretty b => ArisingIn (CoreError a) b
  | NoSuchVar a
  | InvalidCoercion (Coercion a)
  | PatternMismatch [(a, Type a)] [(a, Type a)]

data Scope a = Scope { vars :: VarMap.Map (Type a, VarInfo)
                     , types :: VarMap.Map VarInfo
                     , tyVars :: VarSet.Set }
  deriving (Show)

data Context = Vanilla | Tail
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
  pretty (ArisingIn e c) = pretty e </> text "arising in" <+> pretty c
  pretty (NoSuchVar a) = text "No such variable" <+> pretty a
  pretty (InvalidCoercion a) = text "Illegal coercion" <+> pretty a
  pretty (PatternMismatch l r) = text "Expected vars" <+> pVs l </>
                                 text "     got vars" <+> pVs r
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

runLintOK :: IsVar a => ExceptT (CoreError a) (Writer [CoreError a]) () -> Either [CoreError a] ()
runLintOK m = case runWriter (runExceptT m) of
                     (Right _, []) -> Right ()
                     (Right _, es) -> Left es
                     (Left e, es) -> Left (es ++ [e])

checkStmt :: (IsVar a, MonadError (CoreError a) m, MonadWriter [CoreError a] m)
          => Scope a
          -> [Stmt a]
          -> m ()
checkStmt _ [] = pure ()
checkStmt s t@(Foreign v ty _:xs) = do
  tryContext t $ do
    -- Ensure we're declaring a value
    unless (varInfo v == ValueVar) (throwError (InfoIllegal v ValueVar (varInfo v)))
    -- And the type is well formed
    checkType s ty

  checkStmt (s { vars = insertVar v ty (vars s) }) xs

checkStmt s t@(StmtLet vs:xs) = do
  let s' = s { vars = foldr (\(v, t, _) -> insertVar v t) (vars s) vs }
  for_ vs $ \(v, ty, e) -> tryContext t $ do
    -- Ensure we're declaring a value
    unless (varInfo v == ValueVar) (throwError (InfoIllegal v ValueVar (varInfo v)))
    -- And the type is well formed
    checkType s ty
    -- And the definition matches the expected type
    ty' <- checkTerm Tail s' e
    if ty `apart` ty' then throwError (TypeMismatch ty ty') else pure ()

  checkStmt s' xs
checkStmt s t@(Type v ctors:xs) = do
  tryContext t $ do
    -- Ensure we're declaring a type
    unless (varInfo v == TypeConVar) (throwError (InfoIllegal v TypeConVar (varInfo v)))

  let s' = s { vars = foldr (uncurry insertVar) (vars s) ctors
             , types = foldr insertTy (types s) (toVar v : map (toVar . fst) ctors)}
  tryContext t $ for_ ctors $ \(v, x) -> do
     -- Ensure we're declaring a constructor
    unless (varInfo v == DataConVar) (throwError (InfoIllegal v DataConVar (varInfo v)))
    -- Ensure the type is well formed
    checkType s' x

  checkStmt s' xs

checkAtom :: (IsVar a, MonadError (CoreError a) m, MonadWriter [CoreError a] m)
         => Scope a
         -> Atom a
         -> m (Type a)
checkAtom s a@(Ref v ty) =
  case VarMap.lookup (toVar v) (vars s) of
    Nothing -> throwError (NoSuchVar v)
    Just (ty', inf')
      -- Ensure the types line up
      | ty `apart` ty' -> throwError (TypeMismatch ty' ty) `withContext` a
      -- Ensure the variable info lines up, and it's a valid variable
      | inf' /= varInfo v -> throwError (InfoMismatch v inf'(varInfo v))
      | not (isValueInfo inf') -> throwError (InfoIllegal v ValueVar inf')
      | otherwise -> pure ty
checkAtom _ (Lit l) = pure (litTy l)
checkAtom s l@(Lam (TermArgument a ty) bod) = do
  tryContext l $ do
    -- Ensure type is valid and we're declaring a value
    unless (varInfo a == ValueVar) (throwError (InfoIllegal a ValueVar (varInfo a)))
    checkType s ty

  bty <- checkTerm Tail (s { vars = insertVar a ty (vars s) }) bod `withContext` l
  pure (ForallTy Irrelevant ty bty)
checkAtom s l@(Lam (TypeArgument a ty) bod) = do
  tryContext l $ do
    -- Ensure type is valid and we're declaring a tyvar
    unless (varInfo a == TypeVar) (throwError (InfoIllegal a TypeVar (varInfo a)))
    checkType s ty

  bty <- checkTerm Tail (s { tyVars = VarSet.insert (toVar a) (tyVars s) }) bod `withContext` l
  pure (ForallTy (Relevant a) ty bty)

checkJoinAtom :: (IsVar a, MonadError (CoreError a) m, MonadWriter [CoreError a] m)
              => Scope a
              -> Atom a
              -> m (Type a)
checkJoinAtom s a@(Ref v ty) =
  case VarMap.lookup (toVar v) (vars s) of
    Nothing -> throwError (NoSuchVar v)
    Just (ty', inf')
      -- Ensure the types line up
      | ty `apart` ty' -> throwError (TypeMismatch ty' ty) `withContext` a
      -- Ensure the variable info lines up, and it's a valid variable
      | inf' /= varInfo v -> throwError (InfoMismatch v inf' (varInfo v))
      | not (isJoinInfo inf') -> throwError (InfoIllegal v (JoinVar (-1)) inf')
      | otherwise -> pure ty
checkJoinAtom _ _ = throwError (InfoIllegal unknownVar (JoinVar (-1)) ValueVar)


checkTerm :: forall a m. (IsVar a, MonadError (CoreError a) m, MonadWriter [CoreError a] m)
          => Context
          ->  Scope a
          -> Term a
          -> m (Type a)

-- If we're in a tail position, verify the join point has arity 1
checkTerm Tail s (App fa@(Ref f _) x)
  | CoVar _ _ (JoinVar a) <- toVar f = do
      -- The arity in a tail position must be 1
      unless (a == 1) (throwError (InfoMismatch f (JoinVar 1) (JoinVar a)))

      f' <- checkJoinAtom s fa
      x' <- checkAtom s x
      case f' of
        ForallTy Irrelevant a r | a `uni` x' -> pure r
        _ -> throwError (TypeMismatch (ForallTy Irrelevant x' unknownTyvar) f')

checkTerm Tail s t@(TyApp fa@(Ref f _) x)
  | CoVar _ _ (JoinVar a) <- toVar f = do
      -- The arity in a tail position must be 1
      unless (a == 1) (throwError (InfoMismatch f (JoinVar 1) (JoinVar a)))

      f' <- checkJoinAtom s fa
      checkType s x `withContext` t
      case f' of
        ForallTy (Relevant a) _ ty -> pure (substituteInType (VarMap.singleton (toVar a) x) ty)
        _ -> throwError (TypeMismatch (ForallTy (Relevant unknownVar) unknownTyvar unknownTyvar) f') `withContext` t

checkTerm c s t@(Let JoinBind (One (v, ty, e)) r)
  | CoVar _ _ (JoinVar a) <- toVar v
  = do
      tryContext t $ do
        checkType s ty

        (ty', op, ea) <- case e of
          Atom at@(Ref f _)
            | CoVar _ _ (JoinVar a') <- toVar f -> (,(==),a') <$> checkJoinAtom s at
          Atom at@Lam{} -> (,(<=),lamArity at) <$> checkAtom s at
          App fa@(Ref f _) x
            | CoVar _ _ (JoinVar a') <- toVar f -> do
                f' <- checkJoinAtom s fa
                x' <- checkAtom s x
                case f' of
                  ForallTy Irrelevant a r | a `uni` x' -> pure (r, (==), a' - 1)
                  _ -> throwError (TypeMismatch (ForallTy Irrelevant x' unknownTyvar) f')
          TyApp fa@(Ref f _) x
            | CoVar _ _ (JoinVar a') <- toVar f -> do
                f' <- checkJoinAtom s fa
                checkType s x `withContext` t
                case f' of
                  ForallTy (Relevant a) _ ty -> pure (substituteInType (VarMap.singleton (toVar a) x) ty, (==), a' - 1)
                  _ -> throwError (TypeMismatch (ForallTy (Relevant unknownVar) unknownTyvar unknownTyvar) f') `withContext` t
          _ -> throwError (InfoIllegal v (JoinVar (-1)) (JoinVar a))

        unless (a `op` ea) (throwError (InfoMismatch v (JoinVar ea) (JoinVar a)))
        if ty `apart` ty' then throwError (TypeMismatch ty ty') else pure ()

      checkTerm c (s { vars = insertVar v ty (vars s) }) r

checkTerm _ s (Atom a) = checkAtom s a
checkTerm _ s (App f x) = do
  f' <- checkAtom s f
  x' <- checkAtom s x
  case f' of
    ForallTy Irrelevant a r | a `uni` x' -> pure r
    _ -> throwError (TypeMismatch (ForallTy Irrelevant x' unknownTyvar) f')
checkTerm c s t@(Let k (One (v, ty, e)) r) = do
  tryContext t $ do
    -- Ensure type is valid and we're declaring the appropriate variable
    case k of
      ValueBind | ValueVar <- varInfo v -> pure ()
                | otherwise -> throwError (InfoIllegal v ValueVar (varInfo v))
      JoinBind | JoinVar a <- varInfo v
               , a <= lamArity' e -> pure ()
               | otherwise -> throwError (InfoIllegal v (JoinVar (lamArity' e)) (varInfo v))
    checkType s ty

    ty' <- checkTerm Vanilla s e
    if ty `apart` ty' then throwError (TypeMismatch ty ty') else pure ()

  checkTerm c (s { vars = insertVar v ty (vars s) }) r
checkTerm c s t@(Let k (Many vs) r) = do
  let s' = s { vars = foldr (\(v, t, _) -> insertVar v t) (vars s) vs }
  for_ vs $ \(v, ty, e) -> tryContext t $ do
    -- Ensure type is valid and we're declaring a value
    case k of
      ValueBind | ValueVar <- varInfo v -> pure ()
                | otherwise -> throwError (InfoIllegal v ValueVar (varInfo v))
      JoinBind | JoinVar a <- varInfo v
               , a <= lamArity' e -> pure ()
               | otherwise -> throwError (InfoIllegal v (JoinVar (lamArity' e)) (varInfo v))
    checkType s ty

    ty' <- checkTerm Vanilla s' e
    if ty `apart` ty' then throwError (TypeMismatch ty ty') else pure ()

  checkTerm c s' r

checkTerm c s t@(Match e bs) = flip withContext t $ do
  tye <- checkAtom s e
  (ty:tys) <- for bs $ \Arm { _armPtrn = p, _armTy = ty, _armBody = r, _armVars = vs } -> do
    if vs /= patternVars p
    then tell [ArisingIn (PatternMismatch (patternVars p) vs) t]
    else pure ()

    pVars <- checkPattern ty p `withContext` p
    if ty `apart` tye
    then throwError (TypeMismatch ty tye)
    else checkTerm c (s { vars = VarMap.union pVars (vars s), tyVars = tyVars s <> foldMap (freeInTy . fst) pVars }) r

  -- Ensure all types are consistent
  foldrM (\ty ty' -> if ty `apart` ty'
                     then throwError (TypeMismatch ty ty')
                     else pure ty') ty tys
    where
      checkPattern :: Type a -> Pattern a -> m (VarMap.Map (Type a, VarInfo))
      checkPattern ty' (Capture a ty)
        | ty `apartOpen` ty' = throwError (TypeMismatch ty ty')
        | varInfo a /= ValueVar = throwError (InfoIllegal a ValueVar (varInfo a))
        | otherwise = pure (VarMap.singleton (toVar a) (ty, varInfo a))
      checkPattern (RowsTy _ _) (PatLit RecNil) = pure mempty
      checkPattern (ExactRowsTy _) (PatLit RecNil) = pure mempty
      checkPattern ty' (PatLit l) = do
        let ty = litTy l
        if ty `apart` ty'
        then throwError (TypeMismatch ty ty')
        else pure mempty
      checkPattern ty' (Constr a) =
        case VarMap.lookup (toVar a) (vars s) of
          Nothing -> throwError (NoSuchVar a)
          Just (ty, inf)
            -- Ensure types line up
            | inst ty `apartOpen` ty' -> throwError (TypeMismatch ty' (inst ty))
            -- Ensure we're matching on a constructor
            | inf /= varInfo a -> throwError (InfoMismatch a inf (varInfo a))
            | inf /= DataConVar  -> throwError (InfoMismatch a DataConVar (varInfo a))
            | otherwise -> pure mempty
      checkPattern ty' (Destr a p) =
        case VarMap.lookup (toVar a) (vars s) of
          Nothing -> throwError (NoSuchVar a)
          Just (ty, inf)
            -- Ensure we're matching on a constructor
            | inf /= varInfo a -> throwError (InfoMismatch a inf (varInfo a))
            | inf /= DataConVar  -> throwError (InfoMismatch a DataConVar (varInfo a))
            -- Ensure types line up
            | ForallTy Irrelevant x r <- inst ty
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

checkTerm _ s t@(Extend f fs) = do
  tyf' <- checkAtom s f
  for_ fs $ \(_, ty, v) -> tryContext t $ do
    ty' <- checkAtom s v
    if ty `apart` ty' then throwError (TypeMismatch ty ty') else pure ()

  case tyf' of
    ExactRowsTy [] -> pure . ExactRowsTy $ map (\(f, ty, _) -> (f, ty)) fs
    _-> pure . RowsTy tyf' $ map (\(f, ty, _) -> (f, ty)) fs

checkTerm _ s t@(TyApp f x) = do
  f' <- checkAtom s f
  checkType s x `withContext` t
  case f' of
    ForallTy (Relevant a) _ ty -> pure (substituteInType (VarMap.singleton (toVar a) x) ty)
    _ -> throwError (TypeMismatch (ForallTy (Relevant unknownVar) unknownTyvar unknownTyvar) f') `withContext` t

checkTerm _ s t@(Cast x co) = do
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
checkType s (ConTy v) =
  case VarMap.lookup (toVar v) (types s) of
    Nothing -> throwError (NoSuchVar v)
    Just inf | inf /= varInfo v -> throwError (InfoMismatch v inf (varInfo v))
             | not (isTypeInfo inf) -> throwError (InfoIllegal v TypeConVar inf)
             | otherwise -> pure ()
checkType _ (VarTy v)
  | varInfo v /= TypeVar = throwError (InfoIllegal v TypeVar (varInfo v))
  | otherwise = pure ()
checkType s (ForallTy Irrelevant a r) = checkType s a >> checkType s r
checkType s (ForallTy (Relevant vs) c v) = do
  unless (varInfo vs == TypeVar) (throwError (InfoIllegal vs TypeVar (varInfo vs)))
  let s' = s { tyVars = VarSet.insert (toVar vs) (tyVars s) }
  checkType s' c >> checkType s' v
checkType s (AppTy f x) = checkType s f >> checkType s x
checkType s (RowsTy f fs) = checkType s f >> traverse_ (checkType s . snd) fs
checkType s (ExactRowsTy fs) = traverse_ (checkType s . snd) fs
checkType _ StarTy = pure ()

unknownVar :: IsVar a => a
unknownVar = fromVar (CoVar (-100) "?" ValueVar)

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

uni, apart, uniOpen, apartOpen :: IsVar a => Type a -> Type a -> Bool
uni = unifyClosed
apart a b = not (uni a b)
uniOpen a b = isJust (unify a b)
apartOpen a b = not (uniOpen a b)

patternVars :: Pattern a -> [(a, Type a)]
patternVars (Capture v ty) = [(v, ty)]
patternVars (Destr _ p) = patternVars p
patternVars (PatExtend p ps) = patternVars p ++ concatMap (patternVars . snd) ps
patternVars Constr{} = []
patternVars PatLit{} = []
