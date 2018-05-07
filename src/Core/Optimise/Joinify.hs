{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
module Core.Optimise.Joinify where

import Control.Monad.Gen
import Control.Lens

import qualified Data.VarMap as VarMap
import qualified Data.VarSet as VarSet
import Data.Triple

import Core.Optimise
import Core.Types
import Core.Arity (lamArity')

import Debug.Trace
import Pretty

data JoinConversion = Preserve | JoinOne Int | JoinMany [Int]
  deriving (Show, Eq)

matchJoinPass :: forall m a. (MonadGen Int m, IsVar a) => [Stmt a] -> m [Stmt a]
matchJoinPass = traverse transS where
  transS (StmtLet vars) = StmtLet <$> traverse (third3A (transT mempty False . gatherJoinPass)) vars
  transS s = pure s

  transA :: VarMap.Map Int -> AnnAtom JoinConversion a -> m (Atom a)
  transA s (Ref v t) =
    let v'@(CoVar i name _) = toVar v
    in pure (maybe (Ref v t) (flip Ref t . fromVar . CoVar i name . JoinVar) (VarMap.lookup v' s))
  transA _ (Lit l) = pure (Lit l)
  transA s (Lam arg b) = Lam arg <$> transT s True b

  transT :: VarMap.Map Int -> Bool -> AnnTerm JoinConversion a -> m (Term a)
  transT s _ (AnnAtom _ a) = Atom <$> transA s a
  transT s _ (AnnApp _ f a) = App <$> transA s f <*> transA s a
  transT s _ (AnnTyApp _ f t) = flip TyApp t <$> transA s f
  transT s _ (AnnCast _ f t) = flip Cast t <$> transA s f

  -- Lambda to join point promotion
  transT s True (AnnLet (JoinOne a) ValueBind (One (v, ty, e)) r) = do
    let v' = toVar v
        s' = VarMap.insert v' a s
    e' <- transT s False e
    Let JoinBind (One (fromVar (set covarInfo (JoinVar a) v'), ty, e')) <$> transT s' True r
  transT s True (AnnLet (JoinMany as) ValueBind (Many vs) r) = do
    let s' = foldr (uncurry (VarMap.insert . toVar . fst3)) s (zip vs as)
    es' <- traverse (transT s' False . thd3) vs
    let vs' = zipWith3 (\(v, ty, _) a e -> (fromVar (set covarInfo (JoinVar a) (toVar v)), ty, e)) vs as es'
    Let JoinBind (Many vs') <$> transT s' True r

  -- Intermediate usage of lambda to join point promotion
  transT s True (AnnLet _ ValueBind (One (v, ty, AnnApp _ f@(Ref fv _) x)) r)
    | Just fa <- VarMap.lookup (toVar fv) s = do
    let a = fa - 1
        v' = toVar v
        s' = VarMap.insert v' a s
    f' <- transA s f
    x' <- transA s x
    Let JoinBind (One (fromVar (set covarInfo (JoinVar a) v'), ty, App f' x')) <$> transT s' True r
  transT s True (AnnLet _ ValueBind (One (v, ty, AnnTyApp _ f@(Ref fv _) x)) r)
    | Just fa <- VarMap.lookup (toVar fv) s = do
    let a = fa - 1
        v' = toVar v
        s' = VarMap.insert v' a s
    f' <- transA s f
    Let JoinBind (One (fromVar (set covarInfo (JoinVar a) v'), ty, TyApp f' x)) <$> transT s' True r

  -- Match commuting conversion
  transT s True (AnnLet _ ValueBind (One (name, nameTy, AnnMatch _ sc as)) cont) = do
    join <- fromVar <$> fresh (JoinVar 1)
    let Just res = approximateType cont
        joinTy = ForallTy Irrelevant nameTy res

        shoveJoinArm :: AnnAtom JoinConversion a -> Type a -> AnnArm JoinConversion a -> m (AnnArm JoinConversion a)
        shoveJoinArm j ty = armBody %%~ shoveJoin j ty

        shoveJoin :: AnnAtom JoinConversion a -> Type a -> AnnTerm JoinConversion a -> m (AnnTerm JoinConversion a)
        shoveJoin j ty (AnnLet b k bind body) = AnnLet b k bind <$> shoveJoin j ty body
        shoveJoin j ty (AnnMatch b t bs) = AnnMatch b t <$> traverse (shoveJoinArm j ty) bs
        shoveJoin j _  (AnnAtom b a) = pure (AnnApp b j a)

        shoveJoin j (ForallTy Irrelevant ty _) ex = do
          var <- fromVar <$> fresh ValueVar
          pure (AnnLet Preserve ValueBind (One (var, ty, ex)) (AnnApp Preserve j (Ref var ty)))
        shoveJoin j t ex = error ("What: shoveJoin " ++ show j ++ " " ++ show t ++ " " ++ show ex)

    as <- traverse (shoveJoinArm (Ref join joinTy) joinTy) as
    transT s True (AnnLet Preserve JoinBind (One (join, joinTy, AnnAtom Preserve (Lam (TermArgument name nameTy) cont)))
                   (AnnMatch Preserve sc as))

  transT s c (AnnLet _ k (One var) r) = do
    var' <- third3A (transT s False) var
    Let k (One var') <$> transT s c r

  transT s c (AnnLet _ k (Many vs) r) = do
    vs' <- traverse (third3A (transT s False)) vs
    Let k (Many vs') <$> transT s c r

  transT s _ (AnnExtend _ t rs) = Extend <$> transA s t <*> traverse (third3A (transA s)) rs
  transT s c (AnnMatch _ t bs) = Match <$> transA s t <*> traverse (armBody %%~ transT s c) bs

gatherJoinPass :: forall a. IsVar a => Term a -> AnnTerm JoinConversion a
gatherJoinPass = snd . goT mempty True mempty where
  goA _ a (Ref v ty) =
    (if toVar v `VarMap.member` a then traceShow (string "Deleting" <+> pretty v) else id)
    (VarMap.delete (toVar v) a, Ref v ty)
  goA u a (Lam arg body) = Lam arg <$> goT (VarMap.foldrWithKey (\a _ -> VarSet.insert a) u a) True a body
  goA _ a (Lit l) = (a, Lit l)

  -- Tail calls of candiates
  goT u True a (App (Ref f fty) x)
    | Just n <- VarMap.lookup (toVar f) a
    , toVar f `VarSet.notMember` u
    = let a'= if n > 1 then VarMap.insert (toVar f) 1 a else a
      in AnnApp Preserve (Ref f fty) <$> goA u a' x
  goT u True a (TyApp (Ref f fty) x)
    | Just n <- VarMap.lookup (toVar f) a
    , toVar f `VarSet.notMember` u
    = let a'= if n > 1 then VarMap.insert (toVar f) 1 a else a
      in (a', AnnTyApp Preserve (Ref f fty) x)

  -- If we're visiting an application of a candidate which is used in a tail position
  goT u True a (Let ValueBind (One (v, vty, App (Ref f fty) x)) r)
    | Just n <- VarMap.lookup (toVar f) a, n > 1
    , toVar f `VarSet.notMember` u
    = let (a', x') = goA u a x
          -- We insert our entry into the set with an arity of one less
          -- and visit child nodes with it
          ca = VarMap.insert (toVar v) (n - 1) a'
          (ca', r') = goT u True ca r
          -- We can extract the lookup from our new set and update as appropriate
          varr = maybe 0 (+1) (VarMap.lookup (toVar v) ca')
          a'' = VarMap.delete (toVar v) $
                case VarMap.lookup (toVar f) ca' of
                  Just i | varr == 0 -> VarMap.delete (toVar f) ca'
                         | i > varr -> VarMap.insert (toVar f) varr ca'
                  _ -> ca'
      in traceShow (string "Mapping" <+> pretty v <+> string (show varr) <+> string "=" <+> pretty f <+> string " => " <+> string (show (VarMap.lookup (toVar f) a'')))
        (a'', AnnLet Preserve ValueBind (One (v, vty, AnnApp Preserve (Ref f fty) x')) r')
  goT u True a (Let ValueBind (One (v, vty, TyApp (Ref f fty) x)) r)
    | Just n <- VarMap.lookup (toVar f) a, n > 1
    , toVar f `VarSet.notMember` u
    = let -- We insert our entry into the set with an arity of one less
          -- and visit child nodes with it
          ca = VarMap.insert (toVar v) (n - 1) a
          (ca', r') = goT u True ca r
          -- We can extract the lookup from our new set and update as appropriate
          varr = maybe 0 (+1) (VarMap.lookup (toVar v) ca')
          a' = VarMap.delete (toVar v) $
                case VarMap.lookup (toVar f) ca' of
                  Just i | varr == 0 -> VarMap.delete (toVar f) ca'
                         | i > varr -> VarMap.insert (toVar f) varr ca'
                  _ -> ca'
      in traceShow (string "Mapping" <+> pretty v <+> string (show varr) <+> string "=" <+> pretty f <+> string " => " <+> string (show (VarMap.lookup (toVar f) a')))
         (a', AnnLet Preserve ValueBind (One (v, vty, AnnTyApp Preserve (Ref f fty) x)) r')

  -- Single definitions in the tail position which are lambdas are candidates
  goT u True a (Let ValueBind (One (v, ty, e)) r) | isLam e =
    let (a', e') = goT u False a e
        -- And visit the remaining nodes with this definition's arity
        ca = VarMap.insert (toVar v) (lamArity' e) a'
        (ca', r') = goT u True ca r
        -- If we've still got something in scope, then we can convert it.
        arr = case VarMap.lookup (toVar v) ca' of
                Nothing -> Preserve
                Just x -> traceShow (string "Promoting" <+> pretty v, x) $ JoinOne x
    in ( VarMap.delete (toVar v) ca'
       , AnnLet arr ValueBind (One (v, ty, e')) r')

  -- Recursive definitions in the tail position which are lambdas are candidates
  goT u True a (Let ValueBind (Many vs) r) | all (isLam . thd3) vs =
    let ca = foldr (\(v, _, e) -> VarMap.insert (toVar v) (lamArity' e)) a vs
        (ca', vs') = foldr (go3 (goLam . flip (goT u))) (ca, []) vs
        (ca'', r') = goT u True ca' r
        -- If we've still got something in scope, then we can convert it.
        arr = case traverse (flip VarMap.lookup ca'' . toVar . fst3) vs of
                Nothing -> Preserve
                Just x -> JoinMany x
    in ( foldr (VarMap.delete . toVar . fst3) ca'' vs
       ,  AnnLet arr ValueBind (Many vs') r')

  -- Normal "fallback" visitors
  -- Honestly, this is just a MonadState (VarMap.Map Int), but it's a faff.
  goT u _ a (Atom at) = AnnAtom Preserve <$> goA u a at
  goT u _ a (App f x) =
    let (a', f') = goA u a f
        (a'', x') = goA u a' x
    in (a'', AnnApp Preserve f' x')
  goT u _ a (TyApp f x) =
    let (a', f') = goA u a f
    in (a', AnnTyApp Preserve f' x)
  goT u c a (Let k (One (v, ty, e)) r) =
    -- TODO: Join points should not push upvalues - strip the lambda first
    let (a', e') = goT u False a e
        (a'', r') = goT u c a' r
    in (a'', AnnLet Preserve k (One (v, ty, e')) r')
  goT u c a (Let k (Many vs) r) =
    -- TODO: Join points should not push upvalues - strip the lambda first
    let (a', vs') = foldr (go3 (goT u False)) (a, []) vs
        (a'', r') = goT u c a' r
    in (a'', AnnLet Preserve k (Many vs') r')
  goT u c a (Match t ps) =
    let (a', t') = goA u a t
        (a'', ps') = foldr goP (a', []) ps
    in (a'', AnnMatch Preserve t' ps')
    where
      goP ar (a, ps) =
        let (a', e') = goT u c a (view armBody ar)
        in (a', set armBody e' ar:ps)
  goT u _ a (Extend f fs) =
    let (a', f') = goA u a f
        (a'', fs') = foldr (go3 (goA u)) (a', []) fs
    in (a'', AnnExtend Preserve f' fs')
  goT u _ a (Cast e c) =
    let (a', e') = goA u a e
    in (a', AnnCast Preserve e' c)


  go3 g (f, ty, e) (a, fs) =
    let (a', e') = g a e
    in (a', (f, ty, e'):fs)

  isLam (Atom Lam{}) = True
  isLam _ = False

  goLam :: Functor f => (Bool -> Term a -> f (AnnTerm JoinConversion a)) -> Term a -> f (AnnTerm JoinConversion a)
  goLam f a@(Atom Lam{}) = goImpl a
    where goImpl (Atom (Lam arg t)) = AnnAtom Preserve . Lam arg <$> goImpl t
          goImpl t = f True t
  goLam f t = f False t
