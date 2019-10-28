{-# LANGUAGE ScopedTypeVariables #-}

{- | Eliminate new types with a single case, converting constructors and
   pattern matches on them into a coercion.
-}
module Core.Optimise.Newtype (killNewtypePass) where

import Control.Monad.Namey
import Control.Monad
import Control.Lens

import qualified Data.VarMap as V
import Data.Triple

import Core.Optimise
import Core.Types

-- | Run the new-type elimination pass.
killNewtypePass :: forall a m. (IsVar a, MonadNamey m) => [Stmt a] -> m [Stmt a]
killNewtypePass = go mempty mempty where
  go :: V.Map (Atom a) -> V.Map (Coercion a) -> [Stmt a] -> m [Stmt a]
  go ss m (Type n cs:xs) = case cs of
    [(var, tp)] | Just nt <- isNewtype tp -> do
      (con, phi, sub) <- newtypeWorker (var, tp) nt
      (Type n [] :) . (con :) <$> go (ss <> sub) (V.insert (toVar var) phi m) xs
    _ -> (Type n cs:) <$> go ss m xs
  go ss m (x@Foreign{}:xs) = (x:) <$> go ss m xs
  go ss m (StmtLet (Many vs):xs) = do
    vs' <- goBinding ss m vs
    xs' <- go ss m xs
    pure (StmtLet (Many vs'):xs')
  go ss m (StmtLet (One v):xs) = do
    ~[v'] <- goBinding ss m [v]
    xs' <- go ss m xs
    pure (StmtLet (One v'):xs')

  go _ _ [] = pure []

isNewtype :: IsVar a => Type a -> Maybe (Spine a)
isNewtype (ForallTy Irrelevant from to) =
  case isNewtype to of
    Just _ -> Nothing
    _ -> Just $ Spine [(Irrelevant, from)] to
isNewtype (ForallTy (Relevant var) k t) = do
  (Spine tys res) <- isNewtype t
  guard (var `occursInTy` res)
  pure (Spine ((Relevant var, k):tys) res)
isNewtype _ = Nothing

newtypeMatch :: IsVar a => V.Map (Coercion a) -> [Arm a] -> Maybe (Coercion a, Arm a)
newtypeMatch m (it@Arm { _armPtrn = Destr c _, _armTy = ty }:xs)
  | Just phi@(SameRepr _ cod) <- V.lookup (toVar c) m =
    case unify cod ty of
      Just map -> pure (substituteInCo map (Symmetry phi), it)
      Nothing -> error $ "failed to match newtype-constructor types " ++ show cod ++ " and " ++ show ty
  | otherwise = newtypeMatch m xs
newtypeMatch m (_:xs) = newtypeMatch m xs
newtypeMatch _ [] = Nothing

-- Note: (again) the order of parameters to unify matters! The
-- substitution is always in terms of the *first* parameter. Here, the
-- results were backwards, so the solution wasn't being applied properly
-- and the generated code was wrong.

newtypeWorker :: forall a m. (IsVar a, MonadNamey m)
              => (a, Type a) -> Spine a -> m (Stmt a, Coercion a, V.Map (Atom a))
newtypeWorker (cn, tp) (Spine tys cod) = do
  let CoVar nam id _ = toVar cn
      (Irrelevant, dom) = last tys

      cname :: a
      cname = fromVar (CoVar nam id ValueVar)

      phi = SameRepr dom cod

      wrap ((Relevant v, c):ts) ex = Lam (TypeArgument v c) <$> wrap ts ex
      wrap [(Irrelevant, c)] ex = do
        v <- fresh ValueVar
        Lam (TermArgument (fromVar v) c) <$> pure (ex (fromVar v) c)
      wrap _ _ = undefined

      work var ty = Cast (Ref var ty) cod phi

      work :: a -> Type a -> Term a
      wrap :: [(BoundTv a, Type a)] -> (a -> Type a -> Term a) -> m (Term a)

  wrapper <- wrap tys work
  let con = ( cname, tp, wrapper )
  pure (StmtLet (One con), phi, V.singleton (toVar cn) (Ref (fromVar (CoVar nam id ValueVar)) tp))

goBinding :: forall a m. (IsVar a, MonadNamey m)
          => V.Map (Atom a) -> V.Map (Coercion a) -> [(a, Type a, Term a)] -> m [(a, Type a, Term a)]
goBinding ss m = traverse (third3A (fmap (substitute ss) . goTerm)) where
  goTerm :: Term a -> m (Term a)
  goTerm e@Atom{}   = pure e
  goTerm e@App{}    = pure e
  goTerm e@Extend{} = pure e
  goTerm e@Values{} = pure e
  goTerm e@Values{} = pure e
  goTerm e@Cast{}   = pure e
  goTerm e@TyApp{}  = pure e

  goTerm (Lam arg e) = Lam arg <$> goTerm e
  goTerm (Let (Many vs) e) = Let . Many <$> goBinding ss m vs <*> goTerm e
  goTerm (Let (One (v, t, e)) b) = do
    e' <- goTerm e
    Let (One (v, t, e')) <$> goTerm b

  goTerm (Match a x) = case newtypeMatch m x of
    Just (phi, Arm { _armPtrn = Destr _ [Capture v ty], _armBody = bd }) -> do
      var <- fresh ValueVar
      let Just (_, castCodomain) = relates phi
      bd' <- goTerm (Let (One (v, ty, Atom (Ref (fromVar var) castCodomain))) bd)
      pure $ Let (One (fromVar var, castCodomain, Cast a castCodomain phi)) bd'
    _ -> Match a <$> traverse (armBody %%~ goTerm) x

data Spine a =
  Spine [(BoundTv a, Type a)] (Type a)
  deriving (Eq, Show, Ord)
