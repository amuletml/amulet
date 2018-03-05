{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module Core.Optimise.Newtype (killNewtypePass) where

import Control.Monad.Infer (Gen, fresh)

import qualified Data.VarMap as V
import Data.VarSet (IsVar(..))
import Data.Triple

import Core.Core

killNewtypePass :: forall a. IsVar a => [Stmt a] -> Gen Int [Stmt a]
killNewtypePass = go mempty where
  go :: V.Map (Coercion a) -> [Stmt a] -> Gen Int [Stmt a]
  go m (Type n cs:xs) = case cs of
    [(var, tp)] | Just nt <- isNewtype tp -> do
      (con, phi) <- newtypeCo (var, tp) nt
      (Type n [] :) . (con :) <$> go (V.insert (toVar var) phi m) xs
    _ -> (Type n cs:) <$> go m xs
  go m (x@Foreign{}:xs) = (x:) <$> go m xs
  go m (StmtLet vs:xs) = do
    vs' <- goBinding m vs
    (StmtLet vs':) <$> go m xs
  go _ [] = pure []

isNewtype :: Type a -> Maybe (Type a, Type a)
isNewtype (ForallTy _ t) = isNewtype t
isNewtype (ArrTy x y) = Just (x, y)
isNewtype _ = Nothing

newtypeMatch :: IsVar a => V.Map (Coercion a) -> [(Pattern a, Type a, Term a)] -> Maybe (Coercion a, (Pattern a, Type a, Term a))
newtypeMatch m (it@(Destr c _, _, _):xs)
  | Just phi <- V.lookup (toVar c) m = pure (phi, it)
  | otherwise = newtypeMatch m xs
newtypeMatch m (_:xs) = newtypeMatch m xs
newtypeMatch _ [] = Nothing

newtypeCo :: IsVar a => (a, Type a) -> (Type a, Type a) -> Gen Int (Stmt a, Coercion a)
newtypeCo (cn, tp) (dom, cod) = do
  var <- fresh
  let con = [(cn, tp, Atom (wrap tp (work phi)))]
      wrap (ForallTy v t) e = Lam Big (v, StarTy) (Atom (wrap t e))
      wrap _ e = e

      phi = SameRepr dom cod

      work c = Lam Small (fromVar var, dom) (Cast (Ref (fromVar var) dom) c)
  pure (StmtLet con, phi)


goBinding :: forall a. IsVar a => V.Map (Coercion a) -> [(a, Type a, Term a)] -> Gen Int [(a, Type a, Term a)]
goBinding m vs = traverse (third3A (goTerm m)) vs where
  goTerm :: V.Map (Coercion a) -> Term a -> Gen Int (Term a)
  goTerm m (Atom x) = Atom <$> goAtom m x
  goTerm m (App f x) = App <$> goAtom m f <*> goAtom m x
  goTerm m (Let vs e) = Let <$> goBinding m vs <*> (goTerm m e)
  goTerm m (Extend a as) = Extend <$> goAtom m a <*> traverse (third3A (goAtom m)) as
  goTerm m (TyApp f t) = TyApp <$> goAtom m f <*> pure t
  goTerm m (Cast f t) = Cast <$> goAtom m f <*> pure t
  goTerm m (Match a x) = case newtypeMatch m x of
    Just (phi, (Destr _ p, _, bd)) -> do
      var <- fresh
      let SameRepr _ castCodomain = phi
      bd <- goTerm m (Match (Ref (fromVar var) castCodomain) [(p, castCodomain, bd)])
      pure $ Let [(fromVar var, castCodomain, Cast a phi)] bd
    _ -> Match <$> goAtom m a <*> traverse (third3A (goTerm m)) x

  goAtom m (Lam s v e) = Lam s v <$> goTerm m e
  goAtom _ x = pure x
