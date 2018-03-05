{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module Core.Optimise.Newtype (killNewtypePass) where

import Control.Monad.Infer (Gen, fresh)

import qualified Data.VarMap as V
import Data.VarSet (IsVar(..))
import Data.Triple

import Core.Optimise
import Core.Types
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
isNewtype (ArrTy from to) = Just (from, to)
isNewtype _ = Nothing

newtypeMatch :: IsVar a => V.Map (Coercion a) -> [(Pattern a, Type a, Term a)] -> Maybe (Coercion a, (Pattern a, Type a, Term a))
newtypeMatch m (it@(Destr c _, ty, _):xs)
  | Just phi@(SameRepr _ cod) <- V.lookup (toVar c) m =
    case unify ty cod of
      Just map -> pure (substituteInCo map (Symmetry phi), it)
      Nothing -> pure (Symmetry phi, it)
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
goBinding m vs = traverse (third3A goTerm) vs where

  goTerm :: Term a -> Gen Int (Term a)
  goTerm (Atom x) = Atom <$> goAtom x
  goTerm (App f x) = App <$> goAtom f <*> goAtom x
  goTerm (Let vs e) = Let <$> goBinding m vs <*> (goTerm e)
  goTerm (Extend a as) = Extend <$> goAtom a <*> traverse (third3A goAtom) as
  goTerm (TyApp f t) = TyApp <$> goAtom f <*> pure t
  goTerm (Cast f t) = Cast <$> goAtom f <*> pure t
  goTerm (Match a x) = case newtypeMatch m x of
    Just (phi, (Destr _ p, _, bd)) -> do
      var <- fresh
      let Just (_, castCodomain) = relates phi
      bd <- goTerm (Match (Ref (fromVar var) castCodomain) [(p, castCodomain, bd)])
      pure $ Let [(fromVar var, castCodomain, Cast a phi)] bd
    _ -> Match <$> goAtom a <*> traverse (third3A goTerm) x

  goAtom (Lam s v e) = Lam s v <$> goTerm e
  goAtom x = pure x
