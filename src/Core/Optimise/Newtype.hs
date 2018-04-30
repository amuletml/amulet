{-# LANGUAGE ScopedTypeVariables #-}
module Core.Optimise.Newtype (killNewtypePass) where

import Control.Monad.Infer (Gen, fresh)
import Control.Monad
import Control.Lens

import qualified Data.VarMap as V
import Data.VarSet (IsVar(..))
import Data.Triple

import Core.Optimise
import Core.Types

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

isNewtype :: IsVar a => Type a -> Maybe (Type a, Type a)
isNewtype (ForallTy Irrelevant from to) = Just (from, to)
isNewtype (ForallTy (Relevant var) k t) = do
  (from, to) <- isNewtype t
  guard (var `occursInTy` to || k == StarTy)
  pure (from, to)
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

newtypeCo :: IsVar a => (a, Type a) -> (Type a, Type a) -> Gen Int (Stmt a, Coercion a)
newtypeCo (cn, tp) (dom, cod) = do
  var <- fresh
  let con = [(cn, tp, Atom (wrap tp (work phi)))]
      wrap (ForallTy (Relevant v) c t) e = Lam (TypeArgument v c) (Atom (wrap t e))
      wrap _ e = e

      phi = SameRepr dom cod

      work c = Lam (TermArgument (fromVar var) dom) (Cast (Ref (fromVar var) dom) c)
  pure (StmtLet con, phi)


goBinding :: forall a. IsVar a => V.Map (Coercion a) -> [(a, Type a, Term a)] -> Gen Int [(a, Type a, Term a)]
goBinding m = traverse (third3A goTerm) where

  goTerm :: Term a -> Gen Int (Term a)
  goTerm (Atom x) = Atom <$> goAtom x
  goTerm (App f x) = App <$> goAtom f <*> goAtom x
  goTerm (Let (Many vs) e) = Let . Many <$> goBinding m vs <*> goTerm e
  goTerm (Let (One (v, t, e)) b) = do
    e' <- goTerm e
    Let (One (v, t, e')) <$> goTerm b
  goTerm (Extend a as) = Extend <$> goAtom a <*> traverse (third3A goAtom) as
  goTerm (TyApp f t) = TyApp <$> goAtom f <*> pure t
  goTerm (Cast f t) = Cast <$> goAtom f <*> pure t
  goTerm (Match a x) = case newtypeMatch m x of
    Just (phi, Arm { _armPtrn = Destr _ p, _armBody = bd, _armVars = vs, _armTyvars = tvs }) -> do
      var <- fresh
      let Just (_, castCodomain) = relates phi
      bd' <- goTerm (Match (Ref (fromVar var) castCodomain) [Arm { _armPtrn = p, _armTy = castCodomain
                                                                 , _armBody = bd, _armVars = vs, _armTyvars = tvs }])
      pure $ Let (One (fromVar var, castCodomain, Cast a phi)) bd'
    _ -> Match <$> goAtom a <*> traverse (armBody %%~ goTerm) x

  goAtom (Lam arg e) = Lam arg <$> goTerm e
  goAtom x = pure x
