module Core.Types
  ( arity
  , approximateType, approximateAtomType
  , unify, unifyWith, unifyClosed
  , replaceTy
  ) where

import qualified Data.VarMap as VarMap
import qualified Data.Text as T
import Core.Builtin
import Core.Core
import Core.Var

import Control.Lens

import Control.Monad.State
import Control.Applicative

import Data.Traversable
import Data.Foldable
import Data.Maybe
import Data.List

import Text.Pretty.Semantic

-- | Compute the arity of a function type. Namely, how many terms one can
-- apply to it.
arity :: Type a -> Int
arity (ForallTy Relevant{} _ t) = arity t
arity (ForallTy Irrelevant _ t) = 1 + arity t
arity _ = 0

-- | Get the type of an atom.
approximateAtomType :: IsVar a => Atom a -> Type a
approximateAtomType (Ref _ t) = t
approximateAtomType (Lit l) = fmap fromVar $ case l of
  Int{} -> tyInt
  Float{} -> tyFloat
  Str{} -> tyString
  LitTrue -> tyBool
  LitFalse -> tyBool
  Unit -> tyUnit
  RecNil -> ExactRowsTy []

-- | Approximate the type of a term, returning 'Nothing' if there was a
-- mismatch.
approximateType :: IsVar a => AnnTerm b a -> Maybe (Type a)
approximateType (AnnAtom _ a) = pure (approximateAtomType a)
approximateType (AnnCast _ _ phi) = snd <$> relates phi
approximateType (AnnApp _ f _) = do
  let ForallTy _ _ d = approximateAtomType f
  pure d
approximateType (AnnLam _ (TypeArgument v k) f) = ForallTy (Relevant v) k <$> approximateType f
approximateType (AnnLam _ (TermArgument _ t) f) = ForallTy Irrelevant t <$> approximateType f
approximateType (AnnLet _ _ e) = approximateType e
approximateType (AnnMatch _ _ xs) = case xs of
  (x:_) -> approximateType (x ^. armBody)
  [] -> error "impossible approximateType empty match"
approximateType (AnnExtend _ e rs) = pure (RowsTy (approximateAtomType e) (map (\(x, _, t) -> (x, approximateAtomType t)) rs))
approximateType (AnnValues _ xs) = pure (ValuesTy (map approximateAtomType xs))
approximateType (AnnTyApp _ f at) = do
  let ForallTy (Relevant v) _ t = approximateAtomType f
  let replace = transform go
      go (VarTy v') | v == v' = at
      go x = x
  pure (replace t)

-- | Attempt to unify two types, returning a possible substitution
-- mapping of type variables to replacement types.
unify :: IsVar a => Type a -> Type a -> Maybe (VarMap.Map (Type a))
unify = unifyWith mempty

-- | Attempt to unify two types with an existing unification scheme.
unifyWith :: IsVar a => VarMap.Map (Type a) -> Type a -> Type a -> Maybe (VarMap.Map (Type a))
unifyWith m a b = execStateT (unify' a b) m

unify' :: IsVar a => Type a -> Type a -> StateT (VarMap.Map (Type a)) Maybe ()
unify' t'@(VarTy v) t
  | t' == t = pure ()
  | otherwise = do
      x <- gets (VarMap.lookup (toVar v))
      case x of
        Just t' -> unify' t t'
        Nothing -> modify (VarMap.insert (toVar v) t)
unify' t (VarTy v) = unify' (VarTy v) t
unify' (ConTy v) (ConTy v') = mempty <$ guard (v == v')
unify' (ForallTy Irrelevant a b) (ForallTy Irrelevant a' b') = liftA2 (<>) (unify' a a') (unify' b b')
unify' x@RowsTy{} y@RowsTy{} =
  let (inner, rows) = getRows x
      (inner', rows') = getRows y
   in do
     mgu <- unify' inner inner'
     ts <- for (zip (sortOn fst rows) (sortOn fst rows')) $
       \((_, t), (_, t')) -> unify' t t'
     pure (mgu <> fold ts)
unify' (ForallTy (Relevant v) c t) (ForallTy (Relevant v') c' t') = liftA2 (<>) (unify' c c') (unify' t (replaceTy v' (VarTy v) t'))
unify' (AppTy f t) (AppTy f' t') = liftA2 (<>) (unify' f f') (unify' t t')
unify' (ValuesTy xs) (ValuesTy xs') = goTup xs xs' where
  goTup [] [] = lift (Just mempty)
  goTup (x:xs) (y:ys) = (<>) <$> unify' x y <*> goTup xs ys
  goTup _ _ = lift Nothing
unify' StarTy StarTy = pure ()
unify' NilTy NilTy = pure ()
unify' (RowsTy NilTy []) NilTy = pure ()
unify' NilTy (RowsTy NilTy []) = pure ()
unify' _ _ = lift Nothing

-- | Replace a type variable with a new type inside a type.
--
-- This is a more lightweight version of @Core.Optimise@'s
-- @substituteInType@.
replaceTy :: IsVar a => a -> Type a -> Type a -> Type a
replaceTy var at = transform (go var) where
  go v (VarTy v') | v == v' = at
  go _ x = x

-- | Determines if these two types unify under /closed/ variables. This
-- effectively determines if the two types are equivalent.
unifyClosed :: IsVar a => Type a -> Type a -> Bool
unifyClosed = go mempty where
  go _ (ConTy a) (ConTy b) = a == b
  go s (VarTy a) (VarTy b) = fromMaybe a (VarMap.lookup (toVar a) s) == b
  go s (ForallTy (Relevant v) c ty) (ForallTy (Relevant v') c' ty')
    | v == v' = go s c c' && go s' ty ty'
    | otherwise = go (VarMap.insert (toVar v) v' s') ty ty' && go s c c'
    where s' = VarMap.delete (toVar v') s
  go s (ForallTy Irrelevant a r) (ForallTy Irrelevant a' r') = go s a a' && go s r r'
  go s (AppTy f x) (AppTy f' x') = go s f f' && go s x x'
  go s (RowsTy f ts) (RowsTy f' ts') = and (zipWith (\(l, t) (l', t') -> l == l' && go s t t') (sortOn fst firsts) (sortOn fst seconds)) where
    get (RowsTy f ts) = get f ++ ts
    get NilTy = []
    get x = error ("Malformed record type" ++ show (pretty x))

    firsts = get f ++ ts
    seconds = get f' ++ ts'

  go s (ValuesTy xs) (ValuesTy xs') = goTup s xs xs'
  go _ StarTy StarTy = True
  go _ NilTy NilTy = True
  go _ _ _ = False

  goTup _ [] [] = True
  goTup s (x:xs) (y:ys) = go s x y && goTup s xs ys
  goTup _ _ _ = False

getRows :: Type a -> (Type a, [(T.Text, Type a)])
getRows (RowsTy i ts) =
  let (inner, ts') = getRows i
   in (inner, ts' ++ ts)
getRows x = (x, [])
