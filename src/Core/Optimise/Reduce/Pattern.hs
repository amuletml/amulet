{-# LANGUAGE ScopedTypeVariables #-}
module Core.Optimise.Reduce.Pattern
  ( Subst
  , PatternResult(..)
  , reducePattern
  , filterDeadArms
  , simplifyArms
  ) where

import Control.Lens

import qualified Data.VarMap as VarMap
import qualified Data.VarSet as VarSet
import qualified Data.HashSet as HSet
import qualified Data.Text as T
import Data.Triple
import Data.List

import Core.Optimise.Reduce.Base

type Subst a = [(a, Atom a)]

data PatternResult a
  = PatternFail -- ^ A pattern which did not match at all
  | PatternUnknown (Subst a) -- ^ A pattern whose success cannot be determined
  | PatternPartial (Subst a) -- ^ A pattern which did match but not all variables could be substituted
  | PatternComplete (Subst a) -- ^ A pattern which matched and all variables were found
  deriving (Show)

instance Ord a => Semigroup (PatternResult a) where
  PatternFail <> _ = PatternFail
  _ <> PatternFail = PatternFail

  (PatternUnknown s) <> t = PatternUnknown (s <> extract t)
  s <> (PatternUnknown t) = PatternUnknown (extract s <> t)

  (PatternPartial s) <> t = PatternPartial (s <> extract t)
  s <> (PatternPartial t) = PatternPartial (extract s <> t)

  (PatternComplete s) <> (PatternComplete t) = PatternComplete (s <> t)

extract :: PatternResult a -> Subst a
extract PatternFail = mempty
extract (PatternUnknown s) = s
extract (PatternPartial s) = s
extract (PatternComplete s) = s

reducePattern :: forall a. IsVar a
              => ReduceScope a
              -> Atom a -> Pattern a
              -> PatternResult a

-- A wildcard always yields a complete pattern
reducePattern _ _ PatWildcard = PatternComplete mempty

-- Literals are relatively easy to accept/reject
reducePattern _ _ (PatLit RecNil) = PatternComplete mempty
reducePattern _ _ (PatLit Unit) = PatternComplete mempty
reducePattern _ (Lit l') (PatLit l)
  | l == l'   = PatternComplete mempty
  | otherwise = PatternFail

-- If we're matching against a known constructor it is easy to accept or reject
reducePattern s (Ref v _) (Constr c)
  | lookupRawVar v s == c = PatternComplete mempty

  | isCtor (lookupRawVar v s) s = PatternFail
  | Just (App (Ref c' _) _) <- lookupRawTerm v s
  , isCtor (lookupRawVar c' s) s = PatternFail

reducePattern s (Ref v _) (Destr c (Capture a _))
  | Just (App (Ref c' _) a') <- lookupRawTerm v s
  , lookupRawVar c' s == c = PatternComplete [(a, a')]

  | isCtor (lookupRawVar v s) s = PatternFail
  | Just (App (Ref c' _) _) <- lookupRawTerm v s
  , isCtor (lookupRawVar c' s) s = PatternFail

-- Attempt to reduce the field
reducePattern s e (PatExtend (Capture r' _) fs) =
  foldr ((<>) . handle)
        (PatternComplete [(r', e)])
        fs where
  handle :: (T.Text, Capture a) -> PatternResult a
  handle (f, Capture v _) =
    case find ((==f) . fst3) fs' of
      Nothing -> PatternPartial mempty
      Just (_, _, e) -> PatternComplete [(v, e)]

  fs' = simplifyRecord e

  simplifyRecord (Ref v _) =
    case lookupTerm v s of
      Just (Atom r) -> simplifyRecord r
      Just (Extend r fs) -> fs ++ simplifyRecord r
      _ -> []
  simplifyRecord _ = []

reducePattern _ _ (PatValues []) = PatternComplete mempty
reducePattern s (Ref v _) (PatValues cs)
  | Just (Values vs) <- lookupTerm v s
  = PatternComplete (zipWith (\(Capture c _) a -> (c,a)) cs vs)

-- If we know this pattern doesn't match, then remove it
reducePattern s (Ref v _) p
  | VarDef { varNotAmong = ps } <- lookupVar v s
  , p `elem` ps = PatternFail

-- If we are matching against a literal, we know any usage of it within
-- the branch will be that.
reducePattern _ (Ref v _) (PatLit l) = PatternUnknown [(v, Lit l)]

reducePattern _ _ _ = PatternUnknown mempty

-- | Remove arms which are shadowed by a previous one and so will never be executed
filterDeadArms :: forall a v x. (IsVar v, IsVar a) => (x -> Pattern v) -> ReduceScope a -> [x] -> [x]
filterDeadArms pat s = goArm where
  goArm [] = []
  goArm (a:as) =
    case pat a of
      -- Trivial patterns which always match
      PatWildcard   -> [a]
      PatLit Unit   -> [a]
      PatLit RecNil -> [a]
      PatExtend{}   -> [a]
      PatValues _   -> [a]

    -- Unbounded literals will never be complete unless we have a wildcard
      PatLit l@Int{}   -> a : goLit (HSet.singleton l) as
      PatLit l@Str{}   -> a : goLit (HSet.singleton l) as
      PatLit l@Float{} -> a : goLit (HSet.singleton l) as

      -- See if we've got the other case
      PatLit LitTrue  -> a : goBool LitTrue  as
      PatLit LitFalse -> a : goBool LitFalse as

      -- See if we've got all cases
      Destr v _ -> a : goSum (buildSumSet v) as
      Constr v  -> a : goSum (buildSumSet v) as

  -- | Build a HashSet of literals, removing those we've already seen.
  goLit _ [] = []
  goLit v (a:as) =
    case pat a of
      PatWildcard -> [a]
      PatLit l | HSet.member l v -> goLit v as
               | otherwise -> a : goLit (HSet.insert l v) as
      _ -> a : goLit v as

  -- | Remove booleans we've already seen, and terminate if we have both true
  -- and false.
  goBool _ [] = []
  goBool l (a:as) =
    case pat a of
      PatWildcard -> [a]
      PatLit l' | l /= l' -> [a]
      _ -> a : goBool l as

  -- | Build a set of variables we've yet to seen, skipping cases which do not
  -- appear in the set and terminating when it's empty.
  goSum _ [] = []
  goSum m _ | m == mempty = []
  goSum m (a:as) =
    case pat a of
      PatWildcard -> [a]
      Constr v   | VarSet.member (toVar v) m -> a : goSum (VarSet.delete (toVar v) m) as
                 | otherwise -> goSum m as
      Destr v _  | VarSet.member (toVar v) m -> a : goSum (VarSet.delete (toVar v) m) as
                 | otherwise -> goSum m as
      _ -> a : goSum m as

  -- | Build a set of all _other_ cases for the given type variable.
  buildSumSet :: v -> VarSet.Set
  buildSumSet v =
    let Just (v', _) = VarMap.lookup (toVar v) (s ^. ctorScope)
        Just cases = VarMap.lookup (toVar v') (s ^. typeScope)
    in foldr (\(a, _) -> if toVar a == toVar v then id else VarSet.insert (toVar a)) mempty cases

-- | Simplify the set of arms: first use 'reducePattern' to narrow down our
-- selection of patterns, then eliminate dead arms with 'filterDeadArms'.
--
-- This either returns a single arm, if the match is known to be complete (and
-- so can be eliminated), or a list of potential arms otherwise.
simplifyArms :: IsVar a
             => ReduceScope a
             -> Atom a -> [AnnArm b (OccursVar a)]
             -> Either (AnnArm b (OccursVar a), Subst a) [(AnnArm b (OccursVar a), Subst a)]
simplifyArms s test arms = filterDeadArms (_armPtrn . fst) s <$> foldCases arms where
  foldCases [] = Right []
  foldCases (a@Arm { _armPtrn = p }:as) =
    case reducePattern s test (underlying <$> p) of
      PatternFail -> foldCases as
      PatternUnknown subst -> Right ((a, subst):either pure id (foldCases as))
      PatternPartial subst -> Right [(a, subst)]
      PatternComplete subst -> Left (a, subst)
