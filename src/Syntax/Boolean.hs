-- | Quantifier-free boolean formulae without negation, used to encode
-- minimal definitions of type classes.
{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable,
   DeriveDataTypeable #-}
module Syntax.Boolean where

-- Ord-based nub
import qualified Data.Set as Set

import Data.Data

import Text.Pretty.Semantic

-- | A formula.
data Formula a
  = Var a -- ^ Variables
  | And [Formula a] -- ^ a AND b AND c
  | Or [Formula a] -- ^ a OR b OR c
  | Paren (Formula a) -- (a)
  deriving (Eq, Show, Ord, Functor, Foldable, Traversable, Data, Typeable)

data FormulaRes a
  = Sat
  | Unsat (Formula a)

isFalse :: Formula a -> Bool
isFalse (Or []) = False
isFalse _ = True

isTrue :: Formula a -> Bool
isTrue (And []) = True
isTrue _ = False

satisfy :: Ord a => (a -> Bool) -> Formula a -> FormulaRes a
satisfy assignment formula
  | isTrue formula' = Sat
  | otherwise = Unsat formula'
  where
    formula' = simpl (\x -> if assignment x then Just True else Nothing) formula

simpl :: Ord a => (a -> Maybe Bool) -> Formula a -> Formula a
simpl f (Var x) = case f x of
  Just b ->
    if b then And [] else Or []
  Nothing -> Var x
simpl f (And xs) = mkAnd (map (simpl f) xs)
simpl f (Or xs) = mkOr (map (simpl f) xs)
simpl f (Paren x) = simpl f x

mkAnd :: Ord a => [Formula a] -> Formula a
mkAnd = maybe (Or []) (mkAnd' . nub) . concatMapM fromAnd where
  fromAnd :: Formula a -> Maybe [Formula a]
  fromAnd (And xs) = Just xs
  fromAnd (Or []) = Nothing
  fromAnd x = Just [x]

  mkAnd' [x] = x
  mkAnd' xs = And xs

mkOr :: Ord a => [Formula a] -> Formula a
mkOr = maybe (And []) (mkOr' . nub) . concatMapM fromOr
  where
  fromOr (Or xs) = Just xs
  fromOr (And []) = Nothing
  fromOr x = Just [x]

  mkOr' [x] = x
  mkOr' xs = Or xs

nub :: Ord a => [a] -> [a]
nub x = Set.toList (Set.fromList x)

concatMapM :: (Traversable t, Applicative f) => (a -> f [b]) -> t a -> f [b]
concatMapM f xs = concat <$> traverse f xs

instance Pretty a => Pretty (Formula a) where
  pretty (Paren x) = parens (pretty x)
  pretty (Var x) = skeyword (pretty x)
  pretty (Or [x, y]) = string "either" <+> pretty x <+> string "or" <+> pretty y
  pretty (Or xs) =
        string "one of"
    <+> hsep (punctuate comma (map pretty (init xs))) <+> string "or"
    <+> pretty (last xs)
  pretty (And xs) =
        hsep (punctuate comma (map pretty (init xs)))
    <> string ", and"
    <+> pretty (last xs)
