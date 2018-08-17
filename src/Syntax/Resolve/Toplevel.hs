{-# LANGUAGE FlexibleContexts #-}

-- | This simply provides some helper methods for extracting all
-- top-level variables within a module.
module Syntax.Resolve.Toplevel
  ( extractToplevel, extractToplevels
  ) where

import Syntax.Var
import Syntax.Let
import Syntax

-- | Extract all top-level variables declared within a statement,
-- including those within child modules.
--
-- This returns a lists of both term and type variables.
--
-- The 'Semigroup' instance is used in order to prepend module names onto
-- variables declared in child modules.
extractToplevel :: Semigroup (Var p) => Toplevel p -> ([Var p], [Var p])
extractToplevel (LetStmt d) = (foldMap bindVariables d, [])
extractToplevel (ForeignVal v _ _ _) = ([v], [])
extractToplevel (TypeDecl v _ c) = (map ctor c, [v]) where
  ctor (UnitCon v _) = v
  ctor (ArgCon v _ _) = v
  ctor (GeneralisedCon v _ _) = v
extractToplevel (Open _ _) = ([], [])
extractToplevel (Module v xs) = let (vs, ts) = extractToplevels xs
                                in (map (v<>) vs, map (v<>) ts)

-- | Extract all top-level variables declared within a series of
-- statements including those within child modules.
--
-- This returns a lists of both term and type variables.
--
-- The 'Semigroup' instance is used in order to prepend module names onto
-- variables declared in child modules.
extractToplevels :: Semigroup (Var p) => [Toplevel p] -> ([Var p], [Var p])
extractToplevels [] = ([], [])
extractToplevels (x:xs) = let (vs, ts) = extractToplevels xs
                              (v, t) = extractToplevel x
                          in (v ++ vs, t ++ ts)
