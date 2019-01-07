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
-- including those within child modules, that are visible outside that
-- module.
--
-- This returns a lists of both term and type variables.
--
-- The 'Semigroup' instance is used in order to prepend module names onto
-- variables declared in child modules.
extractToplevel :: Semigroup (Var p) => Toplevel p -> ([Var p], [Var p])
extractToplevel (LetStmt Public d) = (foldMap bindVariables d, [])
extractToplevel (LetStmt Private _) = mempty
extractToplevel (ForeignVal Public v _ _ _) = ([v], [])
extractToplevel (ForeignVal Private _ _ _ _) = mempty
extractToplevel (TypeDecl Public v _ c) = (map ctor c, [v]) where
  ctor (UnitCon v _) = v
  ctor (ArgCon v _ _) = v
  ctor (GeneralisedCon v _ _) = v
extractToplevel (TypeDecl Private _ _ _) = mempty
extractToplevel (Open _ _) = mempty
extractToplevel (Module Public v xs) =
  let (vs, ts) = extractToplevels xs
  in (map (v<>) vs, map (v<>) ts)
extractToplevel (Module Private _ _) = mempty
extractToplevel (Class v Public _ _ ms _) = (foldMap getName ms, [v]) where
  getName (MethodSig v _ _) = [v]
  getName (DefaultMethod b _) = bindVariables b
extractToplevel (Class _ Private _ _ _ _) = mempty
extractToplevel Instance{} = ([], [])

-- | Extract all top-level variables declared within a series of
-- statements including those within child modules, that are visible
-- outside that module.
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
