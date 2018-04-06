{-# LANGUAGE FlexibleContexts #-}
module Syntax.Let where

import qualified Data.Set as Set
import Data.Semigroup
import Data.Triple
import Data.Graph

import Syntax

depOrder :: (Show (Var p), Show (Ann p), Ord (Var p))
         => [(Var p, Expr p, Ann p)]
         -> [SCC (Var p, Expr p, Ann p)]
depOrder = stronglyConnComp . build where
  build = map (\it@(var, ex, _) -> (it, var, Set.toList (freeIn ex)))

freeIn :: (Show (Var p), Show (Ann p), Ord (Var p)) => Expr p -> Set.Set (Var p)
freeIn (Ascription e _ _)  = freeIn e
freeIn (RecordExt e rs _)  = freeIn e <> foldMap (freeIn . snd) rs
freeIn (BinOp a b c _)     = freeIn a <> freeIn b <> freeIn c
freeIn (VarRef v _)        = Set.singleton v
freeIn (Begin es _)        = foldMap freeIn es
freeIn (Let vs b _)        = (freeIn b <> foldMap (freeIn . snd3) vs) Set.\\ Set.fromList (map fst3 vs)
freeIn (App f x _)         = freeIn f <> freeIn x
freeIn (Fun p e _)         = freeIn e Set.\\ bound p
freeIn (Record rs _)       = foldMap (freeIn . snd) rs
freeIn (Access e _ _)      = freeIn e
freeIn (Match t ps _)      = freeIn t <> foldMap freeInBranch ps where
  freeInBranch (p, e)      = freeIn e Set.\\ bound p
freeIn Literal{}           = mempty
freeIn Hole{}              = mempty
freeIn (If a b c _)        = freeIn a <> freeIn b <> freeIn c
freeIn (Tuple es _)        = foldMap freeIn es
freeIn (ExprWrapper _ e _) = freeIn e -- wrappers only bind type arguments
freeIn x = error (show x)

bound :: Ord (Var p) => Pattern p -> Set.Set (Var p)
bound (Destructure _ x _) = maybe mempty bound x
bound (PRecord vs _)      = foldMap (bound . snd) vs
bound (PTuple ps _)       = foldMap bound ps
bound (Capture p _)       = Set.singleton p
bound (PType p _ _)       = bound p
bound Wildcard{}          = mempty
bound PLiteral{}          = mempty
