{-# LANGUAGE FlexibleContexts #-}
module Syntax.Let where

import qualified Data.Set as Set
import Data.Graph

import Control.Lens

import Syntax.Var
import Syntax

depOrder :: (Show (Var p), Show (Ann p), Ord (Var p))
         => [Binding p]
         -> [SCC (Binding p)]
depOrder = stronglyConnComp . build where
  build = map (\it@(Binding var ex _ _) -> (it, var, Set.toList (freeIn ex)))

freeIn :: (Show (Var p), Show (Ann p), Ord (Var p)) => Expr p -> Set.Set (Var p)
freeIn (Ascription e _ _)   = freeIn e
freeIn (RecordExt e rs _)   = freeIn e <> foldMap (freeIn . snd) rs
freeIn (BinOp a b c _)      = freeIn a <> freeIn b <> freeIn c
freeIn (VarRef v _)         = Set.singleton v
freeIn (Begin es _)         = foldMap freeIn es
freeIn (Let vs b _)         = (freeIn b <> foldMap (freeIn . view bindBody) vs) Set.\\ foldMapOf (each . bindVariable) Set.singleton vs
freeIn (App f x _)          = freeIn f <> freeIn x
freeIn (Fun p e _)          = freeIn e Set.\\ bound' p where
  bound' (PatParam p) = bound p
  bound' (ImplParam p) = bound p
freeIn (Record rs _)        = foldMap (freeIn . snd) rs
freeIn (Access e _ _)       = freeIn e
freeIn (Match t ps _)       = freeIn t <> foldMap freeInBranch ps where
  freeInBranch (p, e)       = freeIn e Set.\\ bound p
freeIn Literal{}            = mempty
freeIn Hole{}               = mempty
freeIn (If a b c _)         = freeIn a <> freeIn b <> freeIn c
freeIn (Tuple es _)         = foldMap freeIn es
freeIn (ExprWrapper _ e _)  = freeIn e -- wrappers only bind type arguments
freeIn (Parens e _)         = freeIn e
freeIn (LeftSection a b _)  = freeIn a <> freeIn b
freeIn (RightSection a b _) = freeIn a <> freeIn b
freeIn (BothSection b _)    = freeIn b
freeIn AccessSection{}      = mempty
freeIn x = error (show x)

bound :: Ord (Var p) => Pattern p -> Set.Set (Var p)
bound (Destructure _ x _) = maybe mempty bound x
bound (PRecord vs _)      = foldMap (bound . snd) vs
bound (PTuple ps _)       = foldMap bound ps
bound (Capture p _)       = Set.singleton p
bound (PType p _ _)       = bound p
bound Wildcard{}          = mempty
bound PLiteral{}          = mempty
bound (PWrapper _ p _)    = bound p
