{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
module Syntax.Let where

import qualified Data.Set as Set
import qualified Data.Map as Map

import Data.Graph

import Control.Lens

import Syntax.Var
import Syntax

import GHC.Exts (IsList(..))

depOrder :: (Show (Var p), Show (Ann p), Ord (Var p))
         => [Binding p]
         -> [SCC (Binding p)]
depOrder binds = extra ++ stronglyConnComp nodes where
  (extra, nodes, mapping) = foldr buildNode (mempty, mempty, mempty) binds

  buildNode it@(Binding var ex _ _) (e, n, m) =
    ( e, (it, var, freeInMapped ex):n, Map.insert var var m )
  buildNode it@(Matching p ex _) (e, n, m) =
    case bound p of
      [] -> (AcyclicSCC it:e, n, m)
      vs@(var:_) -> (e, (it, var, freeInMapped ex):n, foldr (`Map.insert`var) m vs)
  buildNode it@(ParsedBinding p ex _ _) (e, n, m) =
    case bound p of
      [] -> (AcyclicSCC it:e, n, m)
      vs@(var:_) -> (e, (it, var, freeInMapped ex):n, foldr (`Map.insert`var) m vs)

  freeInMapped = Set.toList . Set.foldr (maybe id Set.insert . flip Map.lookup mapping) mempty . freeIn

freeIn :: (Show (Var p), Show (Ann p), Ord (Var p)) => Expr p -> Set.Set (Var p)
freeIn (Ascription e _ _)   = freeIn e
freeIn (RecordExt e rs _)   = freeIn e <> foldMap (freeIn . view fExpr) rs
freeIn (BinOp a b c _)      = freeIn a <> freeIn b <> freeIn c
freeIn (VarRef v _)         = Set.singleton v
freeIn (Begin es _)         = foldMap freeIn es
freeIn (Let vs b _)         = (freeIn b <> foldMap (freeIn . view bindBody) vs) Set.\\ foldMapOf (each . bindVariable) Set.singleton vs
freeIn (App f x _)          = freeIn f <> freeIn x
freeIn (Fun p e _)          = freeIn e Set.\\ bound' p where
  bound' (PatParam p) = bound p
  bound' (ImplParam p) = bound p
freeIn (Record rs _)        = foldMap (freeIn . view fExpr) rs
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

bound :: (IsList (m (Var p)), Item (m (Var p)) ~ Var p, Monoid (m (Var p)))
      => Pattern p -> m (Var p)
bound (Destructure _ x _) = maybe mempty bound x
bound (PRecord vs _)      = foldMap (bound . snd) vs
bound (PTuple ps _)       = foldMap bound ps
bound (Capture p _)       = fromList [p]
bound (PType p _ _)       = bound p
bound Wildcard{}          = mempty
bound PLiteral{}          = mempty
bound (PWrapper _ p _)    = bound p

bindVariables :: (IsList (m (Var p)), Item (m (Var p)) ~ Var p, Monoid (m (Var p)))
              => Binding p -> m (Var p)
bindVariables Binding { _bindVariable = v } = fromList [v]
bindVariables Matching { _bindPattern = p } = bound p
bindVariables ParsedBinding { _bindPattern = p } = bound p
