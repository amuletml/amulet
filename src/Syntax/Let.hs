{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
module Syntax.Let where

import qualified Data.Set as Set
import qualified Data.Map as Map

import Data.Graph

import Control.Lens

import Syntax.Var
import Syntax

import GHC.Exts (IsList(..))

depOrder :: Ord (Var p)
         => [Binding p]
         -> [SCC (Binding p)]
depOrder binds = extra ++ stronglyConnComp nodes where
  (extra, nodes, mapping) = foldr buildNode (mempty, mempty, mempty) binds

  buildNode it@(Binding var ex _) (e, n, m) =
    ( e, (it, var, freeInMapped ex):n, Map.insert var var m )
  buildNode it@(Matching p ex _) (e, n, m) =
    case bound p of
      [] -> (AcyclicSCC it:e, n, m)
      vs@(var:_) -> (e, (it, var, freeInMapped ex):n, foldr (`Map.insert`var) m vs)
  buildNode it@(TypedMatching p ex _ _) (e, n, m) =
    case bound p of
      [] -> (AcyclicSCC it:e, n, m)
      vs@(var:_) -> (e, (it, var, freeInMapped ex):n, foldr (`Map.insert`var) m vs)

  freeInMapped = Set.toList . Set.foldr (maybe id Set.insert . flip Map.lookup mapping) mempty . freeIn

freeIn :: Ord (Var p) => Expr p -> Set.Set (Var p)
freeIn (Ascription e _ _)   = freeIn e
freeIn (RecordExt e rs _)   = freeIn e <> foldMap (freeIn . view fExpr) rs
freeIn (BinOp a b c _)      = freeIn a <> freeIn b <> freeIn c
freeIn (VarRef v _)         = Set.singleton v
freeIn (Begin es _)         = foldMap freeIn es
freeIn (Let vs b _)         = (freeIn b <> foldMap (freeIn . view bindBody) vs) Set.\\ foldMapOf (each . bindVariable) Set.singleton vs
freeIn (App f x _)          = freeIn f <> freeIn x
freeIn (Fun p e _)          = freeIn e Set.\\ bound (p ^. paramPat)
freeIn (Record rs _)        = foldMap (freeIn . view fExpr) rs
freeIn (Access e _ _)       = freeIn e
freeIn (Match t ps _)       = freeIn t <> foldMap freeInBranch ps where
  freeInBranch (p, e)       = freeIn e Set.\\ bound p
freeIn Literal{}            = mempty
freeIn Hole{}               = mempty
freeIn (If a b c _)         = freeIn a <> freeIn b <> freeIn c
freeIn (Tuple es _)         = foldMap freeIn es
freeIn (ExprWrapper w e _)  =
  case w of
    x Syntax.:> y -> freeIn (ExprWrapper x (ExprWrapper y e undefined) undefined)
    ExprApp x -> freeIn x <> freeIn e
    _ -> freeIn e
freeIn (Parens e _)         = freeIn e
freeIn (LeftSection a b _)  = freeIn a <> freeIn b
freeIn (RightSection a b _) = freeIn a <> freeIn b
freeIn (BothSection b _)    = freeIn b
freeIn AccessSection{}      = mempty
freeIn Function{} = error "ds Function freeIn"
freeIn TupleSection{} = error "ds TupleSection freeIn"
freeIn OpenIn{} = error "ds OpenIn freeIn"
freeIn Syntax.Lazy{} = error "ds Lazy freeIn"

bound :: (IsList m, Item m ~ Var p, Monoid m)
      => Pattern p -> m
bound (Destructure _ x _) = foldMap bound x
bound (PAs p v _)         = fromList [v] <> bound p
bound (PRecord vs _)      = foldMap (bound . snd) vs
bound (PTuple ps _)       = foldMap bound ps
bound (Capture p _)       = fromList [p]
bound (PType p _ _)       = bound p
bound Wildcard{}          = mempty
bound PLiteral{}          = mempty
bound (PWrapper _ p _)    = bound p
bound (PSkolem p _ _)     = bound p

boundWith :: (IsList m, Item m ~ (Var p, Ann p), Monoid m)
          => Pattern p -> m
boundWith (Destructure _ x _) = foldMap boundWith x
boundWith (PRecord vs _)      = foldMap (boundWith . snd) vs
boundWith (PTuple ps _)       = foldMap boundWith ps
boundWith (Capture p a)       = fromList [(p, a)]
boundWith (PAs p v a)         = fromList [(v, a)] <> boundWith p
boundWith (PType p _ _)       = boundWith p
boundWith Wildcard{}          = mempty
boundWith PLiteral{}          = mempty
boundWith (PWrapper _ p _)    = boundWith p
boundWith (PSkolem p _ _)     = boundWith p


bindVariables :: (IsList (m (Var p)), Item (m (Var p)) ~ Var p, Monoid (m (Var p)))
              => Binding p -> m (Var p)
bindVariables Binding { _bindVariable = v } = fromList [v]
bindVariables Matching { _bindPattern = p } = bound p
bindVariables TypedMatching { _bindPattern = p } = bound p
