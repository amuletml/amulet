{-# LANGUAGE DeriveDataTypeable, FlexibleInstances #-}
module Core.Core where


import qualified Data.VarSet as VarSet
import Data.Generics hiding (empty)
import Data.Data (Data, Typeable)
import Data.Text (Text, pack)
import Data.Triple

import Syntax (Var(..), Resolved)

data CoTerm
  = CotRef (Var Resolved) CoType
  | CotLam Size (Var Resolved, CoType) CoTerm
  | CotApp CoTerm CoTerm -- removes a λ

  | CotLet [(Var Resolved, CoType, CoTerm)] CoTerm
  | CotMatch CoTerm [(CoPattern, CoType, CoTerm)]
  | CotBegin [CoTerm] CoTerm

  | CotLit CoLiteral

  | CotExtend CoTerm [(Text, CoType, CoTerm)]

  | CotTyApp CoTerm CoType -- removes a Λ
  deriving (Eq, Show, Ord, Data, Typeable)

data CoPattern
  = CopCapture (Var Resolved) CoType
  | CopConstr (Var Resolved)
  | CopDestr (Var Resolved) CoPattern
  | CopExtend CoPattern [(Text, CoPattern)]

  | CopLit CoLiteral
  deriving (Eq, Show, Ord, Data, Typeable)

data CoLiteral
  = ColInt Integer
  | ColStr Text
  | ColTrue | ColFalse
  | ColUnit | ColRecNil
  deriving (Eq, Show, Ord, Data, Typeable)

data CoType
  = CotyCon (Var Resolved)
  | CotyVar (Var Resolved)
  | CotyForall [Var Resolved] CoType
  | CotyArr CoType CoType
  | CotyApp CoType CoType
  | CotyRows CoType [(Text, CoType)]
  | CotyExactRows [(Text, CoType)]
  | CotyStar -- * :: *
  deriving (Eq, Show, Ord, Data, Typeable)

data Size
  = Big | Small
  deriving (Eq, Show, Ord, Data, Typeable)

data CoStmt
  = CosForeign (Var Resolved) CoType Text
  | CosLet [(Var Resolved, CoType, CoTerm)]
  | CosType (Var Resolved) [(Var Resolved, CoType)]
  deriving (Eq, Show, Ord, Data, Typeable)


{-# ANN freeIn "HLint: ignore" #-}
-- Rationale: can't use <> because of Doc. Ughr.
freeIn :: CoTerm -> VarSet.Set
freeIn (CotRef v _) = VarSet.singleton v
freeIn (CotLam Small (v, _) e) = VarSet.delete v (freeIn e)
freeIn (CotLam Big _ e) = freeIn e
freeIn (CotApp f x) = freeIn f `mappend` freeIn x
freeIn (CotLet vs e) = VarSet.difference (freeIn e `mappend` foldMap (freeIn . thd3) vs)
                                         (VarSet.fromList (map fst3 vs))
freeIn (CotMatch e bs) = freeIn e `mappend` foldMap freeInBranch bs where
  freeInBranch (b, _, e) = VarSet.difference (freeIn e) (bound b)
  bound (CopCapture v _) = VarSet.singleton v
  bound (CopDestr _ p) = bound p
  bound (CopExtend p ps) = foldMap (bound . snd) ps `mappend` bound p
  bound _ = mempty
freeIn (CotLit _) = mempty
freeIn (CotExtend c rs) = freeIn c `mappend` foldMap (freeIn . thd3) rs
freeIn (CotTyApp f _) = freeIn f
freeIn (CotBegin xs x) = foldMap freeIn xs `mappend` freeIn x

isError :: CoTerm -> Bool
isError (CotApp (CotTyApp (CotRef (TgInternal n) _) _) _) = n == pack "error"
isError _ = False

stripTyApp :: CoTerm -> CoTerm
stripTyApp = everywhere (mkT go) where
  go (CotTyApp x _) = x
  go x = x
