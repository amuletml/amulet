{-# LANGUAGE DeriveDataTypeable, DataKinds, TypeFamilies,
   DisambiguateRecordFields, DuplicateRecordFields, DeriveAnyClass,
   RecordWildCards, DeriveFunctor, FlexibleContexts #-}
module Optimise.Collect where

import qualified Data.Map.Strict as M

import Data.Data (Data)
import Data.Typeable

import Data.Text (Text)

import Control.Arrow

import Data.Spanned
import Data.Span

import Syntax

import Control.Monad.Reader

data SymInfo a
  = DataType { vars :: [Var Typed]
             , constrs :: M.Map (Var Typed) ConstrInfo
             , location :: Span
             , extra :: a
             }
  | NativeValue { valType :: Type Typed
                , valDecl :: Expr Typed
                , location :: Span
                , extra :: a
                }
  | ForeignValue { valType :: Type Typed
                 , valFrag :: Text
                 , location :: Span
                 , extra :: a
                 }
  deriving (Eq, Show, Ord, Data, Typeable, Spanned, Functor)

data ConstrInfo
  = GadtConstr { conType :: Type Typed
               , equalities :: M.Map (Type Typed) (Type Typed)
               , location :: Span
               }
  | AlgConstr  { conType :: Type Typed, location :: Span }
  | UnitConstr Span
  deriving (Eq, Show, Ord, Data, Typeable, Spanned)

type SymbolTable a = M.Map (Var Resolved) (SymInfo a)

tally :: [Toplevel Typed] -> SymbolTable ()
tally = M.fromAscList . map (first eraseVarTy) . foldMap tally' where
  tally' (LetStmt vs an) = map (\(x@(TvName _ _ t), y) -> (x, NativeValue t y an ())) vs
  tally' (ForeignVal var frg t an) = [(var, ForeignValue t frg an ())]
  tally' (TypeDecl nm vs cs an) = [(nm, DataType vs (tallyCons cs) an ())]
  tally' ValStmt{} = []

  tallyCons = M.fromAscList . foldMap tallyCon

  tallyCon (UnitCon v s) = [(v, UnitConstr s)]
  tallyCon (ArgCon v t s) = [(v, AlgConstr t s)]
  tallyCon (GADTCon v (TyCons eqs t) s) = [(v, GadtConstr t eqm s)] where
    eqm = M.fromAscList $ foldMap (\(Equal a b _) -> [(a, b)]) eqs
  tallyCon (GADTCon v t s) = [(v, GadtConstr t M.empty s)]


-- The most general way to know about a declaration.
illuminate :: ( MonadPlus m
              , MonadReader (SymbolTable b) m)
           => Var Resolved
           -> (SymInfo b -> m a)
           -> m a
illuminate var k = do
  syms <- ask
  case M.lookup var syms of
    Just x -> k x
    Nothing -> mzero

lookupInfo :: ( MonadPlus m
              , MonadReader (SymbolTable b) m)
           => Var Resolved -> m (SymInfo b)
lookupInfo = flip illuminate pure

updateInfo :: (a -> a) -> Var Resolved -> SymbolTable a -> SymbolTable a
updateInfo k sym tab = M.update (\x -> Just (fmap k x)) sym tab
