{-# LANGUAGE ScopedTypeVariables, LambdaCase, OverloadedStrings #-}
module Optimise.Usage where

import qualified Data.Map.Strict as M

import Control.Monad.State
import Optimise.Collect

import Generics.SYB
import Syntax

import Control.Arrow

import Pretty (tracePrettyId)

countUsages :: SymbolTable a -> SymbolTable (Int, a)
countUsages sym = M.foldrWithKey (\v i -> updateInfo (first (const i)) v)
                                 (fmap (fmap ((,) 0)) sym)
                                 usages where
  usages = execState (mapM_ cU sym) M.empty
  cU (NativeValue _ e _ _) = everywhereM (mkM var) e where
    -- This one is needed V
    var :: Expr Typed -> State (M.Map (Var Resolved) Int) (Expr Typed)
    var x@(VarRef (TvName _ n _) _) = do
      modify . flip M.alter (ReName n) $ \case
        Nothing -> Just 1
        Just x -> Just (x + 1)
      pure x
    var x = pure x
  cU _ = pure undefined

consume :: TaggedName -> SymbolTable (Int, a) -> SymbolTable (Int, a)
consume = updateInfo (first (const 1)) . ReName

erase :: SymbolTable (Int, a) -> SymbolTable (Int, a)
erase = M.filterWithKey drop where
  drop (ReName (TgName x _))
    | tracePrettyId x == "main" = const True
    | otherwise = \case
        DataType{} -> True
        x -> (/= 0) . fst . extra $ x
  drop (ReName (TgInternal _)) = const True
