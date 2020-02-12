{-# LANGUAGE DeriveGeneric, OverloadedStrings, DuplicateRecordFields, FlexibleContexts, TypeFamilies #-}
module AmuletLsp.Features.TypeOverlay
  ( OverlayData(OverlayData)
  , getTypeOverlay
  , resolveTypeOverlay
  ) where

import Control.Lens hiding (List)

import qualified Data.Text as T
import Data.Aeson.Types
import Data.Foldable
import Data.Span

import GHC.Generics

import Language.Haskell.LSP.Types

import Syntax.Types
import Syntax

import AmuletLsp.Features

import Text.Pretty.Semantic

data OverlayData = OverlayData
  { name :: T.Text
  , id   :: Int
  , file :: Int
  }
  deriving Generic

instance ToJSON OverlayData
instance FromJSON OverlayData

-- | Get the unresolved type overlay for this program.
--
-- The provided name is the unique "name" for this file. While we could
-- pass the 'NormalizedUri', this should be more memory efficient.
getTypeOverlay :: Name -> [Toplevel Resolved] -> [CodeLens]
getTypeOverlay (TgName _ modu) = getTops [] where
  getTop ac (LetStmt _ _ b) = foldl getBinding ac b
  getTop ac (Module _ _ (ModStruct ms _)) = getTops ac ms
  getTop ac (Open (ModStruct ms _)) = getTops ac ms
  getTop ac (Include (ModStruct ms _)) = getTops ac ms
  getTop ac _ = ac
  getTops = foldl' getTop

  mk :: Var Resolved -> Span -> CodeLens
  mk (TgName c n) p = CodeLens (firstLine (rangeOf p)) Nothing (Just (toJSON (OverlayData c n modu)))
  mk TgInternal{} _ = error "Impossible binding to TgInternal"

  getBinding :: [CodeLens] -> Binding Resolved -> [CodeLens]
  getBinding ac (Binding v _ _ pos) = mk v pos:ac
  getBinding ac (Matching p _ pos) | Just v <- singleVar p = mk v pos:ac
  getBinding ac _ = ac

  singleVar :: Pattern Resolved -> Maybe (Var Resolved)
  singleVar (Capture v _) = Just v
  singleVar (PType p _ _) = singleVar p
  singleVar _ = Nothing

getTypeOverlay TgInternal{} = error "Impossible"

-- | Resolve a type overlay, updating the lens with the type of the
-- embedded variable.
resolveTypeOverlay :: Env -> OverlayData -> CodeLens -> Maybe CodeLens
resolveTypeOverlay env (OverlayData v n _) (CodeLens range Nothing _) =
  let var = TgName v n
  in case env ^. names . at var of
       Nothing -> Nothing
       Just ty ->
         let cmd = Command (renderBasic $ pretty var <+> colon <+> displayType ty) "" Nothing
         in Just (CodeLens range (Just cmd) Nothing)
resolveTypeOverlay _ _ c@(CodeLens _ Just{} _)  = Just c
