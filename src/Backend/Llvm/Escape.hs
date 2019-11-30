{-# LANGUAGE FlexibleContexts #-}
module Backend.Llvm.Escape (EscapeScope, emptyScope, getName, escape) where

import Control.Monad.State.Class

import Backend.Escape

import qualified Data.Text as T
import Data.Text (Text)

import qualified LLVM.AST as Llvm

import Core.Var

emptyScope :: EscapeScope
emptyScope = createEscape builtins (basicEscaper mempty)

builtins :: [(CoVar, Text)]
builtins = []

getName :: MonadState EscapeScope m => CoVar -> m Llvm.Name
getName var = do
  st <- get
  let t = T.unpack (getVar var st)
  pure $ Llvm.mkName t

escape :: MonadState EscapeScope m => CoVar -> m Llvm.Name
escape var = do
  st <- get
  let (t, st') = pushVar var st
  put st'
  pure $ Llvm.mkName (T.unpack t)
