{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Backend.Llvm (compileToLlvm) where

import Control.Monad.State.Strict

import Syntax.Resolve.Scope (Signature(..))

import Backend.Llvm.Escape

import qualified Data.Text.Encoding as T

import Core.Core
import Core.Var

import qualified LLVM.AST as Llvm
import LLVM.IRBuilder

compileToLlvm :: Maybe Signature -> [Stmt CoVar] -> Llvm.Module
compileToLlvm _ prog = evalState (buildModuleT "amulet" (traverse genStmt prog)) emptyScope

genStmt :: (MonadModuleBuilder m, MonadState EscapeScope m) => Stmt CoVar -> m ()
genStmt (Foreign _var _c_type _text) = undefined =<< escape _var
genStmt (StmtLet _binding) = undefined
genStmt (Type _type_name _constructors) = undefined
genStmt (RawCode asm) = do
  let str = T.encodeUtf8 asm
  emitDefn (Llvm.ModuleInlineAssembly str)
