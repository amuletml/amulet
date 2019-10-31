{-| The frontend for the Lua backend. This acts as the primary access point
  for basic interaction with the Lua compiler.
-}
module Backend.Lua
  ( compileProgram
  , LuaStmt
  ) where

import Control.Monad.State

import Data.Foldable

import Language.Lua.Syntax

import Backend.Lua.Postprocess
import Backend.Lua.Emit

import Syntax.Resolve.Scope

import Core.Occurrence
import Core.Core
import Core.Var

-- | Compile a collection of "Core"'s top-level statements to a Lua
-- statement
compileProgram :: IsVar a => Maybe Signature -> [Stmt a] -> LuaStmt
compileProgram sig
  = LuaDo . toList . fst
  . maybe id (\sig (stmt, state) -> (addExport sig stmt state, state)) sig
  . uncurry addBuiltins
  . flip runState defaultEmitState . emitStmt
  . snd . tagOccurStmt (const occursSet) OccursVar (foldMap exportedNames sig)
