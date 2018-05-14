module Backend.Lua
  ( compileProgram
  , LuaStmt
  ) where

import Backend.Lua.Postprocess
import Backend.Lua.Syntax
import Backend.Lua.Emit

import Core.Occurrence
import Core.Core
import Core.Var

import Syntax.Types

compileProgram :: IsVar a => Env -> [Stmt a] -> LuaStmt
compileProgram e = LuaDo . addOperators . emitProgram e . tagOccursVar
