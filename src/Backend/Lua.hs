{-# LANGUAGE OverloadedStrings #-}

{-| The frontend for the Lua backend. This acts as the primary access point
  for basic interaction with the Lua compiler.
-}
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

-- | Compile a collection of "Core"'s top-level statements to a Lua
-- statement
compileProgram :: IsVar a => Env -> [Stmt a] -> LuaStmt
compileProgram e = LuaDo . (unitDef :) . addOperators . emitProgram e . tagOccursVar where
  unitDef = LuaLocal [ LuaName "__builtin_unit" ] [ LuaTable [ (LuaString "__tag", LuaString "__builtin_unit") ] ]
