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

-- | Compile a collection of "Core"'s top-level statements to a Lua
-- statement
compileProgram :: IsVar a => [Stmt a] -> LuaStmt
compileProgram = LuaDo . (unitDef :) . addOperators . emitProgram . tagOccursVar where
  unitDef = LuaLocal [ LuaName "__builtin_unit" ] [ LuaTable [ (LuaString "__tag", LuaString "__builtin_unit") ] ]
