-- | The backend for the Scheme backend.
module Backend.Scheme (compileProgram, Scheme) where

import Backend.Scheme.Emit
import Language.Scheme

import Syntax.Resolve.Scope

import Core.Occurrence
import Core.Core
import Core.Var

-- | Compile a collection of "Core"'s top-level statements to a Scheme
-- program.
compileProgram :: IsVar a => Maybe Signature -> [Stmt a] -> Scheme
compileProgram sig = undefined

