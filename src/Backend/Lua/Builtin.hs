{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

{-|A collection of all builtin variables which Amulet might use
-}
module Backend.Lua.Builtin
  ( ExtraVars, builtinVars
  , builtinEscape
  , builtinBuilders
  ) where

import Control.Lens

import qualified Data.VarMap as VarMap
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import Data.Sequence (Seq)

import Core.Builtin
import Core.Var

import Language.Lua.Syntax
import Language.Lua.Quote

import Backend.Escape

-- | The basic escaper for Lua code bases
escaper :: T.Text -> T.Text
escaper = basicEscaper keywords

-- | The default 'EscapeScope' for the backend.
builtinEscape:: EscapeScope
builtinEscape
  = flip createEscape escaper
  . ((vError, "error"):)
  $ map (\(v, n, _, _, _) -> (v, n)) builtins

-- | A set of variables which will be lazilly emitted by the backend.
--
-- See "Backend.Emit.Postprocess" for more information.
type ExtraVars = VarMap.Map ([CoVar], Seq LuaStmt)

-- | The default 'ExtraVars' for the backend.
builtinVars :: ExtraVars
builtinVars = VarMap.fromList $ map (\(v, _, d, _, s) -> (v, (d, Seq.fromList s))) builtins

-- | All native "builders"
builtinBuilders :: VarMap.Map ((Int, [LuaExpr] -> [LuaExpr]), LuaVar)
builtinBuilders = foldr go mempty builtins where
  go (_, _, _, Nothing, _) m = m
  go (v, n, _, Just b, _)  m = VarMap.insert v (b, LuaName n) m

builtins :: [( CoVar, T.Text, [CoVar]
             , Maybe (Int, [LuaExpr] -> [LuaExpr])
             , [LuaStmt] )]
builtins =
  [ ( vLAZY, "__builtin_Lazy", [], Nothing
      -- Lazy doesn't technically _need_ a tag, and including it in fact
      -- raises the memory usage of things. But, the REPL is taught to
      -- recognise __tag fields and print them as constructors, and so
      -- this looks better.
    , [luaStmts|
      local function __builtin_Lazy(x)
        return { x, false, __tag = "lazy" }
      end
      |] )
  , ( vForce, "__builtin_force", [vUnit], Nothing
    , [luaStmts|
       local function __builtin_force_err()
         error("Loop while forcing thunk")
       end
       local function __builtin_force(x)
         if x[2] then
           return x[1]
         else
           local thunk = x[1]
           x[1] = __builtin_force_err
           x[1] = thunk(__builtin_unit)
           x[2] = true
           return x[1]
         end
       end
      |] )
  , ( vOpApp, "__builtin_app", [], Nothing
    , [luaStmts|
      local function __builtin_app(f, x)
        return f(x)
      end
    |] )
  , ( vUnit, "__builtin_unit", [], Nothing
    , [luaStmts|local __builtin_unit = { __tag = "__builtin_unit" }|] )
  ] ++ map genOp ops

  where
    genOp (var, op) =
      let name = escaper (var ^. covarName)
          name_ = LuaName name
          inner = LuaBinOp (LuaRef left) op (LuaRef right)
      in ( var, name, []
         , Just (2, \[l, r] -> [LuaBinOp l op r])
         , [luaStmts|local function $name_($left, $right) return %inner end|] )
    left  = LuaName "l"
    right = LuaName "r"

    ops :: [(CoVar, T.Text)]
    ops =
      [ (vOpAdd, "+"),  (vOpAddF, "+")
      , (vOpSub, "-"),  (vOpSubF, "-")
      , (vOpMul, "*"),  (vOpMulF, "*")
      , (vOpDiv, "/"),  (vOpDivF, "/")
      , (vOpExp, "^"),  (vOpExpF, "^")
      , (vOpLt,  "<"),  (vOpLtF,  "<")
      , (vOpGt,  ">"),  (vOpGtF,  "<")
      , (vOpLe,  "<="), (vOpLeF,  "<=")
      , (vOpGe,  ">="), (vOpGeF,  ">=")

      , (vOpConcat, "..")

      , (vOpEq, "==")
      , (vOpNe, "~=")
      ]
