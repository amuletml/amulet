{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

{-|A collection of all builtin variables which Amulet might use
-}
module Backend.Lua.Builtin
  ( ExtraVars, builtinVars
  , builtinEscape
  , builtinBuilders
  ) where

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
builtinEscape :: EscapeScope
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
builtinBuilders :: VarMap.Map ((Int, [LuaExpr] -> (Seq LuaStmt, [LuaExpr])), LuaVar)
builtinBuilders = foldr go mempty builtins where
  go (_, _, _, Nothing, _) m = m
  go (v, n, _, Just b, _)  m = VarMap.insert v (b, LuaName n) m

builtins :: [( CoVar, T.Text, [CoVar]
             , Maybe (Int, [LuaExpr] -> (Seq LuaStmt, [LuaExpr]))
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
  , ( vNIL, "Nil", [], Nothing
    , [luaStmts| local Nil = { __tag = "Nil" } |] )
  , ( vCONS, "Cons", [], Nothing
    , [luaStmts|
      local function Cons(x)
        return { x, __tag = "Cons" }
      end
      |] )
  , ( vForce, "__builtin_force", [vUnit], Nothing
    , [luaStmts|
       local function __builtin_trap()
         error("Loop while forcing thunk")
       end
       local function __builtin_force(x)
         if x[2] then
           return x[1]
         else
           local thunk = x[1]
           x[1] = __builtin_trap
           x[1] = thunk()
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
  , ( vRef, "__builtin_ref", []
    , Just (1, \[var] -> ( mempty, [ [lua| { %var, __tag = 'Ref' } |] ]))
    , [luaStmts|
        local function __builtin_ref(x)
          return { x, __tag = 'Ref' }
        end
      |] )
  , ( vAssign, "__builtin_swap", [vUnit]
    , Just (2, \[var, val] -> ( Seq.fromList [luaStmts| %var[1] = %val |]
                              , [ [lua| __builtin_unit |] ]))
    , [luaStmts|
        local function __builtin_swap(var, val)
          var[1] = val
          return __builtin_unit
        end
      |] )
  , ( vDeref, "__builtin_deref", [], Just (1, \[l] -> (mempty, [ [lua|%l[1]|] ]))
    , [luaStmts|
        local function __builtin_deref(var)
          return var[1]
        end
      |] )
  , ( vStrVal, "__builtin_strval", [], Just (1, \[l] -> (mempty, [ [lua|%l|] ]))
    , [luaStmts|
        local function __builtin_strval(x)
          return x
        end
      |] )
  , ( vKSTR, "__builtin_kstr", [], Just (1, \[l] -> (mempty, [ [lua|%l|] ]))
    , [luaStmts|
        local function __builtin_kstr(x)
          return x
        end
      |] )
  , ( vIntVal, "__builtin_intval", [], Just (1, \[l] -> (mempty, [ [lua|%l|] ]))
    , [luaStmts|
        local function __builtin_intval(x)
          return x
        end
      |] )
  , ( vKINT, "__builtin_kint", [], Just (1, \[l] -> (mempty, [ [lua|%l|] ]))
    , [luaStmts|
        local function __builtin_kint(x)
          return x
        end
      |] )
  , ( vROWCONS, "__builtin_rowcons", [], Just (1, \[l] -> (mempty, [ [lua|%l|] ]))
    , [luaStmts|
        local function __builtin_rowcons(x)
          return x
        end
      |] )
  , ( backendClone, "__builtin_clone", [], Nothing
    , [luaStmts|
         local function __builtin_clone(record)
          local new = {}
          for k, v in pairs(record) do
            new[k] = v
          end
          return new
         end
      |] )
  , ( vExtend, "__builtin_extend", [ backendClone ], Nothing
    , [luaStmts|
        local function __builtin_extend(key, value, record)
          local new = __builtin_clone(record)
          new[key] = value
          return new
        end
      |] )
  , ( vRestrict, "__builtin_restrict", [ backendClone ]
    , Just (2, \[key, t] -> (mempty, [[lua| { _1 = %t[%key], _2 = %t }|]]))
    , [luaStmts|
        local function __builtin_restrict(key, record)
          local new = __builtin_clone(record)
          new[key] = nil
          return { _1 = record[key], _2 = new }
        end
      |] )
  , ( vEQ, "nil", [], Just (0, \[] -> (mempty, [[lua|nil|]])), [] )
    -- Note: the definition generates records that look correct
    -- according to the type, but since the type system prevents
    -- accessing a removed key, it's fine for us /not/ to remove it.
    -- Hence the inline definition just returning the original record.
  ] ++ map genOp ops

  where
    genOp (var, op) =
      let name = escaper (covarDisplayName var)
          name_ = LuaName name
          inner = LuaBinOp (LuaRef left) op (LuaRef right)
      in ( var, name, []
         , Just (2, \[l, r] -> (mempty, [LuaBinOp l op r]))
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
