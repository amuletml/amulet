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
  [ ( vLAZY, "__builtin_Lazy", []
    , Just (1, \[x] -> (mempty, [[lua| { %x, false, __tag = "lazy" }|]]))
      -- Lazy doesn't technically _need_ a tag, and including it in fact
      -- raises the memory usage of things. But, the REPL is taught to
      -- recognise __tag fields and print them as constructors, and so
      -- this looks better.
    , [luaStmts|
      local function __builtin_Lazy(x)
        return { x, false, __tag = "lazy" }
      end
      |] )

  , ( vNIL, "Nil", [], Just (0, \_ -> (mempty, [[lua| { __tag = "Nil" } |]]))
    , [luaStmts| local Nil = { __tag = "Nil" } |] )

  , ( vCONS, "Cons", [], Just (1, \[x] -> (mempty, [[lua| { %x, __tag = "Cons" } |]]))
    , [luaStmts|
      local function Cons(x)
        return { x, __tag = "Cons" }
      end
      |] )

  , ( vForce, "__builtin_force", [], Nothing
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

  , ( vRef, "__builtin_ref", []
    , Just (1, \[var] -> ( mempty, [ [lua| { %var, __tag = 'Ref' } |] ]))
    , [luaStmts|
        local function __builtin_ref(x)
          return { x, __tag = 'Ref' }
        end
      |] )

  , ( vAssign, "__builtin_swap", []
    , Just (2, \[var, val] -> (Seq.fromList [luaStmts| %var[1] = %val |], [ [lua|nil|] ]))
    , [luaStmts|
        local function __builtin_swap(var, val)
          var[1] = val
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

    -- Note: the definition generates records that look correct
    -- according to the type, but since the type system prevents
    -- accessing a removed key, it's fine for us /not/ to remove it.
    -- Hence the inline definition just returning the original record.
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

  -- TC error_message builtins:
  , ( tcString, "__tc_S", [], Just (1, \[x] -> (mempty, [[lua| { %x, __tag = "String" } |]]))
    , [luaStmts|
        local function __tc_S(x)
          return { x, __tag = "String" }
        end
      |] )

  , ( tcHCat, "__tc_H", [], Just (1, \[x] -> (mempty, [[lua| { %x, __tag = ":<#>:" } |]]))
    , [luaStmts|
        local function __tc_H(x)
          return { x, __tag = ":<#>:" }
        end
      |] )

  , ( tcVCat, "__tc_V", [], Just (1, \[x] -> (mempty, [[lua| { %x, __tag = ":<>:" } |]]))
    , [luaStmts|
        local function __tc_V(x)
          return { x, __tag = ":<>:" }
        end
      |] )

  , ( tcShowType, "__tc_St", [], Just (1, \[x] -> (mempty, [[lua| { %x, __tag = "ShowType" } |]]))
    , [luaStmts|
        local function __tc_St(x)
          return { x, __tag = "ShowType" }
        end
      |] )

  , ( tcTYPEABLE, "_Typeable", [], Just (1, \[x] -> (mempty, [[lua| %x() |]]))
    , [luaStmts|
        local function _Typeable(x)
          return x()
        end
      |] )
  , ( tcTypeableApp, "_Typeable_app", [], Nothing
    , [luaStmts|
        local function _Typeable_app(pair)
          local ta, tb = pair._1[1], pair._2[1]
          return { { name = "(" .. ta.name .. ") :$ (" .. tb.name .. ")" } , __tag = "TypeRep" }
        end
      |] )

  , ( tcTypeableKnownKnown, "_Typeable_kk", [], Nothing
    , [luaStmts|
        local function _Typeable_kk(finger, name)
          return { { name = name .. "#" .. finger } , __tag = "TypeRep" }
        end
      |] )

  , ( tcTYPEREP, "_TypeRep", [], Nothing
    , [luaStmts|
        local function _TypeRep(x)
          return { { name = x.name .. "#" .. x.fingerprint }, __tag = "TypeRep" }
        end
      |] )
  , ( tcUnTypeable, "__type_of", [], Just (1, \[x] -> (mempty, [[lua| function() return %x end |]]))
    , [luaStmts|
        local function type_of(x)
          return function() return x end
        end
      |] )
  , ( tcEqTypeRep, "__eq_type_rep", [], Nothing
    , [luaStmts|
        local function __eq_type_rep(tr_a, tr_b, keq, kne)
          if tr_a[1].name == tr_b[1].name then
            return keq()() -- n.b.: first argument is ghost of equality proof
          else
            return kne()
          end
        end
      |] )
  ]
