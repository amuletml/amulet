{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

{-|Handles post-processing the generated Lua sources, injecting any
 additional code which may be needed and (potentially) doing some basic
 optimisations.
-}
module Backend.Lua.Postprocess
  ( addOperators
  , genOperator
  ) where

import qualified Data.Map as Map
import qualified Data.VarMap as VarMap
import qualified Data.VarSet as VarSet

import Language.Lua.Syntax
import Language.Lua.Quote

import Backend.Lua.Emit
import Backend.Escape
import Core.Builtin
import Core.Var


{-|
  Walks the Lua tree and identifies which operators are not fully applied
  (and so their variable is referenced), injecting them into the program.
 -}
addOperators :: [LuaStmt] -> [LuaStmt]
addOperators stmt =
  let ops = foldMap opsStmt stmt
  in foldr ((:) . genOperator) stmt (VarSet.toList ops)

  where
    opsStmt :: LuaStmt -> VarSet.Set
    opsStmt (LuaDo b) = foldMap opsStmt b
    opsStmt (LuaAssign vs xs) = foldMap opsVar vs <> foldMap opsExpr xs
    opsStmt (LuaWhile t b) = opsExpr t <> foldMap opsStmt b
    opsStmt (LuaRepeat b t) = opsExpr t <> foldMap opsStmt b
    opsStmt (LuaFornum _ b e s bo) = opsExpr b <> opsExpr e <> opsExpr s <> foldMap opsStmt bo
    opsStmt (LuaFor _ g b) = foldMap opsExpr g <> foldMap opsStmt b
    opsStmt (LuaLocal _ e) = foldMap opsExpr e
    opsStmt (LuaLocalFun _ _ b) = foldMap opsStmt b
    opsStmt (LuaReturn r) = foldMap opsExpr r
    opsStmt (LuaIfElse t) = foldMap (\(c, b) -> opsExpr c <> foldMap opsStmt b) t
    opsStmt LuaBreak = mempty
    opsStmt (LuaCallS f) = opsCall f
    opsStmt LuaQuoteS{} = mempty

    opsExpr :: LuaExpr -> VarSet.Set
    opsExpr LuaNil = mempty
    opsExpr LuaTrue = mempty
    opsExpr LuaFalse = mempty
    opsExpr LuaDots = mempty
    opsExpr LuaNumber{} = mempty
    opsExpr LuaInteger{} = mempty
    opsExpr LuaString{} = mempty
    opsExpr LuaQuoteE{} = mempty
    opsExpr LuaBitE{} = mempty

    opsExpr (LuaCallE f) = opsCall f
    opsExpr (LuaRef v) = opsVar v
    opsExpr (LuaFunction _ b) = foldMap opsStmt b
    opsExpr (LuaTable ks) = foldMap (\(k, v) -> opsExpr k <> opsExpr v) ks
    opsExpr (LuaBinOp l _ r) = opsExpr l <> opsExpr r
    opsExpr (LuaUnOp _ x) = opsExpr x

    opsVar :: LuaVar -> VarSet.Set
    opsVar (LuaName t) = foldMap VarSet.singleton (Map.lookup t opNames)
    opsVar (LuaIndex t k) = opsExpr t <> opsExpr k
    opsVar LuaQuoteV{} = mempty

    opsCall (LuaCall f xs) = foldMap opsExpr (f:xs)
    opsCall (LuaInvoke f _ xs) = foldMap opsExpr (f:xs)

    opNames = Map.filter (`VarMap.member` ops) (fromEsc escapeScope)
                `Map.union` Map.fromList [ ( "__builtin_Lazy", vLAZY ), ( "__builtin_force", vForce ) ]

-- | Generate the Lua definition for some built-in Amulet variable.
--
-- The name is a little misleading, as this this does not exclusively
-- apply to binary operators.
genOperator :: CoVar -> LuaStmt
genOperator op | op == vLAZY =
  [luaStmt|
    local function __builtin_Lazy(x)
      return { x, false, __tag = "lazy" }
    end
  |]

genOperator op | op == vOpApp =
  [luaStmt|
    local function __builtin_app(f, x)
      return f(x)
    end
  |]

genOperator op | op == vForce =
  [luaStmt|
   local function __builtin_force(x)
     if x[2] then
       return x[1]
     else
       local thunk = x[1]
       x[1] = function()
         error("loop while forcing thunk")
       end
       x[1] = thunk(__builtin_unit)
       x[2] = true
       return x[1]
     end
   end
  |]
genOperator op =
  let name =  getVar op escapeScope
  in LuaLocalFun (LuaName name) [left]
                 [LuaReturn [LuaFunction [right]
                             [LuaReturn [LuaBinOp (LuaRef left) (remapOp op) (LuaRef right)]]]]
  where
    left  = LuaName "l"
    right = LuaName "r"

{- Note: Tags on Lazy

   Lazy doesn't technically _need_ a tag, and including it in fact
   raises the memory usage of things. But, the REPL is taught to
   recognise __tag fields and print them as constructors, and so this
   looks better. -}
