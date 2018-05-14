{-# LANGUAGE OverloadedStrings #-}
module Backend.Lua.Postprocess
  ( addOperators
  ) where

import qualified Data.Map as Map
import qualified Data.VarMap as VarMap
import qualified Data.VarSet as VarSet

import Backend.Lua.Syntax
import Backend.Lua.Emit
import Backend.Escape
import Core.Var

{-|
  Walks the Lua tree and identifies which operators are not fully applied
  (and so their variable is referenced), injecting them into the program.
 -}
addOperators :: [LuaStmt] -> [LuaStmt]
addOperators stmt =
  let ops = foldMap opsStmt stmt
  in foldr ((:) . genOp) stmt (VarSet.toList ops)

  where
    genOp :: CoVar -> LuaStmt
    genOp op =
      let name =  getVar op escapeScope
      in LuaLocal [LuaName name]
                  [ LuaFunction [left]
                    [LuaReturn (LuaFunction [right]
                                [LuaReturn (LuaBinOp (LuaRef left) (remapOp op) (LuaRef right))])] ]

    opsStmt :: LuaStmt -> VarSet.Set
    opsStmt (LuaDo b) = foldMap opsStmt b
    opsStmt (LuaAssign vs xs) = foldMap opsVar vs <> foldMap opsExpr xs
    opsStmt (LuaWhile t b) = opsExpr t <> foldMap opsStmt b
    opsStmt (LuaRepeat b t) = opsExpr t <> foldMap opsStmt b
    opsStmt (LuaIf c t f) = opsExpr c <> foldMap opsStmt t <> foldMap opsStmt f
    opsStmt (LuaFornum _ b e s bo) = opsExpr b <> opsExpr e <> opsExpr s <> foldMap opsStmt bo
    opsStmt (LuaFor _ g b) = foldMap opsExpr g <> foldMap opsStmt b
    opsStmt (LuaLocal _ e) = foldMap opsExpr e
    opsStmt (LuaReturn r) = opsExpr r
    opsStmt (LuaIfElse t) = foldMap (\(c, b) -> opsExpr c <> foldMap opsStmt b) t
    opsStmt LuaBreak = mempty
    opsStmt (LuaCallS f xs) = foldMap opsExpr (f:xs)
    opsStmt LuaBit{} = mempty

    opsExpr :: LuaExpr -> VarSet.Set
    opsExpr LuaNil = mempty
    opsExpr LuaTrue = mempty
    opsExpr LuaFalse = mempty
    opsExpr LuaDots = mempty
    opsExpr LuaNumber{} = mempty
    opsExpr LuaString{} = mempty
    opsExpr LuaBitE{} = mempty

    opsExpr (LuaCall f xs) = foldMap opsExpr (f:xs)
    opsExpr (LuaRef v) = opsVar v
    opsExpr (LuaFunction _ b) = foldMap opsStmt b
    opsExpr (LuaTable ks) = foldMap (\(k, v) -> opsExpr k <> opsExpr v) ks
    opsExpr (LuaBinOp l _ r) = opsExpr l <> opsExpr r

    opsVar :: LuaVar -> VarSet.Set
    opsVar (LuaName t) = maybe mempty VarSet.singleton (Map.lookup t opNames)
    opsVar (LuaIndex t k) = opsExpr t <> opsExpr k

    opNames = Map.filter (`VarMap.member` ops) (fromLua escapeScope)

    left  = LuaName "l"
    right = LuaName "r"
