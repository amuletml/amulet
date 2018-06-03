{-# LANGUAGE OverloadedStrings #-}
module Backend.Lua.Postprocess
  ( addOperators
  , genOperator
  ) where

import qualified Data.Map as Map
import qualified Data.VarMap as VarMap
import qualified Data.VarSet as VarSet

import Backend.Lua.Syntax
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
                `Map.union` Map.fromList [ ( "__builtin_Lazy", vLAZY ), ( "__builtin_force", vForce ) ]


genOperator :: CoVar -> LuaStmt
genOperator op | op == vLAZY =
  LuaLocal [ LuaName "__builtin_Lazy" ]
           [ LuaFunction [ eks ]
             [ LuaReturn (LuaTable [ (LuaNumber 1, LuaRef eks)
                                   , (LuaNumber 2, LuaFalse)
                                   , (LuaString "__tag", LuaString "lazy")
                                   ]) ] ] where
  eks = LuaName "x"
genOperator op | op == vForce =
  LuaLocal [ LuaName "__builtin_force" ]
           [ LuaFunction [ eks ]
             [ LuaIf (LuaRef (LuaIndex (LuaRef eks) (LuaNumber 2)))
                [ LuaReturn ( LuaRef (LuaIndex (LuaRef eks) (LuaNumber 1)) ) ]
                [ LuaAssign [ LuaIndex (LuaRef eks) (LuaNumber 1)
                            , LuaIndex (LuaRef eks) (LuaNumber 2) ]
                            [ LuaCall (LuaRef (LuaIndex (LuaRef eks) (LuaNumber 1)))
                               []
                            , LuaTrue]
                  , LuaReturn (LuaRef (LuaIndex (LuaRef eks) (LuaNumber 1))) ] ] ] where
  eks = LuaName "x"
genOperator op =
  let name =  getVar op escapeScope
  in LuaLocal [LuaName name]
              [ LuaFunction [left]
                [LuaReturn (LuaFunction [right]
                            [LuaReturn (LuaBinOp (LuaRef left) (remapOp op) (LuaRef right))])] ]
  where
    left  = LuaName "l"
    right = LuaName "r"

{- Note: Tags on Lazy

   Lazy doesn't technically _need_ a tag, and including it in fact
   raises the memory usage of things. But, the REPL is taught to
   recognise __tag fields and print them as constructors, and so this
   looks better. -}
