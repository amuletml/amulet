module Backend.Compile where

import Syntax
import Backend.Lua

compileProgram :: [Toplevel] -> LuaStmt
compileProgram = LuaDo . compileProg where 
  compileProg (ForeignVal n s _:xs) = LuaLocal [lowerName n] [LuaBitE s]:compileProg xs
  compileProg (ValStmt _ _:xs) = compileProg xs
  compileProg (LetStmt vs:xs) = locals ns vs' ++ compileProg xs where
    (ns, vs') = unzip $ map compileLet vs
  compileProg [] = [LuaCallS (LuaRef (LuaName "main")) []]

compileLet :: (Var, Expr) -> (LuaVar, LuaExpr)
compileLet (n, e) = (lowerName n, compileExpr e)

compileExpr :: Expr -> LuaExpr
compileExpr (VarRef v) = LuaRef (lowerName v)
compileExpr (Let k s) = iife (locals ns vs ++ [LuaReturn (compileExpr s)]) where
  (ns, vs) = unzip $ map compileLet k
compileExpr (If c t e) = iife [LuaIf (compileExpr c) [LuaReturn (compileExpr t)]
                                                     [LuaReturn (compileExpr e)]]
compileExpr (App f x) = LuaCall (compileExpr f) [compileExpr x]
compileExpr (Fun v e) = LuaFunction [lowerName v] [LuaReturn (compileExpr e)]
compileExpr (Begin []) = iife []
compileExpr (Begin xs) = iife (LuaLocal [LuaName "_"] []:(map (exprStmt . compileExpr) (init xs) ++ [LuaReturn (compileExpr (last xs))])) where
  exprStmt = LuaAssign [LuaName "_"] . (:[])
compileExpr (Literal (LiInt x)) = LuaNumber (fromInteger x)
compileExpr (Literal (LiStr str)) = LuaString str

lowerName :: Var -> LuaVar
lowerName (Refresh a k) = case lowerName a of
                            LuaName x -> LuaName (x ++ show k)
                            _ -> error "absurd: no lowering to namespaces"
lowerName (Name a) = LuaName a

iife :: [LuaStmt] -> LuaExpr
iife b = LuaCall (LuaFunction [] b) []

locals :: [LuaVar] -> [LuaExpr] -> [LuaStmt]
locals xs ys = preDef ++ locals' xs ys where
  locals' (x:xs) (y:ys) = LuaAssign [x] [y]:locals xs ys
  locals' _ _ = []
  preDef = case xs of
             [] -> []
             xs -> [LuaLocal xs []]
