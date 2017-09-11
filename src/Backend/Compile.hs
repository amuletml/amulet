module Backend.Compile
  ( compileProgram
  , compileLet
  , compileExpr
  , compileConstructors
  ) where

import Syntax
import Backend.Lua
import Control.Monad

type Returner = Maybe (LuaExpr -> LuaStmt)

alpha :: [String]
alpha = [1..] >>= flip replicateM ['a'..'z']

compileProgram :: [Toplevel] -> LuaStmt
compileProgram = LuaDo . compileProg where
  compileProg (ForeignVal n s _:xs) = LuaLocal [lowerName n] [LuaBitE s]:compileProg xs
  compileProg (ValStmt _ _:xs) = compileProg xs
  compileProg (LetStmt vs:xs) = locals ns vs' ++ compileProg xs where
    (ns, vs') = unzip $ map compileLet vs
  compileProg (TypeDecl _ _ cs:xs) = compileConstructors cs ++ compileProg xs
  compileProg [] = [LuaCallS (LuaRef (LuaName "main")) []]

compileConstructors :: [(Var, [Type])] -> [LuaStmt]
compileConstructors ((a, []):xs) -- unit constructors, easy
  = LuaLocal [lowerName a] [LuaTable [(LuaNumber 1, LuaString cn)]]:compileConstructors xs where
    (Name cn) = a
compileConstructors ((a, xs):ys) -- non-unit constructors, hard
  = LuaLocal [lowerName a] [vl]:compileConstructors ys where
    (LuaReturn vl) = fn xs alpha
    (Name cn) = a
    mkField x n = (LuaNumber x, LuaRef (LuaName n))
    fn (_:ts) (a:as) = LuaReturn $ LuaFunction [LuaName a] [fn ts as]
    fn [] _ = LuaReturn $ LuaTable ((LuaNumber 1, LuaString cn):take (length xs) (zipWith mkField [2..] alpha))
    fn _ _ = error "absurd"
compileConstructors [] = []

compileLet :: (Var, Expr) -> (LuaVar, LuaExpr)
compileLet (n, e) = (lowerName n, compileExpr e)

compileExpr :: Expr -> LuaExpr
compileExpr (VarRef v) = LuaRef (lowerName v)
compileExpr (App f x) = LuaCall (compileExpr f) [compileExpr x]
compileExpr (Fun (Capture v) e) = LuaFunction [lowerName v] (compileStmt (Just LuaReturn) e)
compileExpr (Fun Wildcard e) = LuaFunction [LuaName "_"] (compileStmt (Just LuaReturn) e)
compileExpr (Literal (LiInt x)) = LuaNumber (fromInteger x)
compileExpr (Literal (LiStr str)) = LuaString str
compileExpr (Literal (LiBool True)) = LuaTrue
compileExpr (Literal (LiBool False)) = LuaFalse
compileExpr s@(Let _ _) = compileIife s
compileExpr s@(If _ _ _) = compileIife s
compileExpr s@(Begin _) = compileIife s
compileExpr s@(Match _ _) = compileIife s
compileExpr (BinOp l (VarRef (Name o)) r) = LuaBinOp (compileExpr l) (remapOp o) (compileExpr r)
compileExpr (BinOp _ _ _) = error "absurd: never parsed"

compileStmt :: Returner -> Expr -> [LuaStmt]
compileStmt r e@(VarRef _) = pureReturn r $ compileExpr e
compileStmt r e@(Literal _) = pureReturn r $ compileExpr e
compileStmt r e@(Fun _ _) = pureReturn r $ compileExpr e
compileStmt r e@BinOp{} = pureReturn r $ compileExpr e
compileStmt r (Let k c) = let (ns, vs) = unzip $ map compileLet k in
                          (locals ns vs ++ compileStmt r c)
compileStmt r (If c t e) = [LuaIf (compileExpr c) (compileStmt r t) (compileStmt r e)]
compileStmt r (Begin xs) = concatMap (compileStmt Nothing) (init xs) ++ compileStmt r (last xs)

compileStmt _ (Match _ []) = error "Cannot have empty match"
compileStmt r (Match e ((Wildcard, c):_)) = compileStmt Nothing e ++ compileStmt r c
compileStmt r (Match e ((Capture v, c):_)) = LuaLocal [lowerName v] [] :
                                             compileStmt (Just (LuaAssign [lowerName v] . (:[]))) e ++
                                             compileStmt r c

compileStmt Nothing (App f x) = [LuaCallS (compileExpr f) [compileExpr x]]
compileStmt (Just r) e@(App _ _) = [r (compileExpr e)]

lowerName :: Var -> LuaVar
lowerName (Refresh a k) = case lowerName a of
                            LuaName x -> LuaName (x ++ show k)
                            _ -> error "absurd: no lowering to namespaces"
lowerName (Name a) = LuaName a

iife :: [LuaStmt] -> LuaExpr
iife b = LuaCall (LuaFunction [] b) []

compileIife :: Expr -> LuaExpr
compileIife = iife . compileStmt (Just LuaReturn)

locals :: [LuaVar] -> [LuaExpr] -> [LuaStmt]
locals xs ys = preDef ++ locals' xs ys where
  locals' (x:xs) (y:ys) = LuaAssign [x] [y]:locals' xs ys
  locals' _ _ = []
  preDef = case xs of
             [] -> []
             xs -> [LuaLocal xs []]

pureReturn :: Returner -> LuaExpr -> [LuaStmt]
pureReturn Nothing _ = []
pureReturn (Just r) e = [r e]

remapOp :: String -> String
remapOp "^" = ".."
remapOp "**" = "^"
remapOp "<>" = "~="
remapOp x = x
