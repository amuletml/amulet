module Backend.Compile
  ( compileProgram
  , compileLet
  , compileExpr
  , compileConstructors
  ) where

import Control.Monad.Gen
import Control.Monad

import Backend.Lua
import Syntax

type Returner = Maybe (LuaExpr -> LuaStmt)

alpha :: [String]
alpha = [1..] >>= flip replicateM ['a'..'z']

compileProgram :: [Toplevel] -> LuaStmt
compileProgram = LuaDo . compileProg where
  compileProg (ForeignVal n' s t:xs)
    = let genCurried n (TyArr _ a) ags bd = LuaFunction [LuaName (alpha !! n)] [LuaReturn (genCurried (succ n) a (LuaRef (LuaName (alpha !! n)):ags) bd)]
          genCurried _ _ [] bd = bd
          genCurried _ _ ags bd = LuaCall bd (reverse ags)
          (Name n) = n'
       in LuaLocal [LuaName ("__" ++ n)] [LuaBitE s]
        : LuaLocal [LuaName n] [genCurried 0 t [] (LuaRef (LuaName ("__" ++ n)))]:compileProg xs
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
compileExpr (_, _, VarRef v) = LuaRef (lowerName v)
compileExpr (_, _, App f x) = LuaCall (compileExpr f) [compileExpr x]
compileExpr (_, _, Fun (Capture v) e) = LuaFunction [lowerName v] (compileStmt (Just LuaReturn) e)
compileExpr (_, _, Fun Wildcard e) = LuaFunction [LuaName "_"] (compileStmt (Just LuaReturn) e)
compileExpr (_, _, Fun k e) = LuaFunction [LuaName "__arg__"] (compileStmt (Just LuaReturn) 
                                                              (undefined, undefined, Match (undefined, undefined, VarRef (Name "__arg__")) [(k, e)]))
compileExpr (_, _, Literal (LiInt x)) = LuaNumber (fromInteger x)
compileExpr (_, _, Literal (LiStr str)) = LuaString str
compileExpr (_, _, Literal (LiBool True)) = LuaTrue
compileExpr (_, _, Literal (LiBool False)) = LuaFalse
compileExpr (_, _, Literal LiUnit) = LuaNil -- evil!
compileExpr s@(_, _, Let _ _) = compileIife s
compileExpr s@(_, _, If _ _ _) = compileIife s
compileExpr s@(_, _, Begin _) = compileIife s
compileExpr s@(_, _, Match _ _) = compileIife s
compileExpr (_, _, BinOp l (_, _, VarRef (Name o)) r) = LuaBinOp (compileExpr l) (remapOp o) (compileExpr r)
compileExpr (_, _, BinOp _ _ _) = error "absurd: never parsed"

compileStmt :: Returner -> Expr -> [LuaStmt]
compileStmt r e@(_, _, VarRef _) = pureReturn r $ compileExpr e
compileStmt r e@(_, _, Literal _) = pureReturn r $ compileExpr e
compileStmt r e@(_, _, Fun _ _) = pureReturn r $ compileExpr e
compileStmt r e@(_, _, BinOp{}) = pureReturn r $ compileExpr e
compileStmt r (_, _, Let k c) = let (ns, vs) = unzip $ map compileLet k in
                          (locals ns vs ++ compileStmt r c)
compileStmt r (_, _, If c t e) = [LuaIf (compileExpr c) (compileStmt r t) (compileStmt r e)]
compileStmt r (_, _, Begin xs) = concatMap (compileStmt Nothing) (init xs) ++ compileStmt r (last xs)
compileStmt r (_, _, Match s ps) = runGen (compileMatch r s ps)
compileStmt Nothing (_, _, App f x) = [LuaCallS (compileExpr f) [compileExpr x]]
compileStmt (Just r) e@(_, _, App _ _) = [r (compileExpr e)]

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

foldAnd :: [LuaExpr] -> LuaExpr
foldAnd = foldl1 k where
  k l r
    | r == LuaTrue = l
    | l == LuaTrue = r
    | r == LuaFalse || l == LuaFalse = LuaFalse
    | otherwise = LuaBinOp l "and" r

patternTest :: Pattern -> LuaExpr ->  LuaExpr
patternTest Wildcard  _ = LuaTrue
patternTest Capture{} _ = LuaTrue
patternTest (PType p _) t = patternTest p t
patternTest (Destructure con ps) vr
  = foldAnd (table vr:tag con vr:zipWith3 innerTest ps (repeat vr) [2..]) where
    innerTest p v k = patternTest p . LuaRef . LuaIndex v . LuaNumber . fromInteger $ k
    table ex = LuaBinOp (LuaCall (LuaRef (LuaName "type")) [ex]) "==" (LuaString "table")
    tag (Name con) vr = LuaBinOp (LuaRef (LuaIndex vr (LuaNumber 1))) "==" (LuaString con)
    tag _ _ = error "absurd: no renaming"

patternBindings :: Pattern -> LuaExpr -> [(LuaVar, LuaExpr)]
patternBindings Wildcard  _ = []
patternBindings (Capture (Name k)) v = [(LuaName k, v)]
patternBindings (Capture _) _ = error "absurd: no renaming"
patternBindings (PType p _) t = patternBindings p t
patternBindings (Destructure _ ps) vr
  = concat $ zipWith3 innerBind ps (repeat vr) [2..] where
    innerBind p v k = patternBindings p . LuaRef . LuaIndex v . LuaNumber . fromInteger $ k

compileMatch :: Returner -> Expr -> [(Pattern, Expr)] -> Gen Int [LuaStmt]
compileMatch r ex ps = do
  x <- (LuaName . ("__" ++ ) . (alpha !!)) <$> gen -- matchee
  let gen ((p, c):ps) = ( patternTest p (LuaRef x)
                        , let pbs = patternBindings p (LuaRef x)
                              (a, b) = unzip pbs
                           in case a of
                                [] -> []
                                _ -> [LuaLocal a b]
                          ++ compileStmt r c )
                        : gen ps
      gen [] = [err]
      err = ( LuaTrue
            , [LuaCallS (LuaRef (LuaName "error"))
                        [LuaString "Pattern matching failure in match expression"]])
  pure $ compileStmt (Just $ LuaLocal [x] . (:[])) ex
       ++ [ LuaIfElse (gen ps) ]
