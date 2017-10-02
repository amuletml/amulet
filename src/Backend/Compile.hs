{-# LANGUAGE OverloadedStrings #-}
module Backend.Compile
  ( compileProgram
  , compileLet
  , compileExpr
  , compileConstructors
  ) where

import Control.Comonad
import Control.Monad.Gen
import Control.Monad

import Backend.Lua
import Syntax

import qualified Data.Text as T
import Data.Text (Text)
import Data.Semigroup ((<>))

type Returner = Maybe (LuaExpr -> LuaStmt)

alpha :: [Text]
alpha = map (T.pack) ([1..] >>= flip replicateM ['a'..'z'])

compileProgram :: [Toplevel 'TypedPhase a] -> LuaStmt
compileProgram = LuaDo . compileProg where
  compileProg (ForeignVal n' s t:xs)
    = let genCurried n (TyArr _ a) ags bd = LuaFunction [LuaName (alpha !! n)] [LuaReturn (genCurried (succ n) a (LuaRef (LuaName (alpha !! n)):ags) bd)]
          genCurried _ _ [] bd = bd
          genCurried _ _ ags bd = LuaCall bd (reverse ags)
          (Name n) = n'
       in LuaLocal [LuaName ("__" <> n)] [LuaBitE s]
        : LuaLocal [LuaName n] [genCurried 0 t [] (LuaRef (LuaName ("__" <> n)))]:compileProg xs
  compileProg (ValStmt _ _:xs) = compileProg xs
  compileProg (LetStmt vs:xs) = locals ns vs' ++ compileProg xs where
    (ns, vs') = unzip $ map compileLet vs
  compileProg (TypeDecl _ _ cs:xs) = compileConstructors cs ++ compileProg xs
  compileProg [] = [LuaCallS (LuaRef (LuaName "main")) []]

compileConstructors :: [(Var 'TypedPhase, [Type 'TypedPhase])] -> [LuaStmt]
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

compileLet :: (Var 'TypedPhase, Expr 'TypedPhase a) -> (LuaVar, LuaExpr)
compileLet (n, e) = (lowerName n, compileExpr e)

compileExpr :: Expr 'TypedPhase a -> LuaExpr
compileExpr (VarRef v _) = LuaRef (lowerName v)
compileExpr (App f x _) = LuaCall (compileExpr f) [compileExpr x]
compileExpr (Fun (Capture v) e _) = LuaFunction [lowerName v] (compileStmt (Just LuaReturn) e)
compileExpr (Fun Wildcard e _) = LuaFunction [LuaName "_"] (compileStmt (Just LuaReturn) e)
compileExpr f@(Fun k e _) = LuaFunction [LuaName "__arg__"] (compileStmt (Just LuaReturn)
                                                              (Match (VarRef (Name "__arg__") (extract f)) [(k, e)] (extract f)))
compileExpr (Literal (LiInt x) _)       = LuaNumber (fromInteger x)
compileExpr (Literal (LiStr str) _)     = LuaString str
compileExpr (Literal (LiBool True) _)   = LuaTrue
compileExpr (Literal (LiBool False) _)  = LuaFalse
compileExpr (Literal LiUnit _)          = LuaNil -- evil!
compileExpr s@(Let{}) = compileIife s
compileExpr s@(If{}) = compileIife s
compileExpr s@(Begin{}) = compileIife s
compileExpr s@(Match{}) = compileIife s
compileExpr (BinOp l (VarRef (Name o) _) r _) = LuaBinOp (compileExpr l) (remapOp o) (compileExpr r)
compileExpr (BinOp{}) = error "absurd: never parsed"

compileStmt :: Returner -> Expr 'TypedPhase a -> [LuaStmt]
compileStmt r e@(VarRef{}) = pureReturn r $ compileExpr e
compileStmt r e@(Literal{}) = pureReturn r $ compileExpr e
compileStmt r e@(Fun{}) = pureReturn r $ compileExpr e
compileStmt r e@(BinOp{}) = pureReturn r $ compileExpr e
compileStmt r (Let k c _) = let (ns, vs) = unzip $ map compileLet k in
                              (locals ns vs ++ compileStmt r c)
compileStmt r (If c t e _) = [LuaIf (compileExpr c) (compileStmt r t) (compileStmt r e)]
compileStmt r (Begin xs _) = concatMap (compileStmt Nothing) (init xs) ++ compileStmt r (last xs)
compileStmt r (Match s ps _) = runGen (compileMatch r s ps)
compileStmt Nothing (App f x _) = [LuaCallS (compileExpr f) [compileExpr x]]
compileStmt (Just r) e@(App{}) = [r (compileExpr e)]

lowerName :: Var 'TypedPhase -> LuaVar
lowerName (Refresh a k) = case lowerName a of
                            LuaName x -> LuaName (x <> T.pack (show k))
                            _ -> error "absurd: no lowering to namespaces"
lowerName (Name a) = LuaName a

iife :: [LuaStmt] -> LuaExpr
iife b = LuaCall (LuaFunction [] b) []

compileIife :: Expr 'TypedPhase a -> LuaExpr
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

remapOp :: Text -> Text
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

patternTest :: Pattern 'TypedPhase -> LuaExpr ->  LuaExpr
patternTest Wildcard  _ = LuaTrue
patternTest Capture{} _ = LuaTrue
patternTest (PType p _) t = patternTest p t
patternTest (Destructure con ps) vr
  = foldAnd (table vr:tag con vr:zipWith3 innerTest ps (repeat vr) [2..]) where
    innerTest p v k = patternTest p . LuaRef . LuaIndex v . LuaNumber . fromInteger $ k
    table ex = LuaBinOp (LuaCall (LuaRef (LuaName "type")) [ex]) "==" (LuaString "table")
    tag (Name con) vr = LuaBinOp (LuaRef (LuaIndex vr (LuaNumber 1))) "==" (LuaString con)
    tag _ _ = error "absurd: no renaming"

patternBindings :: Pattern 'TypedPhase -> LuaExpr -> [(LuaVar, LuaExpr)]
patternBindings Wildcard  _ = []
patternBindings (Capture (Name k)) v = [(LuaName k, v)]
patternBindings (Capture _) _ = error "absurd: no renaming"
patternBindings (PType p _) t = patternBindings p t
patternBindings (Destructure _ ps) vr
  = concat $ zipWith3 innerBind ps (repeat vr) [2..] where
    innerBind p v k = patternBindings p . LuaRef . LuaIndex v . LuaNumber . fromInteger $ k

compileMatch :: Returner -> Expr 'TypedPhase a -> [(Pattern 'TypedPhase, Expr 'TypedPhase a)] -> Gen Int [LuaStmt]
compileMatch r ex ps = do
  x <- (LuaName . ("__" <>) . (alpha !!)) <$> gen -- matchee
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
