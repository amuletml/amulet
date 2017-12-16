{-# LANGUAGE OverloadedStrings #-}
module Backend.Compile
  ( compileProgram
  , compileLet
  , compileExpr
  ) where

import Control.Monad.Gen
import Control.Monad

import Control.Monad.Infer

import Backend.Lua
import Core.Core
import Core.Types
import Syntax

import Data.Semigroup ((<>))

import qualified Data.Map.Strict as M

import qualified Data.Text as T
import Data.Text (Text)
import Data.List (sortOn)
import Data.Maybe (fromMaybe)

type Returner = Maybe (LuaExpr -> LuaStmt)

alpha :: [Text]
alpha = map T.pack ([1..] >>= flip replicateM ['a'..'z'])

compileProgram :: Env -> [CoStmt] -> LuaStmt
compileProgram ev = LuaDo . (extendDef:) . compileProg where
  compileProg (CosForeign n' t s:xs)
    | arity t > 1
    = let ags = map LuaName $ take (arity t) alpha
          mkF (a:ag) bd = LuaFunction [a] [LuaReturn (mkF ag bd)]
          mkF [] bd = bd
       in LuaLocal [LuaName ("__" <> n)] [LuaBitE s]
        : LuaLocal [LuaName n]
            [mkF ags
              (LuaCall (LuaRef (LuaName ("__" <> n)))
                            (map LuaRef ags))]
        :compileProg xs
    | otherwise = LuaLocal [LuaName n] [LuaBitE s]:compileProg xs
    where n = getName n'
  compileProg (CosLet vs:xs) = locals ns vs' ++ compileProg xs where
    (ns, vs') = unzip $ map compileLet vs
  compileProg (CosType _ _ cs:xs) = map compileConstructor cs ++ compileProg xs
  compileProg [] = [LuaCallS (main ev) []] where
    main = LuaRef . LuaName . getTaggedName . head . sortOn key . filter isMain . M.keys . values
    isMain (TgName x _) = x == "main"
    isMain _ = False
    key (TgName k _) = k
    key _ = undefined

compileConstructor :: (Var Resolved, CoType) -> LuaStmt
compileConstructor (var, ty) | arity ty == 0
  = LuaLocal [lowerName var] [LuaTable [(LuaNumber 1, LuaString cn)]] where
    cn = getName var
compileConstructor (var, _) -- non-unit constructors, hard
  = LuaLocal [lowerName var] [vl] where
    vl = LuaFunction [LuaName "x"] [LuaReturn (LuaTable [(LuaNumber 1, LuaString cn), (LuaNumber 2, LuaRef (LuaName "x"))])]
    cn = getName var

compileLet :: (Var Resolved, CoType, CoTerm) -> (LuaVar, LuaExpr)
compileLet (n, _, e) = (lowerName n, compileExpr e)

compileExpr :: CoTerm -> LuaExpr

-- First handle binary operators
compileExpr (CotRef v _) | isBinOp v
  = LuaFunction [left] [LuaReturn (LuaFunction
                                    [right]
                                    [LuaReturn (LuaBinOp (LuaRef left) (remapOp (getTaggedName v)) (LuaRef right))])]
    where left  = LuaName "l"
          right = LuaName "r"
compileExpr (CotApp (CotApp (CotRef f _) left) right) | isBinOp f
  = LuaBinOp (compileExpr left) (remapOp (getTaggedName f)) (compileExpr right)
compileExpr (CotApp (CotRef f _) left) | isBinOp f
  = LuaFunction [name] [LuaReturn (LuaBinOp (compileExpr left) (remapOp (getTaggedName f)) (LuaRef name))]
    where name = LuaName "__r"

compileExpr (CotRef v _) = LuaRef (lowerName v)
compileExpr (CotLam Small (v, _) e) = LuaFunction [lowerName v] (compileStmt (Just LuaReturn) e)
compileExpr (CotApp f x) = LuaCall (compileExpr f) [compileExpr x]
compileExpr (CotLam Big _ e) = compileExpr e
compileExpr (CotTyApp f _) = compileExpr f

compileExpr (CotLit (ColInt x))   = LuaNumber (fromInteger x)
compileExpr (CotLit (ColStr str)) = LuaString str
compileExpr (CotLit ColTrue)      = LuaTrue
compileExpr (CotLit ColFalse)     = LuaFalse
compileExpr (CotLit ColUnit)      = LuaNil -- evil!
compileExpr (CotLit ColRecNil)    = LuaTable []

compileExpr (CotExtend (CotLit ColRecNil) fs)
  = LuaTable (map (\(f, _, e) -> (LuaString f, compileExpr e)) fs)
compileExpr s@CotExtend{} = compileIife s

compileExpr s@CotLet{}    = compileIife s
compileExpr s@CotBegin{}  = compileIife s
compileExpr s@CotMatch{}  = compileIife s

global :: String -> LuaExpr
global x = LuaRef (LuaIndex (LuaRef (LuaName "_G")) (LuaString (T.pack x)))

compileStmt :: Returner -> CoTerm -> [LuaStmt]
compileStmt r e@CotRef{} = pureReturn r $ compileExpr e
compileStmt r e@CotLam{} = pureReturn r $ compileExpr e
compileStmt r e@CotLit{} = pureReturn r $ compileExpr e
compileStmt r (CotLet k c) = let (ns, vs) = unzip $ map compileLet k in
                              (locals ns vs ++ compileStmt r c)
compileStmt r (CotBegin xs x) = concatMap (compileStmt Nothing) xs ++ compileStmt r x
compileStmt r (CotMatch s ps) = runGen (compileMatch r s ps)

compileStmt Nothing e@CotApp{} = case compileExpr e of
                                     LuaCall f a -> [LuaCallS f a]
                                     expr -> [LuaLocal [LuaName "_"] [expr]]
compileStmt (Just r) e@CotApp{} = [r (compileExpr e)]
compileStmt r (CotTyApp f _) = compileStmt r f

compileStmt r e@(CotExtend (CotLit ColRecNil) _) = dirtyReturn r (compileExpr e) -- TODO: Flatten if r is nothing?
compileStmt r (CotExtend tbl exs) = [ LuaLocal [old, new] [compileExpr tbl, LuaTable []]
                                    , LuaFor [k, v] [LuaCall (LuaRef pairs) [LuaRef old]]
                                      [LuaAssign [LuaIndex (LuaRef new) (LuaRef (LuaName k))] [LuaRef (LuaName v)]] ] ++
                                    map (\(f, _, e) -> LuaAssign [LuaIndex (LuaRef new) (LuaString f)] [compileExpr e]) exs ++
                                    pureReturn r (LuaRef new)
  where old = LuaName (T.pack "__o")
        new = LuaName (T.pack "__n")
        k = T.pack "k"
        v = T.pack "v"
        pairs = LuaName (T.pack "pairs")

lowerName :: Var Resolved -> LuaVar
lowerName = LuaName . getTaggedName

lowerKey :: Var Resolved -> LuaExpr
lowerKey = LuaString . getTaggedName

getName :: Var Resolved -> Text
getName = getTaggedName

getTaggedName :: Var Resolved -> Text
getTaggedName (TgName t i) = t <> T.pack (show i)
getTaggedName (TgInternal t) = t

iife :: [LuaStmt] -> LuaExpr
iife b = LuaCall (LuaFunction [] b) []

compileIife :: CoTerm -> LuaExpr
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

dirtyReturn :: Returner -> LuaExpr -> [LuaStmt]
dirtyReturn Nothing e = [LuaLocal [LuaName (T.pack "_")] [e]]
dirtyReturn (Just r) e = [r e]

foldAnd :: [LuaExpr] -> LuaExpr
foldAnd = foldl1 k where
  k l r
    | r == LuaTrue = l
    | l == LuaTrue = r
    | r == LuaFalse || l == LuaFalse = LuaFalse
    | otherwise = LuaBinOp l "and" r

patternTest :: CoPattern -> LuaExpr ->  LuaExpr
patternTest (CopCapture _) _    = LuaTrue
patternTest (CopLit l)     vr   = LuaBinOp (compileExpr (CotLit l)) "==" vr
patternTest (CopRecord rs) vr   = foldAnd (map test rs) where
  test (var', pat) = patternTest pat (LuaRef (LuaIndex vr (LuaString var')))
patternTest (CopConstr con) vr  = foldAnd [tag con vr]
patternTest (CopDestr con p) vr = foldAnd [tag con vr, patternTest p (LuaRef (LuaIndex vr (LuaNumber 2)))]

tag :: Var Resolved -> LuaExpr -> LuaExpr
tag con vr = LuaBinOp (LuaRef (LuaIndex vr (LuaNumber 1))) "==" (lowerKey con)

patternBindings :: CoPattern -> LuaExpr -> [(LuaVar, LuaExpr)]
patternBindings (CopLit _) _      = []
patternBindings (CopCapture n) v  = [(lowerName n, v)]
patternBindings (CopConstr _) _   = []
patternBindings (CopDestr _ p) vr = patternBindings p (LuaRef (LuaIndex vr (LuaNumber 2)))
patternBindings (CopRecord rs) vr = concatMap (index vr) rs where
  index vr (var', pat) = patternBindings pat (LuaRef (LuaIndex vr (LuaString var')))

compileMatch :: Returner -> CoTerm -> [(CoPattern, CoType, CoTerm)] -> Gen Int [LuaStmt]
compileMatch r ex ps = do
  x <- (LuaName . ("__" <>) . (alpha !!)) <$> gen -- matchee
  let gen (p, _, c) = ( patternTest p (LuaRef x)
                      , case patternBindings p (LuaRef x) of
                          [] -> []
                          xs -> [uncurry LuaLocal (unzip xs)]
                        ++ compileStmt r c )
  pure $ compileStmt (Just $ LuaLocal [x] . (:[])) ex
       ++ [ LuaIfElse (map gen ps) ]

--- This is a hack, but we need this for compiling record extension
extendDef :: LuaStmt
extendDef = LuaAssign [ extend ]
                      [ LuaFunction [ t, k, v ]
                         [ LuaLocal [ out ] [ LuaTable [] ]
                         , LuaFor [ "k_", "v_" ] [ LuaCall (global "pairs") [ LuaRef t ] ]
                            [ LuaAssign [ LuaIndex (LuaRef out) (LuaRef k') ] [ LuaRef v' ] ]
                         , LuaAssign [ LuaIndex (LuaRef out) (LuaRef k) ] [ LuaRef v ]
                         , LuaReturn (LuaRef out) ] ]
  where (LuaRef extend) = global "extend"
        t = LuaName "t"
        k = LuaName "k"
        v = LuaName "v"
        out = LuaName "out"
        v' = LuaName "v_"
        k' = LuaName "k_"

ops :: M.Map Text Text
ops = M.fromList [ ("+", "+")
                 , ("-", "-")
                 , ("*", "*")
                 , ("/", "/")
                 , ("**", "^")
                 , ("^", "..")
                 , ("<", "<")
                 , (">", ">")
                 , (">=", ">=")
                 , ("<=", "<=")
                 , ("==", "==")
                 , ("<>", "~=")
                 , ("||", "or")
                 , ("&&", "and") ]

isBinOp :: Var Resolved -> Bool
isBinOp (TgInternal v) = M.member v ops
isBinOp _ = False

remapOp :: Text -> Text
remapOp x = fromMaybe x (M.lookup x ops)
