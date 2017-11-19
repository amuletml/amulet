{-# LANGUAGE OverloadedStrings #-}
module Backend.Compile
  ( compileProgram
  , compileLet
  , compileExpr
  , compileConstructors
  ) where

import Control.Monad.Gen
import Control.Monad

import Control.Monad.Infer

import Backend.Lua
import Syntax

import Control.Arrow ((***))

import Data.Semigroup ((<>))
import Data.Spanned

import qualified Data.Map.Strict as M

import qualified Data.Text as T
import Data.Text (Text)

import Types.Wellformed (arity)

import Data.List (sortOn)
import Pretty (uglyPrint)

type Returner = Maybe (LuaExpr -> LuaStmt)

alpha :: [Text]
alpha = map T.pack ([1..] >>= flip replicateM ['a'..'z'])

compileProgram :: Env -> [Toplevel Typed] -> LuaStmt
compileProgram ev = LuaDo . (extendDef:) . compileProg where
  compileProg (ForeignVal n' s t _:xs)
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
  compileProg (ValStmt{}:xs) = compileProg xs
  compileProg (LetStmt vs _:xs) = locals ns vs' ++ compileProg xs where
    (ns, vs') = unzip $ map compileLet vs
  compileProg (TypeDecl _ _ cs _:xs) = compileConstructors cs ++ compileProg xs
  compileProg [] = [LuaCallS (main ev) []] where
    main = LuaRef . LuaName . getTaggedName . head . sortOn key . filter isMain . M.keys . values
    isMain (TgName x _) = x == "main"
    isMain _ = False
    key (TgName k _) = k
    key _ = undefined

compileConstructors :: [Constructor Typed] -> [LuaStmt]
compileConstructors (UnitCon a _:xs) -- unit constructors, easy
  = LuaLocal [lowerName a] [LuaTable [(LuaNumber 1, LuaString cn)]]:compileConstructors xs where
    cn = getName a
compileConstructors (ArgCon a _ _:ys) -- non-unit constructors, hard
  = LuaLocal [lowerName a] [vl]:compileConstructors ys where
    vl = LuaFunction [LuaName "x"] [LuaReturn (LuaTable [(LuaNumber 1, LuaString cn), (LuaNumber 2, LuaRef (LuaName "x"))])]
    cn = getName a
-- GADT constructors, hardest - we have to figure out the representation from the type
compileConstructors (GADTCon nm ty _:ys)
  | TyArr{} <- ty
  = LuaLocal [lowerName nm] [vl]:compileConstructors ys
  | TyCons _ TyArr{} <- ty
  = LuaLocal [lowerName nm] [vl]:compileConstructors ys
  | TyForall _ t <- ty = compileConstructors (GADTCon nm t undefined:ys)
  | otherwise
  = LuaLocal [lowerName nm] [LuaTable [(LuaNumber 1, LuaString cn)]]:compileConstructors ys
  where
    vl = LuaFunction [LuaName "x"] [LuaReturn (LuaTable [(LuaNumber 1, LuaString cn), (LuaNumber 2, LuaRef (LuaName "x"))])]
    cn = getName nm
compileConstructors [] = []

compileLet :: (Var Typed, Expr Typed) -> (LuaVar, LuaExpr)
compileLet (n, e) = (lowerName n, compileExpr e)

compileExpr :: Expr Typed -> LuaExpr
compileExpr (VarRef v _) = LuaRef (lowerName v)
compileExpr (EHasType e _ _) = compileExpr e
compileExpr (Hole v ann) = LuaCall (global "error") [LuaString msg] where
  msg = "Deferred typed hole " <> uglyPrint v <> " (from " <> uglyPrint ann <> ")"
compileExpr (Access rec f _) = LuaRef (LuaIndex (compileExpr rec) (LuaString f))
compileExpr (App f x _) = LuaCall (compileExpr f) [compileExpr x]
compileExpr (Fun (Capture v _) e _) = LuaFunction [lowerName v] (compileStmt (Just LuaReturn) e)
compileExpr (Fun (Wildcard _) e _) = LuaFunction [LuaName "_"] (compileStmt (Just LuaReturn) e)
compileExpr f@(Fun k e _) = LuaFunction [LuaName "__arg__"]
                              (compileStmt (Just LuaReturn)
                                           (Match (VarRef (TvName Flexible (TgInternal "__arg__") undefined)
                                                          (annotation f))
                                                  [(k, e)] (annotation f)))
compileExpr (Tuple [] _) = LuaNil -- evil!
compileExpr (Tuple [x] _) = compileExpr x -- the root of all evil!
compileExpr (Tuple xs _) = LuaTable (zip (map LuaNumber [1..]) (map compileExpr xs))
compileExpr (Literal (LiInt x) _)       = LuaNumber (fromInteger x)
compileExpr (Literal (LiStr str) _)     = LuaString str
compileExpr (Literal (LiBool True) _)   = LuaTrue
compileExpr (Literal (LiBool False) _)  = LuaFalse
compileExpr (Literal LiUnit _)          = LuaNil -- evil!
compileExpr (Record rows _)             = LuaTable (map (LuaString *** compileExpr) rows)
compileExpr (RecordExt rec exts _)
  = let rec' = compileExpr rec
        extend :: [(LuaExpr, LuaExpr)] -> LuaExpr -> LuaExpr
        extend ((v, e):xs) rec = LuaCall (global "extend") [extend xs rec, v, e]
        extend [] x = x
     in extend (map (LuaString *** compileExpr) exts) rec'
compileExpr s@Let{} = compileIife s
compileExpr s@If{} = compileIife s
compileExpr s@Begin{} = compileIife s
compileExpr s@Match{} = compileIife s
compileExpr (BinOp l (VarRef (TvName _ (TgInternal o) _) _) r _) = LuaBinOp (compileExpr l) (remapOp o) (compileExpr r)
compileExpr BinOp{} = error "absurd: never parsed"
compileExpr LeftSection{} = error "absurd: desugarer removes left sections"
compileExpr RightSection{} = error "absurd: desugarer removes right sections"
compileExpr BothSection{} = error "absurd: desugarer removes both-side sections"
compileExpr AccessSection{} = error "absurd: desugarer removes access sections"

global :: String -> LuaExpr
global x = LuaRef (LuaIndex (LuaRef (LuaName "_G")) (LuaString (T.pack x)))

compileStmt :: Returner -> Expr Typed -> [LuaStmt]
compileStmt r e@VarRef{} = pureReturn r $ compileExpr e
compileStmt r e@Hole{} = pureReturn r $ compileExpr e
compileStmt r e@Access{} = pureReturn r $ compileExpr e
compileStmt r e@Literal{} = pureReturn r $ compileExpr e
compileStmt r e@Fun{} = pureReturn r $ compileExpr e
compileStmt r e@BinOp{} = pureReturn r $ compileExpr e
compileStmt r e@Record{} = pureReturn r $ compileExpr e
compileStmt r e@RecordExt{} = pureReturn r $ compileExpr e
compileStmt r e@Tuple{} = pureReturn r $ compileExpr e
compileStmt r (EHasType e _ _) = compileStmt r e
compileStmt r (Let k c _) = let (ns, vs) = unzip $ map compileLet k in
                              (locals ns vs ++ compileStmt r c)
compileStmt r (If c t e _) = [LuaIf (compileExpr c) (compileStmt r t) (compileStmt r e)]
compileStmt r (Begin xs _) = concatMap (compileStmt Nothing) (init xs) ++ compileStmt r (last xs)
compileStmt r (Match s ps _) = runGen (compileMatch r s ps)
compileStmt Nothing (App f x _) = [LuaCallS (compileExpr f) [compileExpr x]]
compileStmt (Just r) e@App{} = [r (compileExpr e)]

-- These are absurd.
compileStmt r e@LeftSection{} = pureReturn r $ compileExpr e
compileStmt r e@RightSection{} = pureReturn r $ compileExpr e
compileStmt r e@BothSection{} = pureReturn r $ compileExpr e
compileStmt r e@AccessSection{} = pureReturn r $ compileExpr e

lowerName :: Var Typed -> LuaVar
lowerName (TvName _ a _) = LuaName (getTaggedName a)

lowerKey :: Var Typed -> LuaExpr
lowerKey (TvName _ a _) = LuaString (getTaggedName a)

getName :: Var Typed -> Text
getName (TvName _ a _) = getTaggedName a

getTaggedName :: Var Resolved -> Text
getTaggedName (TgName t i) = t <> T.pack (show i)
getTaggedName (TgInternal t) = t

iife :: [LuaStmt] -> LuaExpr
iife b = LuaCall (LuaFunction [] b) []

compileIife :: Expr Typed -> LuaExpr
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
remapOp "||" = "or"
remapOp "&&" = "and"
remapOp x = x

foldAnd :: [LuaExpr] -> LuaExpr
foldAnd = foldl1 k where
  k l r
    | r == LuaTrue = l
    | l == LuaTrue = r
    | r == LuaFalse || l == LuaFalse = LuaFalse
    | otherwise = LuaBinOp l "and" r

patternTest :: Pattern Typed -> LuaExpr ->  LuaExpr
patternTest Wildcard{}    _ = LuaTrue
patternTest Capture{}     _ = LuaTrue
patternTest (PType p _ _) t = patternTest p t
patternTest (PTuple ps _) vr
  | [x] <- ps
  = patternTest x vr
  | otherwise
  = foldAnd (table vr:zipWith3 innerTest ps (repeat vr) [1..]) where
    innerTest p v = patternTest p . LuaRef . LuaIndex v . LuaNumber . fromInteger
patternTest (PRecord rs _) vr = foldAnd (table vr:map (test vr) rs) where
  test vr (var', pat) = patternTest pat (LuaRef (LuaIndex vr (LuaString var')))
patternTest (Destructure con p' _) vr
  | Just p <- p'
  = foldAnd [table vr, tag con vr, patternTest p (LuaRef (LuaIndex vr (LuaNumber 2)))]
  | Nothing <- p'
  = foldAnd [table vr, tag con vr ]

tag :: Var Typed -> LuaExpr -> LuaExpr
tag con vr = LuaBinOp (LuaRef (LuaIndex vr (LuaNumber 1))) "==" (lowerKey con)

table :: LuaExpr -> LuaExpr
table ex = LuaBinOp (LuaCall (LuaRef (LuaName "type")) [ex]) "==" (LuaString "table")

patternBindings :: Pattern Typed -> LuaExpr -> [(LuaVar, LuaExpr)]
patternBindings Wildcard{}  _ = []
patternBindings (Capture n _) v = [(lowerName n, v)]
patternBindings (PType p _ _) t = patternBindings p t
patternBindings (Destructure _ p' _) vr
  | Nothing <- p'
  = []
  | Just p <- p'
  = patternBindings p (LuaRef (LuaIndex vr (LuaNumber 2)))
patternBindings (PRecord rs _) vr = concatMap (index vr) rs where
  index vr (var', pat) = patternBindings pat (LuaRef (LuaIndex vr (LuaString var')))
patternBindings (PTuple ps _) vr
  | [x] <- ps
  = patternBindings x vr
  | otherwise
  = concat $ zipWith3 innerBind ps (repeat vr) [1..] where
    innerBind p v = patternBindings p . LuaRef . LuaIndex v . LuaNumber . fromInteger

compileMatch :: Returner -> Expr Typed -> [(Pattern Typed, Expr Typed)] -> Gen Int [LuaStmt]
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
            , [LuaCallS (global "error")
                        [LuaString "Pattern matching failure in match expression"]])
  pure $ compileStmt (Just $ LuaLocal [x] . (:[])) ex
       ++ [ LuaIfElse (gen ps) ]

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
