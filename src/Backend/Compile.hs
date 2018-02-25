{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, FlexibleContexts #-}
module Backend.Compile
  ( compileProgram
  , compileLet
  , compileExpr
  ) where

import Control.Monad.Gen
import Control.Monad

import Control.Monad.Infer

import Backend.Lua
import Core.Occurrence
import Core.Core
import Core.Types
import Syntax

import qualified Types.Wellformed as W

import Data.Semigroup ((<>))

import qualified Data.Map.Strict as Map
import qualified Data.VarSet as VarSet
import Data.VarSet (IsVar(..))

import qualified Data.Text as T
import Data.Text (Text)
import Data.List (sortOn, partition, uncons)
import Data.Maybe (fromMaybe, maybeToList)

type Returner = Maybe (LuaExpr -> LuaStmt)

alpha :: [Text]
alpha = map T.pack ([1..] >>= flip replicateM ['a'..'z'])

compileProgram :: forall a. Occurs a => Env -> [CoStmt a] -> LuaStmt
compileProgram ev = LuaDo . (extendDef:) . compileProg where
  compileProg :: [CoStmt a] -> [LuaStmt]
  compileProg (CosForeign n' t s:xs)
    | not (doesItOccur n') = compileProg xs
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

  compileProg (CosLet vs:xs) = compileLet (unzip3 vs) ++ compileProg xs
  compileProg (CosType _ cs:xs) = map compileConstructor cs ++ compileProg xs
  compileProg [] =
    let main = fmap (toVar . fst) . uncons
             . sortOn key
             . filter isMain
             . Map.keys
             . _values
        isMain (TgName x _) = x == "main"
        isMain _ = False
        key (TgName k _) = k
        key _ = undefined
     in case main ev of
       Just ref ->
         let go 0 _ = Nothing
             go 1 it = Just (LuaCallS it [])
             go n it = do
               LuaCallS e _ <- go (n - 1) it
               pure $ LuaCallS (LuaCall e []) []
             ar = W.arity (_values ev Map.! ref)
          in maybeToList (go ar (LuaRef (LuaName (getTaggedName ref))))
       Nothing -> []

compileConstructor :: Occurs a => (a, CoType a) -> LuaStmt
compileConstructor (var, ty) | arity ty == 0
  = LuaLocal [lowerName var] [LuaTable [(LuaNumber 1, LuaString cn)]] where
    cn = getName var
compileConstructor (var, _) -- non-unit constructors, hard
  = LuaLocal [lowerName var] [vl] where
    vl = LuaFunction [LuaName "x"] [LuaReturn (LuaTable [(LuaNumber 1, LuaString cn), (LuaNumber 2, LuaRef (LuaName "x"))])]
    cn = getName var

compileAtom :: Occurs a => CoAtom a -> LuaExpr
compileAtom (CoaRef v _) | isBinOp v
  = LuaFunction [left] [LuaReturn (LuaFunction
                                    [right]
                                    [LuaReturn (LuaBinOp (LuaRef left) (remapOp (getTaggedName (toVar v))) (LuaRef right))])]
    where left  = LuaName "l"
          right = LuaName "r"

compileAtom (CoaRef v _) = LuaRef (lowerName v)
compileAtom (CoaLam Small (v, _) e) = LuaFunction [lowerName v] (compileStmt (Just LuaReturn) e)
compileAtom (CoaLam Big _ e) = compileExpr e

compileAtom (CoaLit (ColInt x))   = LuaNumber (fromInteger x)
compileAtom (CoaLit (ColStr str)) = LuaString str
compileAtom (CoaLit ColTrue)      = LuaTrue
compileAtom (CoaLit ColFalse)     = LuaFalse
compileAtom (CoaLit ColUnit)      = LuaNil -- evil!
compileAtom (CoaLit ColRecNil)    = LuaTable []

compileExpr :: Occurs a => CoTerm a -> LuaExpr
compileExpr (CotAtom a) = compileAtom a
compileExpr (CotApp f e) = LuaCall (compileAtom f) [compileAtom e]
compileExpr (CotTyApp f _) = compileAtom f
compileExpr (CotExtend (CoaLit ColRecNil) fs)
  = LuaTable (map (\(f, _, e) -> (LuaString f, compileAtom e)) fs)

compileExpr s@CotExtend{} = compileIife s
compileExpr s@CotLet{}    = compileIife s
compileExpr s@CotMatch{}  = compileIife s

global :: String -> LuaExpr
global x = LuaRef (LuaIndex (LuaRef (LuaName "_G")) (LuaString (T.pack x)))

compileStmt :: Occurs a => Returner -> CoTerm a -> [LuaStmt]
compileStmt r (CotAtom a) = pureReturn r $ compileAtom a
compileStmt r (CotLet k c) = compileLet (unzip3 k) ++ compileStmt r c
compileStmt r (CotMatch s ps) = runGen (compileMatch r s ps)

compileStmt Nothing e@CotApp{} = case compileExpr e of
                                     LuaCall f a -> [LuaCallS f a]
                                     expr -> [LuaLocal [LuaName "_"] [expr]]
compileStmt (Just r) e@CotApp{} = [r (compileExpr e)]
compileStmt r (CotTyApp f _) = pureReturn r $ compileAtom f

compileStmt r e@(CotExtend (CoaLit ColRecNil) _) = dirtyReturn r (compileExpr e) -- TODO: Flatten if r is nothing?
compileStmt r (CotExtend tbl exs) = [ LuaLocal [old, new] [compileAtom tbl, LuaTable []]
                                    , LuaFor [k, v] [LuaCall (LuaRef pairs) [LuaRef old]]
                                      [LuaAssign [LuaIndex (LuaRef new) (LuaRef (LuaName k))] [LuaRef (LuaName v)]] ] ++
                                    map (\(f, _, e) -> LuaAssign [LuaIndex (LuaRef new) (LuaString f)] [compileAtom e]) exs ++
                                    pureReturn r (LuaRef new)
  where old = LuaName (T.pack "__o")
        new = LuaName (T.pack "__n")
        k = T.pack "k"
        v = T.pack "v"
        pairs = LuaName (T.pack "pairs")

lowerName :: Occurs a => a -> LuaVar
lowerName = LuaName . getTaggedName . toVar

lowerKey :: Occurs a => a -> LuaExpr
lowerKey = LuaString . getTaggedName . toVar

getName :: Occurs a => a -> Text
getName = getTaggedName . toVar

getTaggedName :: Var Resolved -> Text
getTaggedName (TgName t i) = t <> T.pack (show i)
getTaggedName (TgInternal t) = t

iife :: [LuaStmt] -> LuaExpr
iife b = LuaCall (LuaFunction [] b) []

compileIife :: Occurs a => CoTerm a -> LuaExpr
compileIife = iife . compileStmt (Just LuaReturn)

compileLet :: forall a. Occurs a => ([a], [CoType a], [CoTerm a]) -> [LuaStmt]
compileLet (vs, _, es) = locals recs (assigns nonrecs) where
  binds = zip vs es
  (nonrecs, recs) = partition (uncurry recursive) binds

  locals :: [(a, CoTerm a)] -> [LuaStmt] -> [LuaStmt]
  locals xs =
    let (v, t) = unzip xs
        bind v e
          | doesItOccur v = [LuaLocal [lowerName v] [compileExpr e]]
          | otherwise = compileStmt Nothing e
     in case v of
       [] -> id
       _ -> (++) (concat (zipWith bind v t))

  assigns :: [(a, CoTerm a)] -> [LuaStmt]
  assigns xs = case xs of
    [] -> []
    xs -> LuaLocal (map (lowerName . fst) xs) []:map one xs
  one (v, t) = LuaAssign [lowerName v] [compileExpr t]

  recursive v (CotAtom term@CoaLam{}) = toVar v `VarSet.member` freeInAtom term
  recursive _ _ = False

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

patternTest :: forall a. Occurs a => CoPattern a -> LuaExpr ->  LuaExpr
patternTest (CopCapture _ _) _   = LuaTrue
patternTest (CopLit ColRecNil) _ = LuaTrue
patternTest (CopLit l)     vr    = LuaBinOp (compileAtom (CoaLit l :: CoAtom a)) "==" vr
patternTest (CopExtend p rs) vr  = foldAnd (patternTest p vr : map test rs) where
  test (var', pat) = patternTest pat (LuaRef (LuaIndex vr (LuaString var')))
patternTest (CopConstr con) vr   = foldAnd [tag con vr]
patternTest (CopDestr con p) vr  = foldAnd [tag con vr, patternTest p (LuaRef (LuaIndex vr (LuaNumber 2)))]

tag :: Occurs a => a -> LuaExpr -> LuaExpr
tag con vr = LuaBinOp (LuaRef (LuaIndex vr (LuaNumber 1))) "==" (lowerKey con)

patternBindings :: Occurs a => CoPattern a -> LuaExpr -> [(LuaVar, LuaExpr)]
patternBindings (CopLit _) _        = []
patternBindings (CopCapture n _) v
  | doesItOccur n = [(lowerName n, v)]
  | otherwise = []
patternBindings (CopConstr _) _     = []
patternBindings (CopDestr _ p) vr   = patternBindings p (LuaRef (LuaIndex vr (LuaNumber 2)))
patternBindings (CopExtend p rs) vr = patternBindings p vr ++ concatMap (index vr) rs where
  index vr (var', pat) = patternBindings pat (LuaRef (LuaIndex vr (LuaString var')))

compileMatch :: Occurs a => Returner -> CoAtom a -> [(CoPattern a, CoType a, CoTerm a)] -> Gen Int [LuaStmt]
compileMatch r ex ps = pure $ genIf (compileAtom ex) ps


  where genBinding x (p, _, c) = ( patternTest p x
                                 , (case patternBindings p x of
                                      [] -> []
                                      xs -> [uncurry LuaLocal (unzip xs)])
                                   ++ compileStmt r c)
        genIf x ps = [ LuaIfElse (map (genBinding x) ps) ]

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

ops :: Map.Map Text Text
ops = Map.fromList [ ("+", "+")
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

isBinOp :: Occurs a => a -> Bool
isBinOp x | TgInternal v <- toVar x = Map.member v ops
isBinOp _ = False

remapOp :: Text -> Text
remapOp x = fromMaybe x (Map.lookup x ops)
