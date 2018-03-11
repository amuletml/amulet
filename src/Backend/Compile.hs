{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, FlexibleContexts, TupleSections #-}
{-# LANGUAGE DeriveFunctor #-}

module Backend.Compile
  ( compileProgram
  ) where

import Control.Monad.Infer

import Backend.Lua
import Core.Occurrence
import Core.Core
import Core.Types
import Syntax (Var(..), Resolved)

import qualified Types.Wellformed as W

import Data.Semigroup ((<>))

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Foldable
import Data.VarSet (IsVar(..))
import Data.Triple
import Data.Maybe (fromMaybe, maybeToList)
import Data.List (sortOn, partition, uncons)
import Data.Text (Text)

type Returner = LuaExpr -> LuaStmt

alpha :: [Text]
alpha = map T.pack ([1..] >>= flip replicateM ['a'..'z'])

compileProgram :: forall a. Occurs a => Env -> [Stmt a] -> LuaStmt
compileProgram ev = LuaDo . compileProg where
  compileProg :: [Stmt a] -> [LuaStmt]
  compileProg (Foreign n' t s:xs)
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

  compileProg (StmtLet vs:xs) = compileLet (unzip3 vs) ++ compileProg xs
  compileProg (Type _ cs:xs) = map compileConstructor cs ++ compileProg xs
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

compileConstructor :: Occurs a => (a, Type a) -> LuaStmt
compileConstructor (var, ty) | arity ty == 0
  = LuaLocal [lowerName var] [LuaTable [(LuaNumber 1, LuaString cn)]] where
    cn = getName var
compileConstructor (var, _) -- non-unit constructors, hard
  = LuaLocal [lowerName var] [vl] where
    vl = LuaFunction [LuaName "x"] [LuaReturn (LuaTable [(LuaNumber 1, LuaString cn), (LuaNumber 2, LuaRef (LuaName "x"))])]
    cn = getName var

type VarStack v = [(v, Term v, LuaExpr)]

newtype ExprContext v a = EC {
  unEC :: VarStack v
       -> (VarStack v -> a -> ([LuaStmt], VarStack v))
       -> ([LuaStmt], VarStack v)
  } deriving (Functor)

instance Applicative (ExprContext v) where
  pure a = EC $ \xs next -> next xs a
  (<*>) = ap

instance Monad (ExprContext v) where
  x >>= f = EC $ \xs next -> unEC x xs (\xs' y -> unEC (f y) xs' next)

compileLit :: Literal -> LuaExpr
compileLit (Int x)   = LuaNumber (fromInteger x)
compileLit (Float x) = LuaNumber x
compileLit (Str str) = LuaString str
compileLit LitTrue   = LuaTrue
compileLit LitFalse  = LuaFalse
compileLit Unit      = LuaNil -- evil!
compileLit RecNil    = LuaTable []

compileAtom' :: Occurs a => Atom a -> ExprContext a (LuaExpr, Maybe (Term a))
compileAtom' (Lit l) = pure (compileLit l, Nothing)
compileAtom' (Lam (TermArgument v _) e) = pure (LuaFunction [lowerName v] (compileStmt LuaReturn e), Nothing)
compileAtom' (Lam TypeArgument{} e) = (,Nothing) <$> compileTerm e

compileAtom' (Ref v _) | isBinOp v
  = pure (LuaFunction
           [left]
           [LuaReturn (LuaFunction
                        [right]
                        [LuaReturn (LuaBinOp (LuaRef left) (remapOp (getName v)) (LuaRef right))])],
           Nothing)
    where left  = LuaName "l"
          right = LuaName "r"

compileAtom' (Ref v _) = EC
  $ \xs next -> case span ((/= toVar v) . toVar . fst3) xs of
             -- -- If the variable is not in the scope then skip
             (_, []) -> next xs (LuaRef (lowerName v), Nothing)
             (before, (_, e, e'):xs') -> let (stmts, xs'') = next xs' (e', Just e)
                                         in (mkLets stmts before, xs'')

compileAtom :: Occurs a => Atom a -> ExprContext a LuaExpr
compileAtom a = fst <$> compileAtom' a

flushStmt :: Occurs a => [LuaStmt] -> b -> ExprContext a b
flushStmt extra e = EC $ \xs next -> let (stmts, xs') = next [] e
                                     in (mkLets (extra ++ stmts) xs, xs')

compileTerm :: Occurs a => Term a -> ExprContext a LuaExpr
compileTerm (Atom a) = compileAtom a

compileTerm (App f e) = do
  e' <- compileAtom e
  f' <- compileAtom' f
  pure $ case f' of
    (LuaCall _ [l], Just (App (Ref v _) _)) | isBinOp v -> LuaBinOp l (remapOp (getName v)) e'
    (fl', _) -> LuaCall fl' [e']

compileTerm (TyApp f _) = compileAtom f
compileTerm (Cast f _) = compileAtom f

compileTerm (Extend (Lit RecNil) fs) = do
  fs' <- foldrM compileRow [] fs
  pure (LuaTable fs')
  where compileRow (f, _, e) es = (:es) . (LuaString f,) <$> compileAtom e
compileTerm (Extend tbl exs) = do
  exs' <- foldrM compileRow [] exs
  tbl' <- compileAtom tbl

  flushStmt ([ LuaLocal [old, new] [tbl', LuaTable []]
             , LuaFor [k, v] [LuaCall (LuaRef pairs) [LuaRef old]]
               [LuaAssign [LuaIndex (LuaRef new) (LuaRef (LuaName k))] [LuaRef (LuaName v)]] ] ++ exs')
            (LuaRef new)

  where old = LuaName (T.pack "__o")
        new = LuaName (T.pack "__n")
        k = T.pack "k"
        v = T.pack "v"
        pairs = LuaName (T.pack "pairs")

        compileRow (f, _, e) es = (:es) . LuaAssign [LuaIndex (LuaRef new) (LuaString f)] . pure <$> compileAtom e

compileTerm (Let (One (x, _, e)) body)
  | usedWhen x == 1 && not (isMultiMatch e) && not (occursInTerm x e) = do
      -- If we've got a let binding which is only used once then push it onto the stack
      e' <- compileTerm e
      EC $ \xs next -> next ((x, e, e'):xs) ()
      compileTerm body

  | usedWhen x == 0 && not (isMultiMatch e) = do
      e' <- compileTerm e
      flushStmt (asStmt e') ()
      compileTerm body

  | not (isMultiMatch e) = do
      -- If we've got a let binding which doesn't branch, then we can emit it as a normal
      -- local Technically this isn't correct (as recursive functions won't work), but the
      -- pretty printer sorts this out.
      e' <- compileTerm e
      flushStmt [ LuaLocal [lowerName x] [e'] ] ()
      compileTerm body
  | otherwise = do
    flushStmt [ LuaLocal [lowerName x] [] ] ()
    flushStmt (compileStmt (LuaAssign [lowerName x] . pure) e) ()
    compileTerm body

  where asStmt (LuaTable fs) = concatMap (asStmt . snd) fs
        asStmt (LuaBinOp a _ b) = asStmt a ++ asStmt b
        asStmt (LuaCall f e) = [ LuaCallS f e ]
        asStmt _ = []

compileTerm (Let (Many bs) body) = do
  -- Otherwise predeclare all variables and emit the bindings
  flushStmt [ LuaLocal (map (lowerName . fst3) bs) [] ] ()
  traverse_ compileLet bs
  compileTerm body
  where compileLet (v, _, e) = flushStmt (compileStmt (LuaAssign [lowerName v] . pure) e) ()

compileTerm m@(Match test [(p, _, body)])
  | [v] <- filter doesItOccur (patternVarsA p)
  , usedWhen v == 1
  = do
      test' <- compileAtom test
      let [(_, bind)] = patternBindings p test'
      EC $ \xs next -> next ((v, m, bind):xs) ()
      compileTerm body
  | otherwise
  = do
      flushStmt [] ()
      test' <- compileAtom test
      flushStmt [uncurry LuaLocal (unzip (patternBindings p test'))] ()
      compileTerm body

compileTerm (Match test branches) = do
  flushStmt [] ()
  test' <- compileAtom test
  compileMatch test' branches

compileStmt :: Occurs a => Returner -> Term a -> [LuaStmt]
compileStmt r term = fst (unEC (compileTerm term) [] (\xs x -> (mkLets [r x] xs, [])))
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
iife [LuaReturn v] = v
iife b = LuaCall (LuaFunction [] b) []

compileLet :: forall a. Occurs a => ([a], [Type a], [Term a]) -> [LuaStmt]
compileLet (vs, _, es) = locals recs (assigns nonrecs) where
  binds = zip vs es
  (nonrecs, recs) = partition (uncurry recursive) binds

  locals :: [(a, Term a)] -> [LuaStmt] -> [LuaStmt]
  locals xs =
    let (v, t) = unzip xs
        bind v e = [LuaLocal [lowerName v] [iife (compileStmt LuaReturn e)]]
     in case v of
       [] -> id
       _ -> (++) (concat (zipWith bind v t))

  assigns :: [(a, Term a)] -> [LuaStmt]
  assigns xs = case xs of
    [] -> []
    xs -> LuaLocal (map (lowerName . fst) xs) []:concatMap one xs
  one (v, t) = compileStmt (LuaAssign [lowerName v] . pure) t

  recursive v (Atom term@Lam{}) = occursInAtom v term
  recursive _ _ = False

mkLets :: Occurs a => [LuaStmt] -> [(a, b, LuaExpr)] -> [LuaStmt]
mkLets = foldl (\stmts (v, _, b) -> LuaLocal [lowerName v] [b] : stmts)

foldAnd :: [LuaExpr] -> LuaExpr
foldAnd = foldl1 k where
  k l r
    | r == LuaTrue = l
    | l == LuaTrue = r
    | r == LuaFalse || l == LuaFalse = LuaFalse
    | otherwise = LuaBinOp l "and" r

patternTest :: forall a. Occurs a => Pattern a -> LuaExpr ->  LuaExpr
patternTest (Capture _ _) _      = LuaTrue
patternTest (PatLit RecNil) _    = LuaTrue
patternTest (PatLit l)  vr       = LuaBinOp (compileLit l) "==" vr
patternTest (PatExtend p rs) vr  = foldAnd (patternTest p vr : map test rs) where
  test (var', pat) = patternTest pat (LuaRef (LuaIndex vr (LuaString var')))
patternTest (Constr con) vr      = foldAnd [tag con vr]
patternTest (Destr con p) vr     = foldAnd [tag con vr, patternTest p (LuaRef (LuaIndex vr (LuaNumber 2)))]

tag :: Occurs a => a -> LuaExpr -> LuaExpr
tag con vr = LuaBinOp (LuaRef (LuaIndex vr (LuaNumber 1))) "==" (lowerKey con)

patternBindings :: Occurs a => Pattern a -> LuaExpr -> [(LuaVar, LuaExpr)]
patternBindings (PatLit _) _     = []
patternBindings (Capture n _) v
  | doesItOccur n = [(lowerName n, v)]
  | otherwise = []
patternBindings (Constr _) _     = []
patternBindings (Destr _ p) vr   = patternBindings p (LuaRef (LuaIndex vr (LuaNumber 2)))
patternBindings (PatExtend p rs) vr = patternBindings p vr ++ concatMap (index vr) rs where
  index vr (var', pat) = patternBindings pat (LuaRef (LuaIndex vr (LuaString var')))

compileMatch :: Occurs a => LuaExpr -> [(Pattern a, Type a, Term a)] -> ExprContext a LuaExpr
compileMatch ex ps = EC $ \xs next -> (genIf next, xs)
  where genBinding next x (p, _, c) = ( patternTest p x
                                      , (case patternBindings p x of
                                           [] -> []
                                           xs -> [uncurry LuaLocal (unzip xs)])
                                        ++ fst (unEC (compileTerm c) [] next))
        genIf next = case map (genBinding next ex) ps of
                       [(LuaTrue, e)] -> e
                       xs -> [LuaIfElse xs]

ops :: Map.Map Text Text
ops = Map.fromList
  [ ("+", "+"), ("+.", "+")
  , ("-", "-"), ("-.", "-")
  , ("*", "*"), ("*.", "*")
  , ("/", "/"), ("/.", "/")
  , ("**", "^"), ("**.", "^")
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

isMultiMatch :: Term a -> Bool
isMultiMatch (Match _ (_:_:_)) = True
isMultiMatch _ = False
