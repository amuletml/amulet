{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, FlexibleContexts, TupleSections #-}

module Backend.Compile
  ( compileProgram
  ) where

import Control.Monad.Infer
import Control.Monad.State
import Control.Monad.Cont
import Control.Arrow
import Control.Lens hiding (uncons)

import Backend.Lua
import Core.Occurrence
import Core.Core
import Core.Types

import Syntax.Types
import Syntax (Var(..), Resolved)

import qualified Types.Wellformed as W

import qualified Data.Map.Strict as Map
import qualified Data.VarSet as VarSet
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Foldable
import Data.VarSet (IsVar(..))
import Data.Triple
import Data.Maybe (fromMaybe, maybeToList)
import Data.List (sortOn, partition, uncons)
import Data.Text (Text)
import Data.Graph
import Data.Char

type Returner = LuaExpr -> LuaStmt

data VarScope = VarScope { toLua :: Map.Map (Var Resolved) Text
                         , fromLua :: Map.Map Text (Var Resolved) }
  deriving (Show)

pushVar :: IsVar a => a -> VarScope -> (Text, VarScope)
pushVar v s = escapeVar (toVar v) where
  escapeVar (TgInternal t) = (t, s)
  escapeVar v@(TgName name _) =
    case Map.lookup v (toLua s) of
      Just _ -> error "Variable already declared"
      Nothing ->
        let Just (t, ts) = T.uncons name
            esc = if isAlpha t && T.all (\x -> x == '_' || isAlphaNum x) ts
                     then (if name `Set.member` luaKeywords then T.cons '_' else id) name
                     else T.concatMap escapeChar name
        in pushFirst Nothing esc

  escapeChar c = if isAlpha c
                    then T.singleton c
                    else fromMaybe T.empty (Map.lookup c chars)

  pushFirst :: Maybe Int -> Text -> (Text, VarScope)
  pushFirst prefix esc =
    let esc' = esc <> maybe T.empty (T.pack . show) prefix
    in case Map.lookup esc' (fromLua s) of
         Nothing -> ( esc'
                    , s { fromLua = Map.insert esc' (toVar v) (fromLua s)
                        , toLua = Map.insert (toVar v) esc' (toLua s) })
         Just _ -> pushFirst (Just (maybe 0 (+1) prefix)) esc

getVar :: IsVar a => a -> VarScope -> Text
getVar v s = case toVar v of
               TgInternal t -> t
               v@TgName{} -> fromMaybe (error ("Cannot find " ++ show v)) (Map.lookup v (toLua s))

alpha :: [Text]
alpha = map T.pack ([1..] >>= flip replicateM ['a'..'z'])

compileProgram :: forall a. Occurs a => Env -> [Stmt a] -> LuaStmt
compileProgram ev = LuaDo . flip evalState (VarScope mempty mempty) . compileProg where
  compileProg :: MonadState VarScope m => [Stmt a] -> m [LuaStmt]
  compileProg (Foreign n' t s:xs)
    | arity t > 1 = do
        n <- state (pushVar n')
        let ags = map LuaName $ take (arity t) alpha
            mkF (a:ag) bd = LuaFunction [a] [LuaReturn (mkF ag bd)]
            mkF [] bd = bd

        xs' <- compileProg xs

        pure $ LuaLocal [LuaName (T.cons '_' n)] [LuaBitE s]
             : LuaLocal [LuaName n]
                [mkF ags
                  (LuaCall (LuaRef (LuaName (T.cons '_' n)))
                    (map LuaRef ags))]
             : xs'

    | otherwise = do
        n <- state (pushVar n')
        (:) (LuaLocal [LuaName n] [LuaBitE s]) <$> compileProg xs

  compileProg (StmtLet vs:xs) = (++) <$> compileLet (unzip3 vs) <*> compileProg xs
  compileProg (Type _ cs:xs) = (++) <$> traverse compileConstructor cs <*> compileProg xs
  compileProg [] =
    let main = fmap (toVar . fst) . uncons
             . sortOn key
             . filter isMain
             . namesInScope
             . view values
        isMain (TgName x _) = x == "main"
        isMain _ = False
        key (TgName k _) = k
        key _ = undefined
     in case main ev of
       Just ref -> do
         ref' <- gets (getVar ref)
         let go 0 _ = Nothing
             go 1 it = Just (LuaCallS it [])
             go n it = do
               LuaCallS e _ <- go (n - 1) it
               pure $ LuaCallS (LuaCall e []) []
             ar = W.arity (ev ^. values . at ref . non undefined)
          in pure (maybeToList (go ar (LuaRef (LuaName ref'))))
       Nothing -> pure []

  compileConstructor :: (MonadState VarScope m, Occurs a) => (a, Type a) -> m LuaStmt
  compileConstructor (var, ty)
    | arity ty == 0 = do
        var' <- state (pushVar var)
        pure $ LuaLocal [LuaName var'] [LuaTable [(LuaNumber 1, LuaString var')]]
    | otherwise = do
        var' <- state (pushVar var)
        pure $ LuaLocal [LuaName var'] [LuaFunction
                                         [LuaName "x"]
                                        [LuaReturn (LuaTable [(LuaNumber 1, LuaString var')
                                                             , (LuaNumber 2, LuaRef (LuaName "x"))])]]

  compileLet :: (MonadState VarScope m, Occurs a) => ([a], [Type a], [Term a]) -> m [LuaStmt]
  compileLet (vs, _, es) = concat <$> traverse compileBind (stronglyConnComp (zipWith letDeps vs es))

  letDeps v e = ((v, e), toVar v, VarSet.toList (freeIn e))

  compileBind (AcyclicSCC (v, e)) = do
    v' <- state (pushVar v)
    s <- get
    pure [LuaLocal [LuaName v'] [iife (compileStmt s LuaReturn e)]]
  compileBind (CyclicSCC bs) = do
    bs' <- traverse (firstA (state . pushVar)) bs
    s <- get
    pure $ LuaLocal (map (LuaName . fst) bs') []
         : concatMap (\(v, e) -> compileStmt s (LuaAssign [LuaName v] . pure) e) bs'

type VarStack v = [(v, Term v, LuaExpr)]

type ExprContext v a = ContT [LuaStmt] (State (VarStack v, VarScope)) a

getStack :: MonadState (VarStack a, VarScope) m => m (VarStack a)
getStack = gets fst

putStack :: MonadState (VarStack a, VarScope) m => VarStack a -> m ()
putStack s = modify (\(_, b) -> (s, b))

pushScope :: IsVar a => MonadState (VarStack a, VarScope) m => a -> m Text
pushScope v = state (\(a, s) -> let (v', s') = pushVar v s in (v', (a, s')))

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
compileAtom' (Lam (TermArgument v _) e) = do
  (v', s) <- gets (pushVar v . snd) -- Note this doesn't modify the scope, only extends it
  pure (LuaFunction [LuaName v'] (compileStmt s LuaReturn e), Nothing)
compileAtom' (Lam TypeArgument{} e) = (,Nothing) <$> compileTerm e

compileAtom' (Ref v _) | isBinOp v
  = pure (LuaFunction
           [left]
           [LuaReturn (LuaFunction
                        [right]
                        [LuaReturn (LuaBinOp (LuaRef left) (remapOp' v) (LuaRef right))])],
           Nothing)
    where left  = LuaName "l"
          right = LuaName "r"

compileAtom' (Ref v _) = ContT $ \next -> do
  xs <- getStack
  case span ((/= toVar v) . toVar . fst3) xs of
             -- -- If the variable is not in the scope then skip
             (_, []) -> do v' <- gets (getVar v . snd)
                           next (LuaRef (LuaName v'), Nothing)
             -- Otherwise push all needed bindings and then continue
             (before, (_, e, e'):xs') -> do
               putStack xs'
               flip mkLets <$> traverse (first3A pushScope) before
                           <*> next (e', Just e)

compileAtom :: Occurs a => Atom a -> ExprContext a LuaExpr
compileAtom a = fst <$> compileAtom' a

flushStmt :: Occurs a => [LuaStmt] -> ExprContext a ()
flushStmt extra = ContT $ \next -> do
  xs <- getStack >>= traverse (first3A pushScope)
  putStack []
  stmts <- next ()
  pure (mkLets (extra ++ stmts) xs)

compileTerm :: forall a. Occurs a => Term a -> ExprContext a LuaExpr
compileTerm (Atom a) = compileAtom a

compileTerm (App f e) = do
  e' <- compileAtom e
  f' <- compileAtom' f
  pure $ case f' of
    (LuaCall _ [l], Just (App (Ref v _) _)) | isBinOp v -> LuaBinOp l (remapOp' v) e'
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
  pure (LuaRef new)

  where old = LuaName (T.pack "__o")
        new = LuaName (T.pack "__n")
        k = T.pack "k"
        v = T.pack "v"
        pairs = LuaName (T.pack "pairs")

        compileRow (f, _, e) es = (:es) . LuaAssign [LuaIndex (LuaRef new) (LuaString f)] . pure <$> compileAtom e

compileTerm (Let (One (x, _, e)) body)
  | usedWhen x == Once && not (isMultiMatch e) = do
      -- If we've got a let binding which is only used once then push it onto the stack
      e' <- compileTerm e
      modify (first ((x, e, e'):))
      compileTerm body

  | usedWhen x == Dead && not (isMultiMatch e) = do
      e' <- compileTerm e
      flushStmt (asStmt e')
      compileTerm body

  | not (isMultiMatch e) = do
      -- If we've got a let binding which doesn't branch, then we can emit it as a normal
      -- local.
      x' <- pushScope x
      e' <- compileTerm e
      flushStmt [ LuaLocal [LuaName x'] [e'] ]
      compileTerm body
  | otherwise = do
    -- TODO: Handle match generation here, so we don't do kuldges with scope extension or anything
    x' <- pushScope x
    flushStmt [ LuaLocal [LuaName x'] [] ]
    s <- gets snd
    flushStmt (compileStmt s (LuaAssign [LuaName x'] . pure) e)
    compileTerm body

  where asStmt (LuaTable fs) = concatMap (asStmt . snd) fs
        asStmt (LuaBinOp a _ b) = asStmt a ++ asStmt b
        asStmt (LuaCall f e) = [ LuaCallS f e ]
        asStmt _ = []

compileTerm (Let (Many bs) body) = do
  -- Otherwise predeclare all variables and emit the bindings
  bs' <- traverse ((LuaName<$>) . pushScope . fst3) bs
  flushStmt [ LuaLocal bs' [] ]
  traverse_ compileLet bs
  compileTerm body
  where compileLet (v, _, e) = do
          s <- gets snd
          flushStmt (compileStmt s (LuaAssign [LuaName (getVar v s)] . pure) e)

compileTerm m@(Match test [Arm { armPtrn = p, armBody = body, armVars = vs}])
  | all (\(x, _) -> usedWhen x == Once || usedWhen x == Dead) vs
  = do
      test' <- compileAtom test
      -- Just push the bindings onto the stack
      modify (first (withMatch (patternBindings p test')++))

      compileTerm body
  | otherwise
  = do
      flushStmt []
      test' <- compileAtom test

      let (once, multi) = partition ((==Once) . usedWhen . fst) (patternBindings p test')

      -- Declare any variable used multiple times (or hoisted into a lambda)
      multi' <- traverse (firstA ((LuaName<$>) . pushScope)) multi
      flushStmt [uncurry LuaLocal (unzip multi')]

      -- Push any variable used once onto the stack
      modify (first (withMatch once++))

      compileTerm body

  where withMatch = map (\(v, b) -> (v,m,b))

compileTerm m@(Match test branches) = do
  flushStmt []
  test' <- compileAtom test
  compileMatch m test' branches

compileStmt :: Occurs a => VarScope -> Returner -> Term a -> [LuaStmt]
compileStmt s r term = evalState (runContT (compileTerm term) finish) ([], s) where
  finish x = mkLets [r x] <$> (traverse (first3A pushScope) =<< getStack)

iife :: [LuaStmt] -> LuaExpr
iife [LuaReturn v] = v
iife b = LuaCall (LuaFunction [] b) []

mkLets :: [LuaStmt] -> [(Text, b, LuaExpr)] -> [LuaStmt]
mkLets = foldl (\stmts (v, _, b) -> LuaLocal [LuaName v] [b] : stmts)

foldAnd :: [LuaExpr] -> LuaExpr
foldAnd = foldl1 k where
  k l r
    | r == LuaTrue = l
    | l == LuaTrue = r
    | r == LuaFalse || l == LuaFalse = LuaFalse
    | otherwise = LuaBinOp l "and" r

patternTest :: forall a. Occurs a => VarScope -> Pattern a -> LuaExpr ->  LuaExpr
patternTest _ (Capture _ _) _      = LuaTrue
patternTest _ (PatLit RecNil) _    = LuaTrue
patternTest _ (PatLit l)  vr       = LuaBinOp (compileLit l) "==" vr
patternTest s (PatExtend p rs) vr  = foldAnd (patternTest s p vr : map test rs) where
  test (var', pat) = patternTest s pat (LuaRef (LuaIndex vr (LuaString var')))
patternTest s (Constr con) vr      = foldAnd [tag s con vr]
patternTest s (Destr con p) vr     = foldAnd [tag s con vr, patternTest s p (LuaRef (LuaIndex vr (LuaNumber 2)))]

tag :: Occurs a => VarScope -> a -> LuaExpr -> LuaExpr
tag scp con vr = LuaBinOp (LuaRef (LuaIndex vr (LuaNumber 1))) "==" (LuaString (getVar con scp))

patternBindings :: Occurs a => Pattern a -> LuaExpr -> [(a, LuaExpr)]
patternBindings (PatLit _) _     = []
patternBindings (Capture n _) v
  | doesItOccur n = [(n, v)]
  | otherwise = []
patternBindings (Constr _) _     = []
patternBindings (Destr _ p) vr   = patternBindings p (LuaRef (LuaIndex vr (LuaNumber 2)))
patternBindings (PatExtend p rs) vr = patternBindings p vr ++ concatMap (index vr) rs where
  index vr (var', pat) = patternBindings pat (LuaRef (LuaIndex vr (LuaString var')))

compileMatch :: Occurs a => Term a -> LuaExpr -> [Arm a] -> ExprContext a LuaExpr
compileMatch match test ps = ContT $ \next ->  gets (pure . genIf next . snd)
  where genBinding next s Arm { armPtrn = p, armBody = c } =
          ( patternTest s p test
          , let (once, multi) = partition ((==Once) . usedWhen . fst) (patternBindings p test)
                (s', multi') = foldl (\(s, vs) (v, e) -> let (v', s') = pushVar v s in (s', (LuaName v', e): vs)) (s, []) multi
            in (case multi' of
                  [] -> []
                  _ -> [uncurry LuaLocal (unzip multi')])
               ++ evalState (runContT (compileTerm c) next) (withMatch once, s') )

        genIf next s =
          case ps of
            (ifs@Arm { armPtrn = PatLit LitTrue } :
             els@Arm { armPtrn = PatLit LitFalse } :_) ->
              LuaIfElse [ (test, snd (genBinding next s ifs))
                        , (LuaTrue, snd (genBinding next s els)) ]

            _ -> LuaIfElse $ map (genBinding next s) ps

        withMatch = map (\(v, b) -> (v, match, b))

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

chars :: Map.Map Char Text
chars = Map.fromList
  [ (':', "_colon")
  , ('!', "_bang")
  , ('#', "_pound")
  , ('$', "_dollar")
  , ('%', "_percent")
  , ('&', "_amp")
  , ('*', "_star")
  , ('+', "_plus")
  , ('.', "_dot")
  , ('/', "_divide")
  , ('<', "_less")
  , ('=', "_equals")
  , ('>', "_greater")
  , ('?', "_ask")
  , ('@', "_at")
  , ('\\', "_slash")
  , ('^', "_hat")
  , ('|', "_bar")
  , ('-', "_minus")
  , ('~', "_tilde")
  , ('[', "_lbrack")
  , (']', "_rbrack ") ]

isBinOp :: Occurs a => a -> Bool
isBinOp x | TgInternal v <- toVar x = Map.member v ops
isBinOp _ = False

remapOp :: Text -> Text
remapOp x = fromMaybe x (Map.lookup x ops)

remapOp' :: IsVar a => a -> Text
remapOp' v = let TgInternal v' = toVar v in remapOp v'

isMultiMatch :: Term a -> Bool
isMultiMatch (Match _ (_:_:_)) = True
isMultiMatch _ = False

luaKeywords :: Set.Set Text
luaKeywords = Set.fromList [ "and", "break", "do", "else", "elseif", "end", "false", "for", "function",  "if", "in", "local", "nil", "not", "or", "repeat", "return", "then", "true", "until", "while" ]
