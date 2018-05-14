{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, FlexibleContexts, TupleSections, TemplateHaskell #-}

module Backend.Lua.Emit
  ( emitProgram
  , escapeScope, remapOp, ops
  ) where

import Control.Monad.State
import Control.Monad.Cont
import Control.Lens hiding (uncons)

import qualified Data.VarMap as VarMap
import qualified Data.VarSet as VarSet
import qualified Data.Text as T
import Data.Foldable
import Data.Triple
import Data.Maybe (fromMaybe, maybeToList)
import Data.List (sortOn, partition, uncons)
import Data.Text (Text)
import Data.Graph

import Backend.Lua.Syntax
import Backend.Escape

import Core.Occurrence
import Core.Builtin
import Core.Types
import Core.Core
import Core.Var

import Syntax.Types
import Syntax (Var(..))

import qualified Types.Wellformed as W

type Returner = LuaExpr -> LuaStmt

data VarEntry v = VarEntry { vVar  :: v
                           , vExpr :: LuaExpr }
  deriving (Show)

data EmitState v = EmitState { _eStack  :: [VarEntry v]
                             , _eEscape :: EscapeScope }
  deriving (Show)

makeLenses ''EmitState

type ExprContext v a = ContT [LuaStmt] (State (EmitState v)) a

emitProgram :: forall a. Occurs a => Env -> [Stmt a] -> [LuaStmt]
emitProgram ev = flip evalState escapeScope . emitProg where
  emitProg :: MonadState EscapeScope m => [Stmt a] -> m [LuaStmt]
  emitProg (Foreign n' t s:xs)
    | arity t > 1 = do
        n <- state (pushVar n')
        let ags = map LuaName $ take (arity t) alpha
            mkF (a:ag) bd = LuaFunction [a] [LuaReturn (mkF ag bd)]
            mkF [] bd = bd

        xs' <- emitProg xs

        pure $ LuaLocal [LuaName (T.cons '_' n)] [LuaBitE s]
             : LuaLocal [LuaName n]
                [mkF ags
                  (LuaCall (LuaRef (LuaName (T.cons '_' n)))
                    (map LuaRef ags))]
             : xs'

    | otherwise = do
        n <- state (pushVar n')
        (:) (LuaLocal [LuaName n] [LuaBitE s]) <$> emitProg xs

  emitProg (StmtLet vs:xs) = (++) <$> emitLet (unzip3 vs) <*> emitProg xs
  emitProg (Type _ cs:xs) = (++) <$> traverse emitConstructor cs <*> emitProg xs
  emitProg [] =
    let main = fmap fst . uncons
             . sortOn key
             . filter isMain
             . namesInScope
             . view values
        isMain (TgName x _) = x == "main"
        isMain _ = False
        key (TgName k _) = k
        key _ = undefined
     in case main ev of
       Just ref@(TgName n i) -> do
         ref' <- gets (getVar (CoVar i n ValueVar))
         let go 0 _ = Nothing
             go 1 it = Just (LuaCallS it [])
             go n it = do
               LuaCallS e _ <- go (n - 1) it
               pure $ LuaCallS (LuaCall e []) []
             ar = W.arity (ev ^. values . at ref . non undefined)
          in pure (maybeToList (go ar (LuaRef (LuaName ref'))))
       _ -> pure []

  emitConstructor :: (MonadState EscapeScope m, Occurs a) => (a, Type a) -> m LuaStmt
  emitConstructor (var, ty)
    | arity ty == 0 = do
        var' <- state (pushVar var)
        pure $ LuaLocal [LuaName var'] [LuaTable [(LuaNumber 1, LuaString var')]]
    | otherwise = do
        var' <- state (pushVar var)
        pure $ LuaLocal [LuaName var'] [LuaFunction
                                         [LuaName "x"]
                                        [LuaReturn (LuaTable [(LuaNumber 1, LuaString var')
                                                             , (LuaNumber 2, LuaRef (LuaName "x"))])]]

  emitLet :: (MonadState EscapeScope m, Occurs a) => ([a], [Type a], [Term a]) -> m [LuaStmt]
  emitLet (vs, _, es) = concat <$> traverse emitBind (stronglyConnComp (zipWith letDeps vs es))

  letDeps v e = ((v, e), toVar v, VarSet.toList (freeIn e))

  emitBind (AcyclicSCC (v, e)) = do
    v' <- state (pushVar v)
    s <- get
    pure [LuaLocal [LuaName v'] [iife (emitStmt s LuaReturn e)]]
  emitBind (CyclicSCC bs) = do
    bs' <- traverse (firstA (state . pushVar)) bs
    s <- get
    pure $ LuaLocal (map (LuaName . fst) bs') []
         : concatMap (\(v, e) -> emitStmt s (LuaAssign [LuaName v] . pure) e) bs'

  alpha :: [Text]
  alpha = map T.pack ([1..] >>= flip replicateM ['a'..'z'])


pushScope :: IsVar a => MonadState (EmitState a) m => a -> m Text
pushScope v = state (\s -> let (v', s') = pushVar v (s ^. eEscape) in (v', set eEscape s' s))

pushEntry :: IsVar a => MonadState (EmitState a) m => VarEntry a -> m (Text, LuaExpr)
pushEntry v = (,vExpr v) <$> pushScope (vVar v)

emitLit :: Literal -> LuaExpr
emitLit (Int x)   = LuaNumber (fromInteger x)
emitLit (Float x) = LuaNumber x
emitLit (Str str) = LuaString str
emitLit LitTrue   = LuaTrue
emitLit LitFalse  = LuaFalse
emitLit Unit      = LuaNil -- evil!
emitLit RecNil    = LuaTable []

emitAtom :: Occurs a => Atom a -> ExprContext a LuaExpr
emitAtom (Lit l) = pure (emitLit l)
emitAtom (Lam (TermArgument v _) e) = do
  (v', s) <- uses eEscape (pushVar v) -- Note this doesn't modify the scope, only extends it
  pure (LuaFunction [LuaName v'] (emitStmt s LuaReturn e))
emitAtom (Lam TypeArgument{} e) = emitTerm e
emitAtom (Ref v _) = ContT $ \next -> do
  xs <- use eStack
  case span ((/= toVar v) . toVar . vVar) xs of
             -- -- If the variable is not in the scope then skip
             (_, []) -> do v' <- uses eEscape (getVar v)
                           next (LuaRef (LuaName v'))
             -- Otherwise push all needed bindings and then continue
             (before, VarEntry { vExpr = e }:xs') -> do
               assign eStack xs'
               flip mkLets <$> traverse pushEntry before
                           <*> next e

flushStmt :: Occurs a => [LuaStmt] -> ExprContext a ()
flushStmt extra = ContT $ \next -> do
  xs <- use eStack >>= traverse pushEntry
  assign eStack []
  stmts <- next ()
  pure (mkLets (extra ++ stmts) xs)

emitTerm :: forall a. Occurs a => Term a -> ExprContext a LuaExpr
emitTerm (Atom a) = emitAtom a

emitTerm (App f e) = do
  e' <- emitAtom e
  f' <- emitAtom f

  esc <- use eEscape
  pure $ case f' of
    -- Attempt to reduce applications of binary functions to the operators
    -- themselves.
    LuaCall (LuaRef (LuaName op)) [l]
      | Just op' <- getEscaped op esc, isBinOp (op' :: CoVar)
      -> LuaBinOp l (remapOp op') e'

    _ -> LuaCall f' [e']


emitTerm (TyApp f _) = emitAtom f
emitTerm (Cast f _) = emitAtom f

emitTerm (Extend (Lit RecNil) fs) = do
  fs' <- foldrM emitRow [] fs
  pure (LuaTable fs')
  where emitRow (f, _, e) es = (:es) . (LuaString f,) <$> emitAtom e
emitTerm (Extend tbl exs) = do
  exs' <- foldrM emitRow [] exs
  tbl' <- emitAtom tbl

  flushStmt ([ LuaLocal [old, new] [tbl', LuaTable []]
             , LuaFor [k, v] [LuaCall (LuaRef pairs) [LuaRef old]]
               [LuaAssign [LuaIndex (LuaRef new) (LuaRef (LuaName k))] [LuaRef (LuaName v)]] ] ++ exs')
  pure (LuaRef new)

  where old = LuaName (T.pack "__o")
        new = LuaName (T.pack "__n")
        k = T.pack "k"
        v = T.pack "v"
        pairs = LuaName (T.pack "pairs")

        emitRow (f, _, e) es = (:es) . LuaAssign [LuaIndex (LuaRef new) (LuaString f)] . pure <$> emitAtom e

emitTerm (Let (One (x, _, e)) body)
  | usedWhen x == Once && not (isMultiMatch e) = do
      -- If we've got a let binding which is only used once then push it onto the stack
      e' <- emitTerm e
      modifying eStack (VarEntry x e':)
      emitTerm body

  | usedWhen x == Dead && not (isMultiMatch e) = do
      e' <- emitTerm e
      flushStmt (asStmt e')
      emitTerm body

  | not (isMultiMatch e) = do
      -- If we've got a let binding which doesn't branch, then we can emit it as a normal
      -- local.
      x' <- pushScope x
      e' <- emitTerm e
      flushStmt [ LuaLocal [LuaName x'] [e'] ]
      emitTerm body
  | otherwise = do
    x' <- pushScope x
    flushStmt [ LuaLocal [LuaName x'] [] ]
    s <- use eEscape
    flushStmt (emitStmt s (LuaAssign [LuaName x'] . pure) e)
    emitTerm body

  where asStmt (LuaTable fs) = concatMap (asStmt . snd) fs
        asStmt (LuaBinOp a _ b) = asStmt a ++ asStmt b
        asStmt (LuaCall f e) = [ LuaCallS f e ]
        asStmt _ = []

emitTerm (Let (Many bs) body) = do
  -- Otherwise predeclare all variables and emit the bindings
  bs' <- traverse ((LuaName<$>) . pushScope . fst3) bs
  flushStmt [ LuaLocal bs' [] ]
  traverse_ emitLet bs
  emitTerm body
  where emitLet (v, _, e) = do
          s <- use eEscape
          flushStmt (emitStmt s (LuaAssign [LuaName (getVar v s)] . pure) e)

emitTerm (Match test [Arm { _armPtrn = p, _armBody = body, _armVars = vs}])
  | all (\(x, _) -> usedWhen x == Once || usedWhen x == Dead) vs
  = do
      test' <- emitAtom test
      -- Just push the bindings onto the stack
      modifying eStack (withMatch (patternBindings p test')++)

      emitTerm body
  | otherwise
  = do
      flushStmt []
      test' <- emitAtom test

      let (once, multi) = partition ((==Once) . usedWhen . fst) (patternBindings p test')

      -- Declare any variable used multiple times (or hoisted into a lambda)
      multi' <- traverse (firstA ((LuaName<$>) . pushScope)) multi
      flushStmt [uncurry LuaLocal (unzip multi')]

      -- Push any variable used once onto the stack
      modifying eStack (withMatch once++)

      emitTerm body

  where withMatch = map (uncurry VarEntry)

emitTerm (Match test branches) = do
  flushStmt []
  test' <- emitAtom test
  emitMatch test' branches

emitStmt :: Occurs a => EscapeScope -> Returner -> Term a -> [LuaStmt]
emitStmt s r term = evalState (runContT (emitTerm term) finish) (EmitState [] s) where
  finish x = mkLets [r x] <$> (traverse pushEntry =<< use eStack)

iife :: [LuaStmt] -> LuaExpr
iife [LuaReturn v] = v
iife b = LuaCall (LuaFunction [] b) []

mkLets :: [LuaStmt] -> [(Text, LuaExpr)] -> [LuaStmt]
mkLets = foldl (\stmts (v, b) -> LuaLocal [LuaName v] [b] : stmts)

foldAnd :: [LuaExpr] -> LuaExpr
foldAnd = foldl1 k where
  k l r
    | r == LuaTrue = l
    | l == LuaTrue = r
    | r == LuaFalse || l == LuaFalse = LuaFalse
    | otherwise = LuaBinOp l "and" r

patternTest :: forall a. Occurs a => EscapeScope -> Pattern a -> LuaExpr ->  LuaExpr
patternTest _ (Capture _ _) _      = LuaTrue
patternTest _ (PatLit RecNil) _    = LuaTrue
patternTest _ (PatLit l)  vr       = LuaBinOp (emitLit l) "==" vr
patternTest s (PatExtend p rs) vr  = foldAnd (patternTest s p vr : map test rs) where
  test (var', pat) = patternTest s pat (LuaRef (LuaIndex vr (LuaString var')))
patternTest s (Constr con) vr      = foldAnd [tag s con vr]
patternTest s (Destr con p) vr     = foldAnd [tag s con vr, patternTest s p (LuaRef (LuaIndex vr (LuaNumber 2)))]

tag :: Occurs a => EscapeScope -> a -> LuaExpr -> LuaExpr
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

emitMatch :: Occurs a => LuaExpr -> [Arm a] -> ExprContext a LuaExpr
emitMatch test ps = ContT $ \next ->  uses eEscape (pure . genIf next)
  where genBinding next s Arm { _armPtrn = p, _armBody = c } =
          ( patternTest s p test
          , let (once, multi) = partition ((==Once) . usedWhen . fst) (patternBindings p test)
                (s', multi') = foldl (\(s, vs) (v, e) -> let (v', s') = pushVar v s in (s', (LuaName v', e): vs)) (s, []) multi
            in (case multi' of
                  [] -> []
                  _ -> [uncurry LuaLocal (unzip multi')])
               ++ evalState (runContT (emitTerm c) next) (EmitState (withMatch once) s') )

        genIf next s =
          case ps of
            (ifs@Arm { _armPtrn = PatLit LitTrue } :
             els@Arm { _armPtrn = PatLit LitFalse } :_) ->
              LuaIfElse [ (test, snd (genBinding next s ifs))
                        , (LuaTrue, snd (genBinding next s els)) ]

            _ -> LuaIfElse $ map (genBinding next s) ps

        withMatch = map (uncurry VarEntry)

ops :: VarMap.Map Text
ops = VarMap.fromList
  [ (vOpAdd, "+"),  (vOpAddF, "+")
  , (vOpSub, "-"),  (vOpSubF, "-")
  , (vOpMul, "*"),  (vOpMulF, "*")
  , (vOpDiv, "/"),  (vOpDivF, "/")
  , (vOpExp, "^"),  (vOpExpF, "^")
  , (vOpLt,  "<"),  (vOpLtF,  "<")
  , (vOpGt,  ">"),  (vOpGtF,  "<")
  , (vOpLe,  "<="), (vOpLeF,  "<=")
  , (vOpGe,  ">="), (vOpGeF,  ">=")

  , (vOpConcat, "..")

  , (vOpEq, "==")
  , (vOpNe, "~=")
  , (vOpOr, "or")
  , (vOpAnd, "and") ]

isBinOp :: IsVar a => a -> Bool
isBinOp v = VarMap.member (toVar v) ops

remapOp :: IsVar a => a -> Text
remapOp v | v'@(CoVar _ n _) <- toVar v = fromMaybe n (VarMap.lookup v' ops)

isMultiMatch :: Term a -> Bool
isMultiMatch (Match _ (_:_:_)) = True
isMultiMatch _ = False

escapeScope :: EscapeScope
escapeScope =
  let escaper = basicEscaper keywords
  in flip createEscape escaper
   . ((vError, "error"):)
   . map (fmap escaper)
   $ VarMap.toList ops
