{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, FlexibleContexts, TupleSections, TemplateHaskell #-}

{-|
  = The Amulet imperative backend

  The backend is one of the more confusing parts of Amulet (even if not the most
  complex). Every time I go near it I have to sit down and work out what it's
  doing, and the confusion has not decreased over time. Consequently, this
  comment hopes to lay out how the backend is designed.

  One thing to note is that while this currently targets Lua, the same
  techniques could be applied to most imperative languages. It may be worth
  defining some generic "imperative IR", which can then be converted to Lua, JS,
  etc... Anyway, a discussion for another day.

  The backend can be thought of as two separate emitters, handling the toplevel
  and terms/atoms respectively. Both receive an "escape scope", which ensures
  variables (especially operators) are valid identifiers, as well as preventing
  accidental shadowing of variable names.

  == The toplevel emitter

  The toplevel is the simpler of these as there is no fancy handling, we simply
  loop through each element, push the variables into the escape code and
  generate the appropriate code. Once all elements have been emitted, we call
  main with the appropriate number of arguments.*

  When generating lets, we delegate off to the term (or statement) emitter.

  *Less said about this the better. We need to fix our handling of how main is
   invoked.

  == The expression emitter

  In order to convert or flat ANF into a tree, we walk down the "spine" of the
  expression tree - the body of let bindings and single-variable matches. For
  each element on the spine, we pop its dependencies from the stack and push the
  result back onto the stack (when the result is only used once).

  Most of the time this occurs with no problem. However, there are situations
  where the variables are used in a different order to the order they are
  consumed in. Here we generate local variables for the entire stack and then
  continue execution with an empty one.

  Some expressions are not part of a "run," or are guaranteed to require a
  statement. These include matches with multiple branches, recursive lets, and
  record extensions. In these cases, we flush* the expression stack and emit the
  term.

  Sadly, the generating of the statement list is a little confusing. Most of the
  generation acts as a WriterT: we prepend onto a list. However, we also keep
  track of a "return generator". This generates the appropriate code to escape
  from this statement. In most cases this is just `return`, though may assign to
  a variable instead.

  Consequently, statement generation is expressed as a ContT [LuaStmt], with the
  initial continuation acting as the returner.

  *Some terms will pop expressions off the stack before flushing the remainder
   as the can be emitted inline.

-}

module Backend.Lua.Emit
  ( emitProgram
  , emitProgramWith
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
import Syntax.Var

import qualified Types.Wellformed as W


type Returner = LuaExpr -> LuaStmt

data VarEntry v =
  VarEntry { vVar  :: v
           , vExpr :: LuaExpr
           , vPure :: Bool
           }
  deriving (Show)

data EmitState v = EmitState { _eStack  :: [VarEntry v]
                             , _eEscape :: EscapeScope }
  deriving (Show)

makeLenses ''EmitState

type ExprContext v a = ContT [LuaStmt] (State (EmitState v)) a

-- | Convert a list of core top-levels into a collection of Lua
-- statements using the default scope.
emitProgram :: forall a. Occurs a => Env -> [Stmt a] -> [LuaStmt]
emitProgram ev = fst . emitProgramWith ev escapeScope

-- | Convert a list of core top-levels into a collection of Lua
-- statements using the provided scope.
emitProgramWith :: forall a. Occurs a => Env -> EscapeScope -> [Stmt a] -> ([LuaStmt], EscapeScope)
emitProgramWith ev esc = flip runState esc . emitProg where
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
             . view names
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
             ar = W.arity (ev ^. names . at ref . non undefined)
          in pure (maybeToList (go ar (LuaRef (LuaName ref'))))
       _ -> pure []

  emitConstructor :: (MonadState EscapeScope m, Occurs a) => (a, Type a) -> m LuaStmt
  emitConstructor (var, ty)
    | arity ty == 0 = do
        var' <- state (pushVar var)
        pure $ LuaLocal [LuaName var'] [LuaTable [(LuaString "__tag", LuaString var')]]
    | otherwise = do
        var' <- state (pushVar var)
        pure $ LuaLocal [LuaName var'] [LuaFunction
                                         [LuaName "x"]
                                        [LuaReturn (LuaTable [ (LuaString "__tag", LuaString var')
                                                             , (LuaNumber 1, LuaRef (LuaName "x"))])]]

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
emitLit Unit      = LuaRef (LuaName "__builtin_unit") -- evil!
emitLit RecNil    = LuaTable []

emitAtom :: Occurs a => Atom a -> ExprContext a LuaExpr
emitAtom (Lit l) = pure (emitLit l)
emitAtom (Lam (TermArgument v _) e) = do
  (v', s) <- uses eEscape (pushVar v) -- Note this doesn't modify the scope, only extends it
  pure (LuaFunction [LuaName v'] (emitStmt s LuaReturn e))
emitAtom (Lam TypeArgument{} e) = emitTerm e
emitAtom (Ref v _) = ContT $ \next -> do
  xs <- use eStack
  case break ((==toVar v) . toVar . vVar) xs of
    -- If we're pure, or we're preceded by pure computations then we're OK to emit directly.
    (before, VarEntry { vExpr = e, vPure = p }:xs') | p || all vPure before -> do
      assign eStack (before ++ xs')
      next e

    -- If we're not in the scope at all, emit the variable
    _ | all ((/= toVar v) . toVar . vVar) xs -> do
      v' <- gets (getVar v . view eEscape)
      next (LuaRef (LuaName v'))

    -- If we're in the list head, then flush everything and return
    _ -> do
      assign eStack []
      vs <- traverse pushEntry xs
      v' <- gets (getVar v . view eEscape)
      flip mkLets vs <$> next (LuaRef (LuaName v'))

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
      | Just op' <- getEscaped op esc, Just opv <- VarMap.lookup (op' :: CoVar) ops
      -> LuaBinOp l opv e'

    _ -> LuaCall f' [e']


emitTerm (TyApp f _) = emitAtom f
emitTerm (Cast f _) = emitAtom f

emitTerm (Extend (Lit RecNil) fs) = do
  {-
    Record literals are nice and simple to generate, and can just be
    emitted as expressions.
  -}
  fs' <- foldrM emitRow [] fs
  pure (LuaTable fs')
  where emitRow (f, _, e) es = (:es) . (LuaString f,) <$> emitAtom e

emitTerm (Extend tbl exs) = do
  {-
    Record extensions have to be emitted as a statement, and so we flush our
    context and continue compilation.
  -}
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
  | Match _ (_:_:_) <- e = do
      {-
        This is a match with multiple branches. We declare our variable
        beforehand, flush all existing variables and emit both branches before
        continuing.
      -}
      x' <- pushScope x
      flushStmt [ LuaLocal [LuaName x'] [] ]
      s <- use eEscape
      flushStmt (emitStmt s (LuaAssign [LuaName x'] . pure) e)
      emitTerm body

  | usedWhen x == Once = do
      {-
        A variable which is only used once can be pushed to the expression
        stack.
      -}
      e' <- emitTerm e
      modifying eStack (VarEntry x e' False:)
      emitTerm body

  | usedWhen x == Dead = do
      {-
        Are we never used? Then we can safely emit the body without a care in
        the world.
      -}
      e' <- emitTerm e
      flushStmt (asStmt e')
      emitTerm body

  | otherwise = do
      {-
        Otheriwse we've got a let binding which doesn't branch, then we can emit
        it as a normal local.
      -}
      x' <- pushScope x
      e' <- emitTerm e
      flushStmt [ LuaLocal [LuaName x'] [e'] ]
      emitTerm body

  where asStmt (LuaTable fs) = concatMap (asStmt . snd) fs
        asStmt (LuaBinOp a _ b) = asStmt a ++ asStmt b
        asStmt (LuaCall f e) = [ LuaCallS f e ]
        asStmt _ = []

emitTerm (Let (Many bs) body) = do
  {-
    Lets which declare mutually recursive variables are relatively trivial to
    deal with: just emit the definitions, declare them and continue.
  -}
  bs' <- traverse ((LuaName<$>) . pushScope . fst3) bs
  flushStmt [ LuaLocal bs' [] ]
  traverse_ emitLet bs
  emitTerm body
  where emitLet (v, _, e) = do
          s <- use eEscape
          flushStmt (emitStmt s (LuaAssign [LuaName (getVar v s)] . pure) e)

emitTerm (Match test branches)
  | [Arm { _armPtrn = p, _armBody = body, _armVars = vs}] <- branches
  , [(x, _)] <- filter ((/=Dead) . usedWhen . fst) vs
  , usedWhen x == Once
  = do
      test' <- emitAtom test
      -- Just push the bindings onto the stack
      modifying eStack (withMatch False (patternBindings p test')++)

      emitTerm body

  | [Arm { _armPtrn = p, _armBody = body }] <- branches =  do
      {-
        Matches with a single arm do not require a branch, so we simply flush
        the stack and declare whatever variables are needed. It might be
        possible to improve this in the future: if we know the test is an
        already popped variable then we could just push them onto the stack.
        It's worth noting that there are some "obvious" cases not handled here
        (such as all variables being unused) as the optimiser should have
        nobbled them already.
      -}
      flushStmt []
      test' <- emitAtom test

      let (once, multi) = partition ((==Once) . usedWhen . fst) (patternBindings p test')

      -- Declare any variable used multiple times (or hoisted into a lambda)
      multi' <- traverse (firstA ((LuaName<$>) . pushScope)) multi
      unless (null multi') (flushStmt [uncurry LuaLocal (unzip multi')])

      -- Push any variable used once onto the stack
      modifying eStack (withMatch True once++)

      emitTerm body

  | (ifs@Arm { _armPtrn = PatLit LitTrue } :
     els@Arm { _armPtrn = PatLit LitFalse } :_) <- branches = do
      {-
        Whilst this may seem a little weird special casing this, as we'll
        generate near equivalent code in the general case, it does allow us to
        merge the test with the definition, as we know it'll only be evaluated
        once.
      -}
      test' <- emitAtom test
      flushStmt[]
      ContT $ \next -> do
        (_, ifs') <- emitArm test' next ifs
        (_, els') <- emitArm test' next els
        pure [ LuaIfElse [ (test', ifs')
                         , (LuaTrue, els') ] ]

  | (els@Arm { _armPtrn = PatLit LitFalse } :
     ifs@Arm { _armPtrn = PatLit LitTrue } :_) <- branches = do
      {-
        As above, but testing against `not EXPR`. In this case we just flip the
        two branches
       -}
      test' <- emitAtom test
      flushStmt[]
      ContT $ \next -> do
        (_, ifs') <- emitArm test' next ifs
        (_, els') <- emitArm test' next els
        pure [ LuaIfElse [ (test', ifs')
                         , (LuaTrue, els') ] ]

  | otherwise = do
      {-
        In the case of the general branch, we will probably be consuming the
        pattern multiple times, and so we need to emit it as a variable.
      -}
      flushStmt []
      test' <- emitAtom test
      ContT $ \next -> pure . LuaIfElse <$> traverse (emitArm test' next) branches

  where withMatch p = map (\(a, b) -> VarEntry a b p)

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
patternTest s (Destr con p) vr     = foldAnd [tag s con vr, patternTest s p (LuaRef (LuaIndex vr (LuaNumber 1)))]

tag :: Occurs a => EscapeScope -> a -> LuaExpr -> LuaExpr
tag scp con vr = LuaBinOp (LuaRef (LuaIndex vr (LuaString "__tag"))) "==" (LuaString (getVar con scp))

patternBindings :: Occurs a => Pattern a -> LuaExpr -> [(a, LuaExpr)]
patternBindings (PatLit _) _     = []
patternBindings (Capture n _) v
  | doesItOccur n = [(n, v)]
  | otherwise = []
patternBindings (Constr _) _     = []
patternBindings (Destr _ p) vr   = patternBindings p (LuaRef (LuaIndex vr (LuaNumber 1)))
patternBindings (PatExtend p rs) vr = patternBindings p vr ++ concatMap (index vr) rs where
  index vr (var', pat) = patternBindings pat (LuaRef (LuaIndex vr (LuaString var')))

emitArm ::  (MonadState (EmitState v1) m, Occurs v)
        => LuaExpr
        -> (LuaExpr -> State (EmitState v) [LuaStmt])
        -> AnnArm () v
        -> m (LuaExpr, [LuaStmt])
emitArm test next Arm { _armPtrn = p, _armBody = c } = do
  esc <- use eEscape
  pure ( patternTest esc p test
       , let (once, multi) = partition ((==Once) . usedWhen . fst) (patternBindings p test)
             (s', multi') = foldl (\(s, vs) (v, e) ->
                                      let (v', s') = pushVar v s
                                      in (s', (LuaName v', e): vs))
                            (esc, []) multi
         in (case multi' of
                [] -> []
                _ -> [uncurry LuaLocal (unzip multi')])
            ++ evalState (runContT (emitTerm c) next) (EmitState (withMatch once) s') )

  where withMatch = map (\(a, b) -> VarEntry a b True)

-- | A mapping from Amulet binary operators to their Lua equivalent.
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

-- | Remap an Amulet binary op to the equivalent Lua operator
remapOp :: IsVar a => a -> Text
remapOp v | v'@(CoVar _ n _) <- toVar v = fromMaybe n (VarMap.lookup v' ops)

-- | The default 'EscapeScope' for the backend
escapeScope :: EscapeScope
escapeScope =
  let escaper = basicEscaper keywords
  in flip createEscape escaper
   . ((vError, "error"):)
   . ((vLAZY, "__builtin_Lazy"):)
   . ((vForce, "__builtin_force"):)
   . map (fmap escaper)
   $ VarMap.toList ops
