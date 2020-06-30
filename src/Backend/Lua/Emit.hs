{-# LANGUAGE
  OverloadedStrings, NamedFieldPuns, FlexibleContexts
, FlexibleInstances, TupleSections, ViewPatterns
, ScopedTypeVariables, TemplateHaskell, QuasiQuotes
, TypeFamilies #-}
module Backend.Lua.Emit
  ( emitStmt
  , TopEmitState(..), topVars, topArity, topEscape, topExVars
  , LuaSimple, unsimple
  , defaultEmitState
  ) where

import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Lens

import qualified Data.Sequence as Seq
import qualified Data.VarMap as VarMap
import qualified Data.VarSet as VarSet
import qualified Data.Map as Map
import Data.Sequence (Seq((:<|)))
import qualified Data.Text as T
import Data.Traversable
import Data.Position
import Data.Foldable
import Data.Triple
import Data.List (partition)

import qualified Core.Builtin as B
import Core.Occurrence
import Core.Intrinsic
import Core.Arity
import Core.Types
import Core.Core
import Core.Var

import Language.Lua.Parser
import Language.Lua.Syntax
import Language.Lua.Quote

import Backend.EmitGraph
import Backend.Lua.Builtin
import Backend.Lua.Inline
import Backend.Escape

data VarDecl
     -- ^ A foreign function whose body should be inlined when used.
  = VarInline Int ([LuaExpr] -> (Seq LuaStmt, [LuaExpr]))
    -- | A "normal" toplevel declaration, which should just be referenced
    -- by variable.
  | VarUpvalue

instance Show VarDecl where
  show (VarInline n f) =
    let e = f $ map (LuaRef . LuaName . T.pack . ("arg"++) . show) [1..n]
    in "VarInline(" ++ show e  ++ ")"
  show VarUpvalue = "VarUpvalue"

data Lua

-- | A wrapper for "simple" Lua expressions
--
-- Purely exists so we don't accidentally start putting random things
-- into 'EmittedStmt's.
--
-- One should use 'simpleOf' instead of using the 'LuaSimp' constructor,
-- for that additional safety.
newtype LuaSimple = LuaSimp { unsimple :: LuaExpr }
  deriving Show

-- | Controls how one of more variables should be returned from a
-- statement
data EmitYield
  = YieldReturn -- ^ Return the expression(s) using 'LuaReturn'
  | YieldDiscard -- ^ Discard this expression
  | YieldStore [LuaVar] -- ^ Assign this expression(s) to these variables
  | YieldDeclare CoVar Type Occurrence -- ^ Declare this variable
  deriving Show

-- | The top-level emitting state. This is substantially simpler than the
-- more general 'EmitState' as it need not maintain an expression graph.
data TopEmitState = TopEmitState
  { _topVars   :: VarMap.Map (VarDecl, [LuaSimple])
  , _topArity  :: ArityScope
  , _topEscape :: EscapeScope
  , _topExVars :: ExtraVars
  }
  deriving Show

makeLenses ''TopEmitState

instance Emitter Lua where
  type EUpvalue Lua = VarDecl
  type EAtom    Lua = LuaSimple
  type EExpr    Lua = LuaExpr
  type EStmt    Lua = Seq LuaStmt
  type EYield   Lua = EmitYield

  upvalue = VarUpvalue
  atomToExpr = unsimple
  mkExprRef = LuaRef . LuaName
  mkAtomRef = LuaSimp . LuaRef . LuaName

  -- | Convert a literal into a Lua expression
  mkAtomLit = LuaSimp . mk where
    mk (Int x)   = LuaInteger (fromIntegral x)
    mk (Float x) = LuaNumber x
    mk (Str str) = LuaString str
    mk LitTrue   = LuaTrue
    mk LitFalse  = LuaFalse
    mk Unit      = LuaNil
    mk RecNil    = LuaTable []

  -- | Emit a declaration for a variable and a collection of expressions
  --
  -- This returns a 'LuaLocal' statement binding such expressions and the
  -- variables which were bound. We do not emit bindings for expressions
  -- which are just variables, returning variable directly.
  genDeclare escape v ty es
    -- If all expressions are simple, then just return them
    | Just vals <- traverse simpleOf es = pure (mempty, vals)
    -- Unpack tuples into multiple assignments
    | ValuesTy ts <- ty = do
        v' <- escape v
        let (rs, lhs, rhs) = tupleVars v' 1 ts es
        pure (pure (LuaLocal lhs rhs), rs)
    -- Generate @local function@ if needed, as it's a wee bit "nicer".
    | [LuaFunction a b] <- es = do
        v' <- escape v
        let var = LuaName v'
        pure (pure (LuaLocalFun var a b), [simpleVar var])
    -- Just do a normal binding
    | otherwise = do
        v' <- escape v
        let var = LuaName v'
        pure (pure (LuaLocal [var] es), [simpleVar var])

    where
      -- | Generate a set of bindings for a tuple variable
      tupleVars :: T.Text -> Int -> [Type] -> [LuaExpr]
              -> ([LuaSimple], [LuaVar], [LuaExpr])
      tupleVars _ _ [] es = ([], [], es)
      tupleVars v n (_:ts) [] =
        let (rs, lhs, rhs) = tupleVars v (n + 1) ts es
            var = LuaName (v <> T.pack ('_':show n))
        in (simpleVar var : rs, var : lhs, rhs)
      tupleVars v n (_:ts) (e:es)
        | Just e' <- simpleOf e =
          let (rs, lhs, rhs) = tupleVars v (n + 1) ts es
          in (e' : rs, lhs, rhs)
        | otherwise =
          let (rs, lhs, rhs) = tupleVars v (n + 1) ts es
              var = LuaName (v <> T.pack ('_':show n))
          in (simpleVar var : rs, var : lhs, e : rhs)

  -- | Generate the appropriate code for the provided yield.
  genYield _ YieldReturn es = pure (pure (LuaReturn es), [])
  genYield _ YieldDiscard es = pure (foldMap asStmt es, [])
  genYield _ (YieldStore vs) es = pure (pure (LuaAssign vs es), simpleVars vs)
  genYield e (YieldDeclare v ty _) es = genDeclare e v ty es

  -- | Convert an expression and a yield context to a set of Lua statements.
  --
  -- Note, this assumes that the result of the expression is not directly
  -- used, and so 'YieldDeclare's will be treated like 'YieldDiscard'.
  yieldStmt YieldReturn es = pure (LuaReturn es)
  yieldStmt (YieldStore vs) es = pure (LuaAssign vs es)
  yieldStmt YieldDiscard es = foldMap asStmt es
  yieldStmt (YieldDeclare _ _ _) es = foldMap asStmt es

  yieldDeclare (YieldDeclare v ty occ) = Just (v, ty, occ)
  yieldDeclare _ = Nothing

-- | The default (initial) state for the emitter.
defaultEmitState :: TopEmitState
defaultEmitState
  = TopEmitState
      (VarMap.map (bimap (uncurry VarInline) (pure . simpleVar)) builtinBuilders)
      emptyScope builtinEscape builtinVars

-- | A wrapper for 'emitLiftedES' which converts the result to a sequence
-- of 'LuaStmt's.
emitLifted :: ( Occurs a
              , MonadReader (EmitScope a) m
              , MonadState (EmitState Lua) m )
         => EmitYield -> AnnTerm VarSet.Set a
         -> m (Seq LuaStmt)
emitLifted yield term = do
  graph <- liftedGraph (extractAnn term)
  scope <- ask
  escape <- use emitEscape
  pure . fst3 $ emitTerm scope escape graph yield term

-- | Emit an term within a child scope.
--
-- This will not modify the existing scope/state in any way, it's just a
-- convenience function to avoid having to extract values manually.
emitLiftedES :: ( Occurs a
                , MonadReader (EmitScope a) m
                , MonadState (EmitState Lua) m )
             => EmitYield -> AnnTerm VarSet.Set a
             -> m (Either [LuaExpr] (Seq LuaStmt))
emitLiftedES yield term = do
  graph <- liftedGraph (extractAnn term)
  scope <- ask
  escape <- use emitEscape
  pure $ emitTermES scope escape graph yield term

-- | A wrapper for 'emitTermES' which converts the result to a sequence
-- of 'LuaStmt's.
emitTerm :: forall a. Occurs a
         => EmitScope a -> EscapeScope -> EmittedGraph Lua
         -> EmitYield -> AnnTerm VarSet.Set a
         -> (Seq LuaStmt, [LuaSimple], EscapeScope)
emitTerm scope esc vars yield term =
  let (vals, EmitState graph prev escapes)
        = flip runReader scope
        . flip runStateT EmitState
          { _emitGraph  = vars
          , _emitPrev   = mempty
          , _emitEscape = esc
          }
        $ emitExpr (fromVar vReturn) yield term >> emitVarBinds (fromVar vReturn)

  in ( fst $ flushGraph vReturn prev graph
     , vals
     , escapes )

-- | Emit a term within the provided context.
--
-- This builds up a graph of expressions/statements, then performs a
-- topological sort on the resulting nodes.
emitTermES :: forall a. Occurs a
           => EmitScope a -> EscapeScope -> EmittedGraph Lua
           -> EmitYield -> AnnTerm VarSet.Set a
           -> Either [LuaExpr] (Seq LuaStmt)
emitTermES scope esc vars yield term =
  let (_, EmitState graph prev _)
        = flip runReader scope
        . flip runStateT EmitState
          { _emitGraph  = vars
          , _emitPrev   = mempty
          , _emitEscape = esc
          }
        $ emitExpr (fromVar vReturn) yield term

      Just retNode = VarMap.lookup vReturn graph
      (stmts, _) = flushDeps retNode prev graph

  in case (stmts, retNode) of
       (Seq.Empty, EmittedExpr { emitExprs = es }) -> Left es
       _ -> Right (stmts <> nodeStmts retNode)

-- | Emit a single expression within the current context
--
-- This walks down the expression spine (lets) until it finds
-- some leaf expression. These are compiled based on the current graph,
-- their dependencies analysed, and they are then inserted into the
-- expression/statement graph.
--
-- Most of the code for that is performed using 'withinExpr', but some
-- non-trivial terms (record extensions, matches) roll their own as we
-- generate statements instead.
emitExpr :: forall a m. ( Occurs a
            , MonadReader (EmitScope a) m
            , MonadState (EmitState Lua) m )
         => a -> EmitYield -> AnnTerm VarSet.Set a
         -> m ()

emitExpr var yield (AnnLet _ (One (v, ty, e)) r) = do
  let yield' = if usedWhen v == Dead then YieldDiscard else YieldDeclare (toVar v) ty (usedWhen v)
  emitExpr v yield' e
  local (emitArity %~ flip extendPureLets [(v, ty, e)]) $
    emitExpr var yield r

emitExpr var yield (AnnLet _ (Many vs) r) =
  local (emitArity %~ flip extendPureLets vs) $ do
    binds <- traverse (\(v, ty, _) -> (v,) <$> genVars pushScope v ty (Just r)) vs
    stmt <- traverse (\((_, _, e), (_, v')) -> emitLifted (YieldStore v') e) (zip vs binds)

    graph <- use emitGraph
    prev  <- use emitPrev
    arity <- view emitArity
    let stmts = case mconcat stmt of
                  (LuaAssign [v] [LuaFunction args bod] :<| Empty) -> pure (LuaLocalFun v args bod)
                  stmts -> LuaLocal (foldMap snd binds) [] <| stmts
        p = all (isPure arity . thd3) vs
        vss = VarSet.fromList (map (toVar . fst3) vs)

        -- We depend on the previous node (if impure), all free variables and
        -- all other nodes within the binding group.
        deps = (if p then mempty else prev)
               <> VarSet.filter (`VarMap.member` graph) (foldMap (extractAnn . thd3) vs)
               `VarSet.difference` vss

        -- Add us to every node
        vs' = VarSet.fromList (map (toVar . fst3) vs)
        graph' = foldr (\(v, v') -> VarMap.insert (toVar v) (EmittedStmt stmts (simpleVars v') vs' deps)) graph binds

    -- Update the graph and impure set if needed
    emitGraph .= graph'
    emitPrev .= if p then prev else vss

    -- Emit the body within the existing context
    emitExpr var yield r

emitExpr var yield (AnnMatch _ test [arm@Arm { _armPtrn = p, _armBody = body, _armVars = vs }])
  | [(v, _)] <- filter (doesItOccur . fst) vs
  , usedWhen v == Once
  = do
      -- Push our pattern into the scope
      withinExpr v (YieldDeclare (toVar v) (getTy v) (usedWhen v)) (AnnAtom (freeInAtom test) test) $ do
        test' <- emitAtom test
        pure . snd . head $ patternBindings p test'

      -- Emit the body within the existing context
      emitExpr var yield body

  | otherwise = do
    test' <- emitAtomMany test
    -- Push those patterns into the graph
    graph' <- patternGraph test test' arm =<< use emitGraph
    emitGraph .= graph'

    -- Emit the body within the existing context
    emitExpr var yield body

  where getTy v = maybe (error "Cannot find pattern variable") snd (find ((==v) . fst) vs)

emitExpr var yield t@(AnnMatch _ test arms) = do
  (yield', stmt, bound) <- case yield of
    YieldReturn -> pure (YieldReturn, mempty, mempty)
    YieldDiscard -> pure (YieldDiscard, mempty, mempty)
    YieldStore vs -> pure (YieldStore vs, mempty, vs)
    YieldDeclare v ty _ -> do
      v' <- genVars pushScope (fromVar v) ty (Just t)
      pure (YieldStore v', pure (LuaLocal v' []), v')

  (deps, binds, body) <- case arms of
    {-
      Whilst this may seem a little weird special casing this, as we'll
      generate near equivalent code in the general case, it does allow us to
      merge the test with the definition, as we know it'll only be evaluated
      once.
    -}
    (Arm { _armPtrn = PatLit LitTrue, _armBody = ifs }:arms')
      | Just Arm { _armBody = els } <- find ((/=PatLit LitTrue) . _armPtrn) arms'
      -> genBoolIf yield' ifs els
    (Arm { _armPtrn = PatLit LitFalse, _armBody = els }:arms')
      | Just Arm { _armBody = ifs } <- find ((/=PatLit LitFalse) . _armPtrn) arms'
      -> genBoolIf yield' ifs els

    {-
      If we're an if block with two cases, then try to generate them inline. We
      have the weird guard, as we want to handle matches of two cases, but also
      the edge-case where we've patterns after a wildcard (such as if the
      optimiser hasn't run).
    -}
    (ifs : els@Arm { _armPtrn = elsPat } : rest) | (elsPat == PatWildcard || null rest)
      -> do
        test' <- emitAtomMany test
        (pat, ifs') <- genBranchES test' yield' ifs
        (_, els') <- genBranchES test' yield' els
        pure . (freeInAtom test,mempty,) . Right $ genIf yield' pat ifs' els'

    _ -> do
      {-
        In the case of the general branch, we will probably be consuming the
        pattern multiple times, and so we need to emit it as a variable.
      -}
      test' <- emitAtomMany test
      (freeInAtom test,mempty,) . Right . pure . LuaIfElse <$> for arms (genBranch test' yield')

  let node = case body of
        Left es -> EmittedExpr [es] yield
        Right ss -> EmittedStmt (stmt <> ss) (simpleVars bound)

  -- Add this node to the graph, depending on the arm body, and the previous impure node.
  prev <- use emitPrev
  pushGraph (node (VarSet.insert (toVar var) binds)
                  (deps <> foldMap (extractAnn . _armBody) arms <> prev))
    -- For the time being, we just assume this expression is impure.
  emitPrev .= one var

  where
    genBranchES test' yield' arm@Arm { _armPtrn = p, _armBody = b } = do
      graph <- patternGraph test test' arm =<< liftedGraph (extractAnn b)
      scope <- ask
      escape <- use emitEscape

      let b' = emitTermES scope escape graph yield' b
      pure (patternTest escape p test', b')

    genBranch test' yield' arm@Arm { _armPtrn = p, _armBody = b } = do
      graph <- patternGraph test test' arm =<< liftedGraph (extractAnn b)
      scope <- ask
      escape <- use emitEscape

      let b' = fst3 $ emitTerm scope escape graph yield' b
      pure (patternTest escape p test', toList b')

    -- | Generate a boolean if statement, trying to reduce to an expression if
    -- possible.
    genBoolIf :: EmitYield
              -> AnnTerm VarSet.Set a -> AnnTerm VarSet.Set a
              -> m (VarSet.Set, VarSet.Set, Either LuaExpr (Seq LuaStmt))
    genBoolIf yield ifs els = do
      (deps, binds, test') <- runNES (freeInAtom test) (emitAtomS test)

      ifs' <- emitLiftedES yield ifs
      els' <- emitLiftedES yield els
      pure . (deps,binds,) $ case (ifs', els') of
        -- Handle binary ops. Note we don't need to ensure e is not an
        -- unboxed tuple, as we know this returns a boolean.
        (Left [LuaTrue], Left [e]) -> Left $ LuaBinOp test' "or" e
        (Left [e], Left [LuaFalse]) -> Left $ LuaBinOp test' "and" e
        (Left [LuaFalse], Left [LuaTrue]) -> Left $ negate test'
        _ -> Right $ genIf yield test' ifs' els'

    -- | Generate a shorter if statement where possible.
    genIf :: EmitYield
          -> LuaExpr -> Either [LuaExpr] (Seq LuaStmt) -> Either [LuaExpr] (Seq LuaStmt)
          -> Seq LuaStmt
    genIf yield test ifs@Left{} els | YieldReturn <- yield
      = Seq.fromList
      $ LuaIfElse [(test, eitherStmts yield ifs)] : eitherStmts yield els
    genIf yield test ifs els@Left{} | YieldReturn <- yield
      = Seq.fromList
      $ LuaIfElse [(negate test, eitherStmts yield els)] : eitherStmts yield ifs

    genIf yield test ifs els =
      pure $ case (eitherStmts yield ifs, eitherStmts yield els) of
               (ifs, Empty) -> LuaIfElse [(test, ifs)]
               (Empty, els) -> LuaIfElse [(negate test, els)]
               (ifs, els) -> LuaIf test ifs els

    negate :: LuaExpr -> LuaExpr
    negate (LuaUnOp "not" x) = x
    negate (LuaBinOp a "==" b) = LuaBinOp a "~=" b
    negate (LuaBinOp a "~=" b) = LuaBinOp a "==" b
    negate (LuaBinOp a "and" b) = LuaBinOp (negate a) "or" (negate b)
    negate (LuaBinOp a "or" b) = LuaBinOp (negate a) "and" (negate b)
    negate x = LuaUnOp "not" x

    eitherStmts _ (Right ss) = toList ss
    eitherStmts yield (Left es) = toList $ yieldStmt yield es

-- Trivial terms. These will just be emitted inline.
emitExpr var yield t@(AnnAtom _ x)    = withinExpr var yield t $ emitAtom x
emitExpr var yield t@(AnnTyApp _ x _) = withinExpr var yield t $ emitAtom x
emitExpr var yield t@(AnnCast _ x _ _)  = withinExpr var yield t $ emitAtom x

emitExpr var yield t@(AnnApp _ f e) = withinTerm var t $ do
  e' <- emitAtom e
  f' <- emitAtomS f

  state <- use nodeState
  case f' of
    -- Attempt to inline native definitions and binary operators
    LuaRef (LuaName name)
      | Just name' <- getEscaped name (state ^. emitEscape)
      , Just EmittedUpvalue { emitTop = VarInline a b } <- state ^. (emitGraph . at name')
      , length e' == a -> case b e' of
          (Seq.Empty, es) -> pure $ EmittedExpr es yield
          (ss, es) -> do
            (ss', vals) <- genYield pushScope' yield es
            pure $ EmittedStmt (ss <> ss') vals
    _ -> pure $ EmittedExpr [LuaCallE (LuaCall f' e')] yield

emitExpr var yield (AnnLam _ TypeArgument{} b) = emitExpr var yield b

emitExpr var yield t@(AnnLam fv (TermArgument v ty) e) = do
  scope <- ask
  graph <- liftedGraph fv
  escape <- use emitEscape

  let (vs, escape') = flip runState escape $ genVars (state . pushVar) v ty (Just e)
      graph' = VarMap.insert (toVar v) (EmittedUpvalue VarUpvalue (simpleVars vs) (one v)) graph

      term :: [LuaExpr] = pure . LuaFunction vs . toList . fst3 $ emitTerm scope escape' graph' YieldReturn e
  withinExpr var yield t (pure term)

emitExpr var yield t@(AnnValues _ xs) =
  {-
    There really isn't much we can do as far as unboxed tuples go. We
    just build up a list of all dependent expressions and statement.
  -}
  withinExpr var yield t $
    foldrM (\e es -> (:es) <$> emitAtomS e) mempty xs

emitExpr var yield t@(AnnExtend _ (Lit RecNil) fs) = do
  {-
    Record literals are nice and simple to generate, and can just be
    emitted as expressions.
  -}
  graph <- use emitGraph
  withinExpr var yield t $
    pure . LuaTable <$> foldrM emitRow mempty (sortAtoms graph thd3 fs)
  where emitRow (f, _, e) es = (:es) . (LuaString f,) <$> emitAtomS e

emitExpr var yield (AnnExtend fv tbl fs) = do
  {-
    Record extensions have to be emitted as a statement. We build up a
    statement list first, and then.
  -}

  graph <- use emitGraph
  let fs' = sortAtoms graph thd3 fs
  (deps, binds, node) <- runNES fv $ case yield of
    YieldDiscard -> do
      exs' <- traverse (emitAtom . thd3) fs'
      tbl' <- emitAtom tbl
      pure $ EmittedStmt (foldMap (foldMap asStmt) (tbl':exs')) []

    YieldStore vs -> do
      let vs' = head vs
      exs' <- foldrM (emitRow vs') mempty fs'
      tbl' <- copy =<< emitAtomS tbl
      pure $ EmittedStmt (LuaAssign vs [tbl'] <| exs') (simpleVars vs)

    YieldReturn -> do
      let vs' = LuaName (T.pack "__n")
      exs' <- foldrM (emitRow vs') mempty fs'
      tbl' <- copy =<< emitAtomS tbl
      pure $ EmittedStmt ((LuaLocal [vs'] [tbl'] <| exs') |> LuaReturn [LuaRef vs']) []

    YieldDeclare var' _ _ -> do
      vs' <- LuaName <$> pushScope' var'
      exs' <- foldrM (emitRow vs') mempty fs'
      tbl' <- copy =<< emitAtomS tbl
      pure $ EmittedStmt (LuaLocal [vs'] [tbl'] <| exs') [simpleVar vs']

  pushGraph (node (VarSet.insert (toVar var) binds) deps)

  where
    emitRow var (f, _, e) es = (<|es) . LuaAssign [LuaIndex (LuaRef var) (LuaString f)] <$> emitAtom e

    copy expr = do
      clone <- uses (nodeState . emitEscape) (getVar B.backendClone)
      pure (LuaCallE (LuaCall (LuaRef (LuaName clone)) [expr]))

-- | Generate the variables needed for this binding
--
-- This is effectively a simplified version of 'genDeclare' when you do
-- not know the RHS.
genVars :: (IsVar a, Monad m)
        => (a -> m T.Text) -- ^ The fresh variable generator function
        -> a -> Type -> Maybe (AnnTerm b a)
        -- ^ The variable, its type and an optional body to extract the names from.
        -> m [LuaVar]
genVars es v ValuesTy{}
  (Just (AnnMatch _ (Ref v' _) (Arm { _armPtrn = PatValues cs }:_)))
  | toVar v == toVar v' = traverse extract cs where
      extract (Capture n _) = LuaName <$> es n
genVars es v (ValuesTy vs) _ = do
  v' <- es v
  let go _ [] = []
      go n (_:ts) = LuaName (v' <> T.pack ('_':show n)) : go (n + 1) ts
  pure (go (1 :: Int) vs)
genVars es v _ _ = pure . LuaName <$> es v

emitStmt :: forall a m. (Occurs a, MonadState TopEmitState m)
         => [AnnStmt VarSet.Set a] -> m (Seq LuaStmt)
emitStmt [] = pure mempty

emitStmt (Foreign n t f@(Intrinsic i):xs) = do
  n' <- pushTopScope n
  topArity %= extendForeign (n, t) f

  -- We've got an expression which can be inlined, push a 'VarInline'!
  let op = opOfIntrinsic i
      inline [x, y] = (mempty, [LuaBinOp x op y])
      inline _ = error "Expected 2 arguments"
      body = LuaBinOp [lua| x|] op [lua| y|]
      ex = [lua|function(x, y) return %body end|]

  topVars %= VarMap.insert (toVar n)
    ( VarInline 2 inline
    , simpleVars [LuaName n'] )
  topExVars %= VarMap.insert (toVar n) ([], pure $ LuaLocal [LuaName n'] [ex])

  emitStmt xs

emitStmt (Foreign n t f@(ForeignCode s):xs) = do
  n' <- pushTopScope n
  topArity %= extendForeign (n, t) f

  let ex = case parseExpr (SourcePos "_" 0 0) (s ^. lazy) of
        Right x -> x
        Left _ -> LuaBitE s

      normalDef :: m (Seq LuaStmt)
      normalDef = do
        topVars %= VarMap.insert (toVar n) (VarUpvalue, simpleVars [LuaName n'])
        pure . pure $ LuaLocal [LuaName n'] [ex]

      basicArity (ForallTy Irrelevant (ValuesTy ts) _) = length ts
      basicArity (ForallTy Irrelevant _ _) = 1
      basicArity (ForallTy Relevant{} _ r) = basicArity r
      basicArity _ = -1

  def <- case ex of
    LuaRef{} -> normalDef

    LuaFunction as bod
      | length as == basicArity t
      , shouldInline as bod -> do
          -- We've got an expression which can be inlined, push a 'VarInline'!
          let inline = case last bod of
                LuaReturn rs -> \xs ->
                  let s' = Map.fromList $ zip as xs
                  in (substStmt s' <$> Seq.fromList (init bod), map (substExpr s') rs)
                _ -> \xs ->
                  let s' = Map.fromList $ zip as xs
                  in (substStmt s' <$> Seq.fromList bod, [])

          topVars %= VarMap.insert (toVar n)
            ( VarInline (length as) inline
            , simpleVars [LuaName n'] )
          -- This one may never be visited normally, so we'll push the
          -- extra-variable version of it instead.
          topExVars %= VarMap.insert (toVar n) ([], pure $ LuaLocal [LuaName n'] [ex])
          pure mempty

    _ | Just ex' <- simpleOf ex -> do
          -- We're a simple expression, so we're guaranteed to never see this!
          topVars %= VarMap.insert (toVar n) (VarUpvalue, [ex'])
          pure mempty

    _ -> normalDef
  (def<>) <$> emitStmt xs

emitStmt (Type _ cs:xs) = do
  traverse_ emitConstructor cs
  topArity %= flip extendPureCtors cs
  emitStmt xs

  where
    emitConstructor (var, ty) = do
      var' <- pushTopScope var
      let tag = LuaString var'
          name = LuaName var'
          ctor = makeConstructor tag (arity ty)
          conbind =
            case ctor of
              LuaFunction v ss -> LuaLocalFun name v ss
              _ -> [luaStmt| local $name = %ctor |]
      topVars %= VarMap.insert (toVar var) (VarUpvalue, [simpleVar (LuaName var')])
      topExVars %= VarMap.insert (toVar var) ([], Seq.singleton conbind)

emitStmt (StmtLet (One (v, ty, e)):xs) = do
  TopEmitState { _topArity = ari, _topEscape = esc, _topVars = vars } <- get
  let yield = if usedWhen v == Dead then YieldDiscard else YieldDeclare (toVar v) ty (usedWhen v)
      (stmts, binds, esc') =
        emitTerm (EmitScope ari) esc
                 (VarMap.mapWithKey (\v (d, b) -> EmittedUpvalue d b (VarSet.singleton v)) vars)
                 yield e

  topArity  %= flip extendPureLets [(v, ty, e)]
  topVars   %= VarMap.insert (toVar v) (VarUpvalue, binds)
  topEscape .= esc'
  (stmts<>) <$> emitStmt xs

emitStmt (StmtLet (Many vs):xs) = do
  binds <- traverse (\(v, ty, _) -> genVars pushTopScope v ty Nothing) vs
  topArity %= flip extendPureLets vs
  topVars  %= \x -> foldr (\(v, b) -> VarMap.insert (toVar . fst3 $ v) (VarUpvalue, simpleVars b)) x (zip vs binds)

  TopEmitState { _topArity = ari, _topEscape = esc, _topVars = vars } <- get

  let graph :: EmittedGraph Lua = VarMap.mapWithKey (\v (d, b) -> EmittedUpvalue d b (VarSet.singleton v)) vars
      (stmt, esc') = foldl' (\(s, esc') ((_, _, e), v') ->
        let (s', _, esc'') = emitTerm (EmitScope ari) esc' graph (YieldStore v') e
        in (s <> s', esc'')) (mempty, esc) (zip vs binds)

  -- We don't strictly need to share escape scopes across expressions, but it helps us avoid shadowing
  topEscape .= esc'

  xs' <- emitStmt xs
  pure $ case stmt of
    (LuaAssign [v] [LuaFunction args bod] :<| Empty) -> LuaLocalFun v args bod <| xs'
    _ -> LuaLocal (mconcat binds) [] <| stmt <> xs'

makeConstructor :: LuaExpr -> Int -> LuaExpr
makeConstructor tag = go names 1 [] where
  go _ _ ac 0 = LuaTable ((LuaString "__tag", tag):reverse ac)
  go (x:xs) idx ac n = LuaFunction [x] [ LuaReturn [go xs (idx + 1) ((LuaInteger idx, LuaRef x):ac) (n - 1)] ]
  go _ _ _ _ = error ""
  names = map (LuaName . T.pack) ([1..] >>= flip replicateM ['a'..'z'])

-- | Push a variable into the current scope
pushTopScope :: (IsVar a, MonadState TopEmitState m) => a -> m T.Text
pushTopScope v = topEscape %%= pushVar v

-- | Convert an expression into a set of Lua statements
asStmt :: LuaExpr -> Seq LuaStmt
asStmt (LuaTable fs) = foldr ((<>) . asStmt . snd) mempty fs
asStmt (LuaBinOp a _ b) = asStmt a <> asStmt b
asStmt (LuaCallE x) = pure (LuaCallS x)
asStmt _ = mempty

-- | Get a simple variable from a normal one
simpleOf :: LuaExpr -> Maybe LuaSimple
simpleOf x | check x = Just (LuaSimp x) where
  -- Basic keywords are obviously simple
  check LuaNil   = True
  check LuaTrue  = True
  check LuaFalse = True
  -- We consider literals simple for the sake of argument
  check LuaInteger{} = True
  check (LuaString x) | T.length x <= 16 = True
  -- For the sake of argument, references to variables are simple
  check (LuaRef LuaName{}) = True
  check _ = False
simpleOf _ = Nothing

simpleVar :: LuaVar -> LuaSimple
simpleVar = LuaSimp . LuaRef

simpleVars :: [LuaVar] -> [LuaSimple]
simpleVars = map simpleVar

patternBindings :: Occurs a => Pattern a -> [LuaExpr] -> [(a, [LuaExpr])]
patternBindings (PatLit _) _     = []
patternBindings PatWildcard _    = []
patternBindings (Constr _) _     = []
patternBindings (Destr _ ps) [vr] = do
  (p, i) <- zip ps [1..]
  captureBinding p [LuaRef (LuaIndex vr (LuaInteger i))]
patternBindings (PatRecord rs) [vr] = concatMap (index vr) rs where
  index vr (var', pat) = captureBinding pat [LuaRef (LuaIndex vr (LuaString var'))]
patternBindings (PatValues ps) vr = mconcat (zipWith (\p v -> captureBinding p [v]) ps vr)
patternBindings _ _ = error "Mismatch between pattern and expression arity"

captureBinding :: Occurs a => Capture a -> [LuaExpr] -> [(a, [LuaExpr])]
captureBinding (Capture n _) v
  | doesItOccur n = [(n, v)]
  | otherwise = []

-- | Generate a new graph for the provided set of patterns
patternGraph :: ( Occurs a
                , MonadReader (EmitScope a) m
                , MonadState (EmitState Lua) m )
             => Atom -> [LuaExpr] -> AnnArm VarSet.Set a
             -> EmittedGraph Lua
             -> m (EmittedGraph Lua)
patternGraph test test' Arm { _armPtrn = p, _armVars = vs } graph = do
  let deps = freeInAtom test
      (once, many) = partition ((==Once) . usedWhen . fst) (patternBindings p test')

  graph' <- case many of
    [] -> pure graph
    [(v, expr)] -> do
      (body, vars) <- genDeclare pushScope v (getTy v) expr
      pure (VarMap.insert (toVar v) (EmittedStmt body vars (one v) deps) graph)

    many@((vFst, _):ps) -> do
      -- If we've got multiple patterns which will be bound, we can merge them into one binding, with all other nodes
      -- just pointing to the first one.
      stmts <- traverse (\(v, expr) -> genDeclare pushScope v (getTy v) expr) many
      let (_, vsFst) = head stmts

      pure $ foldr
        (\((v, _), (_, vs)) -> VarMap.insert (toVar v) (EmittedStmt mempty vs (one v) (one vFst)))
        (VarMap.insert (toVar vFst) (EmittedStmt (foldr (mergeLocs . fst) mempty stmts) vsFst (one vFst) deps) graph)
        (zip ps (tail stmts))

  -- Boring expressions
  pure $ foldr
    (\(v, expr) -> VarMap.insert (toVar v) (EmittedExpr expr (YieldDeclare (toVar v) (getTy v) (usedWhen v)) (one v) deps))
    graph' once

  where
    getTy v = maybe (error "Cannot find pattern variable") snd (find ((==v) . fst) vs)

    -- | Merge two adjacent local definitions
    mergeLocs (LuaLocal vs es :<| Empty) (LuaLocal vs' es' :<| rest)
      | length vs == length es = LuaLocal (vs ++ vs') (es ++ es') <| rest
    mergeLocs l r = l <> r

patternTest :: forall a. IsVar a => EscapeScope -> Pattern a -> [LuaExpr] ->  LuaExpr
patternTest _ PatWildcard _      = LuaTrue
patternTest _ (PatLit RecNil) _  = LuaTrue
patternTest _ PatRecord{} _      = LuaTrue
patternTest _ PatValues{} _      = LuaTrue
patternTest _ (PatLit l)  [vr]   = LuaBinOp vr "==" (emitLit l)
patternTest s (Constr con) [vr]  = tag s con vr
patternTest s (Destr con _) [vr] = tag s con vr
patternTest _ _ _ = undefined

tag :: IsVar a => EscapeScope -> a -> LuaExpr -> LuaExpr
tag scp con vr = LuaBinOp (LuaRef (LuaIndex vr (LuaString "__tag"))) "==" (LuaString (getVar con scp))

one :: IsVar a => a -> VarSet.Set
one = VarSet.singleton . toVar

opOfIntrinsic :: Intrinsic -> T.Text
opOfIntrinsic IntAdd = "+"
opOfIntrinsic IntSub = "-"
opOfIntrinsic IntMul = "*"
opOfIntrinsic IntDiv = "/"
opOfIntrinsic IntPow = "^"
opOfIntrinsic IntEq  = "=="
opOfIntrinsic IntLt  = "<"
opOfIntrinsic IntLe  = "<="

opOfIntrinsic FloatAdd = "+"
opOfIntrinsic FloatSub = "-"
opOfIntrinsic FloatMul = "*"
opOfIntrinsic FloatDiv = "/"
opOfIntrinsic FloatPow = "^"
opOfIntrinsic FloatEq  = "=="
opOfIntrinsic FloatLt  = "<"
opOfIntrinsic FloatLe  = "<="

opOfIntrinsic StrConcat = ".."
opOfIntrinsic StrEq = "=="
opOfIntrinsic StrLt = "<"
opOfIntrinsic StrLe = "<="

opOfIntrinsic BoolEq = "=="
